unit Eagle.Alfred;

interface

uses
  System.Classes,
  System.SysUtils, StrUtils,
  System.Math,
  System.Generics.Collections,
  System.Rtti,
  System.IOUtils,
  System.RegularExpressions,

  XSuperJSON, XSuperObject,

  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.ConsoleIO,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.CommandRegister,
  Eagle.Alfred.Core.CommandRegisterData,

  Eagle.Alfred.Command.Common.DprojParser,
  Eagle.Alfred.Command.Common.HelpBuilder;

var
  start, stop, elapsed: cardinal;

type

  TAlfred = class
  private
    class var FInstance: TAlfred;
  private

    FConfiguration: TConfiguration;
    FCurrentPath: string;
    FAppPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;
    FCommandArgs: TList<string>;
    FCommandRegister: ICommandRegister;
    FHelpBuilder: IHelpBuilder;

    function CreateCommand(CommandMetaData: TCommandMetaData): ICommand;
    function DoGetCommandParamByName(Attrib: ParamAttribute): string;
    function DoGetCommandParamByPosition(Attrib: ParamAttribute; const IsSingle: Boolean): string;
    procedure DoSetParam(Command: ICommand; Method: TRttiMethod; ParamValue: string);
    procedure Execute(const GroupName, CommandName: string);
    procedure ExecuteHelp(const GroupName, CommandName: string);
    function GetCommandParam(Attrib: ParamAttribute; const IsSingle: Boolean): string;
    procedure Help;
    procedure HelpCommand(const GroupName, CommandName: string);
    procedure HelpGroupCommands(const GroupName: string);
    procedure HelpProjectInit;
    procedure Init;
    function IsOptionHelp(const Option: string): Boolean;
    function IsToExecuteHelp: Boolean;
    procedure LoadCommandArgs;
    procedure LoadConfiguration;
    function OptionExists(const OptionAttrib: OptionAttribute): Boolean;
    procedure SetOptions(Command: ICommand; CommandMetaData: TCommandMetaData);
    procedure SetParams(Command: ICommand; CommandMetaData: TCommandMetaData);
    procedure ShowMessageAlert(const Alert: string);

  public

    class function GetInstance(): TAlfred;
    class procedure ReleaseInstance();
    constructor Create;
    destructor Destroy; override;
    procedure Run();
    procedure &Register(Cmd: TClass);

  end;

implementation

{ TAlfred }

constructor TAlfred.Create;
begin

  FCurrentPath := GetCurrentDir;
  FAppPath := ExtractFilePath(ParamStr(0));
  FConsoleIO := TConsoleIO.Create;
  FHelpBuilder := THelpBuilder.Create;

  FCommandArgs := TList<string>.Create;

  FCommandRegister := TCommandRegister.Create;

end;

destructor TAlfred.Destroy;
begin

  if Assigned(FConfiguration) then
    FreeAndNil(FConfiguration);

  if Assigned(FCommandArgs) then
    FreeAndNil(FCommandArgs);

  if Assigned(FPackage) then
    FreeAndNil(FPackage);

  inherited;
end;

function TAlfred.CreateCommand(CommandMetaData: TCommandMetaData): ICommand;
begin

  if CommandMetaData.PackageRequired and not Assigned(FPackage) then
    raise EPackageNotFoundException.Create('Package Not Found');

  Result := CommandMetaData.CommandType.GetMethod('Create').invoke(CommandMetaData.CommandClass, [
    TValue.From<string>(FAppPath),
    TValue.From<string>(FCurrentPath),
    TValue.From<TConfiguration>(FConfiguration),
    TValue.From<TPackage>(FPackage),
    TValue.From<IConsoleIO>(FConsoleIO)])
  .AsType<ICommand>;

end;

function TAlfred.DoGetCommandParamByName(Attrib: ParamAttribute): string;
var
  Arg: string;
  Values: TArray<string>;
begin
  Result := EmptyStr;

  for Arg in FCommandArgs.ToArray do
  begin
    if not Arg.ToLower.StartsWith(Attrib.Name + '=') then
      Continue;

    Values := Arg.Split(['=']);

    if Length(Values) = 1 then
      raise ERequiredParameterException.CreateFmt('Value Required "%s" Not Found', [Attrib.Description]);

    Result := Values[1].Replace('''', '', [rfReplaceAll]).Replace('"', '', [rfReplaceAll]).Trim;

    if Result.IsEmpty then
      raise ERequiredParameterException.CreateFmt('Value Required "%s" Not Found', [Attrib.Description]);

    Break;
  end;
end;

function TAlfred.DoGetCommandParamByPosition(Attrib: ParamAttribute; const IsSingle: Boolean): string;
var
  Shift: Integer;
begin
  Result := EmptyStr;

  Shift := IfThen(IsSingle, 0, 1);

  if (Attrib.Index > 0) and (Attrib.Index + Shift < FCommandArgs.Count) then
  begin
    Result := FCommandArgs.Items[Attrib.Index + Shift];
    if Result.StartsWith('-') or Result.Contains('=') then
      Result := EmptyStr;

  end;

end;

procedure TAlfred.DoSetParam(Command: ICommand; Method: TRttiMethod; ParamValue: string);
begin
  Method.invoke(TObject(Command), [ParamValue]);
end;

procedure TAlfred.Execute(const GroupName, CommandName: string);
var
  Command: ICommand;
  CommandMetaData: TCommandMetaData;
begin
  CommandMetaData := FCommandRegister.GetCommand(GroupName, CommandName);

  Command := CreateCommand(CommandMetaData);

  SetParams(Command, CommandMetaData);

  SetOptions(Command, CommandMetaData);

  Command.Execute;
end;

procedure TAlfred.ExecuteHelp(const GroupName, CommandName: string);
begin

  if (GroupName.IsEmpty and CommandName.IsEmpty) or (not FCommandRegister.ContainsGroupCommand(GroupName)) then
  begin
    Help;
    Exit;
  end;

  if (not FCommandRegister.ContainsCommand(GroupName, CommandName)) then
  begin
    HelpGroupCommands(GroupName);
    Exit;
  end;

  HelpCommand(GroupName, CommandName);

end;

function TAlfred.GetCommandParam(Attrib: ParamAttribute; const IsSingle: Boolean): string;
begin
  Result := DoGetCommandParamByPosition(Attrib, IsSingle);

  if Result.IsEmpty then
    Result := DoGetCommandParamByName(Attrib);

  if Attrib.Required and Result.IsEmpty then
    raise ERequiredParameterException.CreateFmt('Required Parameter "%s" Not Found!', [Attrib.Description]);
end;

class function TAlfred.GetInstance: TAlfred;
begin
  if FInstance = nil then
    Self.FInstance := TAlfred.Create;

  Result := Self.FInstance;
end;

procedure TAlfred.Help;
begin
  FHelpBuilder.ShowSimpleList(FCommandRegister.GetGroupsCommand, 'groups commands', 'group command');
end;

procedure TAlfred.HelpCommand(const GroupName, CommandName: string);
var
  Command: TCommandMetaData;
begin
  Command := FCommandRegister.GetCommand(GroupName, CommandName);
  FHelpBuilder.ShowCommand(Command);
end;

procedure TAlfred.HelpGroupCommands(const GroupName: string);
var
  Commands: TDictionary<string, TCommandMetaData>;
begin
  Commands := FCommandRegister.GetGroup(GroupName);
  FHelpBuilder.ShowCommandsOfGroup(GroupName, Commands);
end;

procedure TAlfred.HelpProjectInit;
begin
  FConsoleIO.WriteAlert(sLineBreak + '* ------- ');
  FConsoleIO.WriteAlert('| File Package.json not found! ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ' + sLineBreak);
end;

procedure TAlfred.Init;
var
  Data: string;
begin
  LoadConfiguration;

  LoadCommandArgs;

  if not FileExists('.\package.json') then
    Exit();

  Data := TFile.ReadAllText('.\package.json').Replace('null', '{}');

  try
    FPackage := TJSON.Parse<TPackage>(Data);
  except
    on E: Exception do
      raise EAlfredException.Create('Package configuration invalid! ' + E.Message);
  end;

  FPackage.Validate;
end;

function TAlfred.IsOptionHelp(const Option: string): Boolean;
begin
  Result := Option.Contains('-h') or Option.Contains('-help')
end;

function TAlfred.IsToExecuteHelp: Boolean;
const
  ALIAS_HELP = '-h';
  NAME_HELP = '--help';
begin
  Result := FCommandArgs.Contains(ALIAS_HELP) or FCommandArgs.Contains(NAME_HELP);
end;

procedure TAlfred.LoadCommandArgs;
var
  ParamName: string;
  I, Count: Integer;
begin

  Count := ParamCount;

  for I := 1 to Count do
  begin
    ParamName := ParamStr(I);
    FCommandArgs.Add(ParamName);
  end;

end;

procedure TAlfred.LoadConfiguration;
var
  Data: string;
begin

  if FileExists(FAppPath + '\alfred.conf') then
    Data := TFile.ReadAllText(FAppPath + '\alfred.conf');

  if not Data.IsEmpty then
  begin
    try
      FConfiguration := TJSON.Parse<TConfiguration>(Data);
    except
      on E: Exception do
        raise EAlfredException.Create('Global configuration invalid! ' + E.Message);
    end;

    Exit;
  end;

  FConfiguration := TConfiguration.Create;
  Data := TJSON.Stringify<TConfiguration>(FConfiguration, True);
  TFile.WriteAllText(FAppPath + '\alfred.conf', Data);
end;

function TAlfred.OptionExists(const OptionAttrib: OptionAttribute): Boolean;
begin
  Result := FCommandArgs.Contains('-' + OptionAttrib.Alias) or FCommandArgs.Contains('--' + OptionAttrib.Name);
end;

procedure TAlfred.Register(Cmd: TClass);
begin
  FCommandRegister.AddCommand(Cmd);
end;

class procedure TAlfred.ReleaseInstance;
begin
  if Self.FInstance <> nil then
    Self.FInstance.Free;
end;

procedure TAlfred.Run;
var
  GroupName, CommandName: string;
begin

  if not IsOptionHelp(ParamStr(1).ToLower) then
    GroupName := ParamStr(1).ToLower;

  if not IsOptionHelp(ParamStr(2).ToLower) then
    CommandName := ParamStr(2).ToLower;

  try
    Init;

    if IsToExecuteHelp() then
    begin
      ExecuteHelp(GroupName, CommandName);
      Exit;
    end;

    Execute(GroupName, CommandName);
  except

    on E: ECommandGroupNotFoundException do
      Help;

    on E: ECommandNotFound do
      HelpGroupCommands(GroupName);

    on E: EPackageNotFoundException do
      HelpProjectInit;

    on E: ERequiredParameterException do
    begin
      ShowMessageAlert(E.Message);
      FConsoleIO.WriteInfo(Format('alfred %s %s [-h | --help] to show helper ', [GroupName, CommandName]));
    end;

    on E: EAlfredException do
      ShowMessageAlert(E.Message);

    on E: EAbort do
      FConsoleIO.WriteError('Command aborted');
  end;

end;

procedure TAlfred.SetOptions(Command: ICommand; CommandMetaData: TCommandMetaData);
var
  CommandOption: TCommandOption;
begin
  for CommandOption in CommandMetaData.CommandOptions do
  begin
    if OptionExists(CommandOption.Attrib) then
      CommandOption.Method.invoke(TObject(Command), []);
  end;
end;

procedure TAlfred.SetParams(Command: ICommand; CommandMetaData: TCommandMetaData);
var
  CommandParam: TCommandParam;
  ParamValue: string;
begin

  for CommandParam in CommandMetaData.CommandParams do
  begin
    ParamValue := GetCommandParam(CommandParam.Attrib, CommandMetaData.CommandAttrib.IsSingle);
    if ParamValue.IsEmpty then
      Continue;

    DoSetParam(Command, CommandParam.Method, ParamValue);
  end;

end;

procedure TAlfred.ShowMessageAlert(const Alert: string);
begin
  FConsoleIO.WriteAlert(sLineBreak + '* ------- ');
  FConsoleIO.WriteAlert(Format('| %s ', [Alert]));
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ' + sLineBreak);
end;

initialization

start := TThread.GetTickCount;
TAlfred.GetInstance;

finalization

TAlfred.ReleaseInstance;

stop := TThread.GetTickCount;
elapsed := stop - start; // milliseconds
Writeln('');
Writeln('Duration: ' + String.Parse(elapsed) + ' milliseconds');

end.
