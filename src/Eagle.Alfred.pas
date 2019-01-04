unit Eagle.Alfred;

interface
uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.IOUtils,
  System.RegularExpressions,

  XSuperJSON, XSuperObject,


  Eagle.ConsoleIO,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Data,
  Eagle.Alfred.Exceptions,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.CommandRegister;

type

  TAlfred = class
  private
    class var FInstance: TAlfred;
  private
    FCurrentPath: string;
    FAppPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;
    FCommandArgs: TList<string>;
    FCommandRegister: ICommandRegister;
    function CreateCommand(CommandMetaData: TCommandMetaData): ICommand;
    procedure DoSetParam(Command: ICommand; Method: TRttiMethod; ParamValue: string);
    procedure Execute(const GroupName, CommandName: string);
    function GetCommandParam(Attrib: ParamAttribute): string;
    procedure Help;
    procedure Init;
    procedure LoadCommandArgs;
    function OptionExists(const OptionAttrib: OptionAttribute): Boolean;
    procedure SetOptions(Command: ICommand; CommandMetaData: TCommandMetaData);
    procedure SetParams(Command: ICommand; CommandMetaData: TCommandMetaData);
  public
    class function GetInstance() : TAlfred;
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

  FCommandArgs := TList<string>.Create;

  FCommandRegister := TCommandRegister.Create;

  Init;

end;

destructor TAlfred.Destroy;
begin

  if Assigned(FCommandArgs) then
    FreeAndNil(FCommandArgs);

  if Assigned(FPackage) then
    FreeAndNil(FPackage);

  inherited;
end;

function TAlfred.CreateCommand(CommandMetaData: TCommandMetaData): ICommand;
begin

  Result := CommandMetaData.CommandType.GetMethod('Create').invoke(CommandMetaData.CommandClass, [
    TValue.From<string>(FAppPath),
    TValue.From<TPackage>(FPackage),
    TValue.From<IConsoleIO>(FConsoleIO)
  ]).AsType<ICommand>;

end;

procedure TAlfred.DoSetParam(Command: ICommand; Method: TRttiMethod; ParamValue: string);
begin
  Method.Invoke(TObject(Command), [ParamValue]);
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

function TAlfred.GetCommandParam(Attrib: ParamAttribute): string;
var
  Arg: string;
begin

  if Attrib.Index > 0 then
  begin
    Result := FCommandArgs.Items[Attrib.Index + 1];
    if Result.StartsWith('-') then
      raise Exception.Create('Error Message');

    Exit;
  end;

  for Arg in FCommandArgs.ToArray do
  begin
    if not Arg.ToLower.StartsWith(Attrib.Name+'=') then
      Continue;

    Result := Arg.Split(['='])[1];
    Exit;
  end;

end;

class function TAlfred.GetInstance: TAlfred;
begin

  if FInstance =  nil then
    Self.FInstance := TAlfred.Create;

  Result := Self.FInstance;

end;

procedure TAlfred.Help;
var
  Par: TPair<string, TClass>;
begin

  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('         Alfred - Code Generate for Delphi');
  FConsoleIO.WriteInfo('-----------------------------------------------------');

  FConsoleIO.WriteInfo('Para obter mais informações sobre um comando específico,');
  FConsoleIO.WriteInfo('digite nome_do_comando HELP');
  FConsoleIO.WriteInfo('');

 // for Par in FCommands.ToArray do
 //   FConsoleIO.WriteInfo(Par.Value.GetName.PadRight(15, ' ') + Par.Value.GetDescription);

end;

procedure TAlfred.Init;
var
  Data: string;
begin

  LoadCommandArgs;

  if not FileExists('.\package.json') then
    Exit;

  Data := TFile.ReadAllText('.\package.json');

  FPackage := TJSON.Parse<TPackage>(Data);

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

function TAlfred.OptionExists(const OptionAttrib: OptionAttribute): Boolean;
begin
  Result := FCommandArgs.Contains('-' +OptionAttrib.Alias) or FCommandArgs.Contains('--' +OptionAttrib.Name);
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

  GroupName := ParamStr(1).ToLower;
  CommandName := ParamStr(2).ToLower;

  if not FCommandRegister.Contains(GroupName, CommandName) then
  begin
    Help;
    Exit;
  end;

  try
    Execute(GroupName, CommandName);
  except on E: EAlfredException do
    FConsoleIO.WriteError(E.Message);
  end;

end;

procedure TAlfred.SetOptions(Command: ICommand; CommandMetaData: TCommandMetaData);
var
  CommandOption: TCommandOption;
begin
  for CommandOption in CommandMetaData.CommandOptions do
  begin
    if OptionExists(CommandOption.Attrib) then
      CommandOption.Method.Invoke(TObject(Command), []);
  end;
end;

procedure TAlfred.SetParams(Command: ICommand; CommandMetaData: TCommandMetaData);
var
  CommandParam: TCommandParam;
  ParamValue: string;
begin

  for CommandParam in CommandMetaData.CommandParams do
  begin
    ParamValue := GetCommandParam(CommandParam.Attrib);
    if ParamValue.IsEmpty then
      Continue;

    DoSetParam(Command, CommandParam.Method, ParamValue);
  end;

end;

initialization
  TAlfred.GetInstance;

finalization
  TAlfred.ReleaseInstance;
end.
