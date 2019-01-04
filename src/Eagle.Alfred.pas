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
  Spring.Reflection,

  Eagle.ConsoleIO,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Data,
  Eagle.Alfred.Exceptions,
  Eagle.Alfred.Core.Command;

type

  TCommandMetaData = record
    CommandClass: TClass;
    CommandType: TRttiType;
  end;

  TAlfred = class
  private
    class var FInstance: TAlfred;
  private
    FCurrentPath: string;
    FAppPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;
    FCmdParameters: TList<string>;
    FCommands: TDictionary<string, TDictionary<string, TCommandMetaData>>;
    function CreateCommand(CommandMetaData: TCommandMetaData): ICommand;
    procedure Execute(const GroupName, CommandName: string);
    function FindFlagInParameters(const FlagName: string): Boolean;
    procedure Help;
    procedure Init;
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
var
  Group: TDictionary<string, TClass>;
begin

  FCurrentPath := GetCurrentDir;
  FAppPath := ExtractFilePath(ParamStr(0));
  FConsoleIO := TConsoleIO.Create;

  FCmdParameters := TList<string>.Create;

  FCommands := TDictionary<string, TDictionary<string, TCommandMetaData>>.Create;

  Init;

end;

destructor TAlfred.Destroy;
begin

  if Assigned(FCmdParameters) then
    FreeAndNil(FCmdParameters);

  if Assigned(FCommands) then
  begin
     FreeAndNil(FCommands);
  end;

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

procedure TAlfred.Execute(const GroupName, CommandName: string);
var
  CommandGroup: TDictionary<string, TCommandMetaData>;
  CommandMetaData: TCommandMetaData;
  Command: ICommand;
begin

  CommandGroup := FCommands.Items[GroupName];

  if not CommandGroup.ContainsKey(CommandName) then
  begin
    Help;
    Exit;
  end;

  CommandMetaData := CommandGroup.Items[CommandName];

  Command := CreateCommand(CommandMetaData);

  Command.Execute;

end;

function TAlfred.FindFlagInParameters(const FlagName: string): Boolean;
var
  ParamName: string;
begin

  ParamName := TRegEx.Replace(FlagName, '([a-z])([A-Z])', '$1-$2').ToLower;

  Result := FCmdParameters.Contains(ParamName);

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

  if not FileExists('.\package.json') then
    Exit;

  Data := TFile.ReadAllText('.\package.json');

  FPackage := TJSON.Parse<TPackage>(Data);

end;

procedure TAlfred.Register(Cmd: TClass);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  RttiMethods: TArray<TRttiMethod>;
  CmdAttrib: CommandAttribute;
  CommandsGroup: TDictionary<string, TCommandMetaData>;
  CommandMetaData: TCommandMetaData;
begin

  RttiContext := TRttiContext.Create;

  try

    RttiType := RttiContext.GetType(Cmd.ClassInfo);

    if not RttiType.TryGetCustomAttribute<CommandAttribute>(CmdAttrib) then
      raise Exception.Create('Error command register');


    if not FCommands.ContainsKey(CmdAttrib.GroupName) then
      FCommands.Add(CmdAttrib.GroupName, TDictionary<string, TCommandMetaData>.Create());

    CommandsGroup := FCommands.Items[CmdAttrib.GroupName];

    CommandMetaData.CommandClass := Cmd;
    CommandMetaData.CommandType := RttiType;

    CommandsGroup.Add(CmdAttrib.Name, CommandMetaData);

  finally
    RttiContext.Free;
  end;

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

  if not FCommands.ContainsKey(GroupName) then
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

initialization
  TAlfred.GetInstance;

finalization
  TAlfred.ReleaseInstance;

end.
