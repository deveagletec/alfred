unit Eagle.Alfred;

interface
uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  Spring.Reflection,
  Eagle.ConsoleIO,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.CommandRegister,
  Eagle.Alfred.DprojParser;

type

  TAlfred = class
  private
    class var FInstance: TAlfred;
  private
    FCurrentPath: string;
    FAppPath: string;
    FDprojParser: TDprojParser;
    FConsoleIO: IConsoleIO;
    FCommands: TDictionary<string, ICommandRegister>;
    procedure Execute(Cmd: string);
    function GetParametersAction(Action: TRttiMethod): TArray<TValue>;
    procedure Help;
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
  FDprojParser := TDprojParser.Create('.\packages\DelphiXE8\', 'EagleGestao');
  FCurrentPath := GetCurrentDir;
  FAppPath := ExtractFilePath(ParamStr(0));
  FConsoleIO := TConsoleIO.Create;

  FCommands := TDictionary<string, ICommandRegister>.Create;

end;

destructor TAlfred.Destroy;
begin

  if Assigned(FDprojParser) then
    FreeAndNil(FDprojParser);

  if Assigned(FCommands) then
    FreeAndNil(FCommands);

  inherited;
end;

procedure TAlfred.Execute(Cmd: string);
var
  Command: ICommandRegister;
  ActionName: string;
  Action: TRttiMethod;
  ParamsAction: TArray<TValue>;
begin

  ActionName := ParamStr(2).ToUpper;

  Command := FCommands.Items[Cmd];

  if not Command.ContainsAction(ActionName) then
  begin
    Help;
    Exit;
  end;

  Action := Command.GetAction(ActionName).Method;

  ParamsAction := GetParametersAction(Action);

  Action.Invoke(Command.GetCommand, ParamsAction);

end;

class function TAlfred.GetInstance: TAlfred;
begin

  if FInstance =  nil then
    Self.FInstance := TAlfred.Create;

  Result := Self.FInstance;

end;

function TAlfred.GetParametersAction(Action: TRttiMethod): TArray<TValue>;
var
  Param: TRttiParameter;
  Params: TList<TValue>;
  Index: Integer;
begin

  Index := 3;

  Params := TList<TValue>.Create;

  try

    for Param in Action.Parameters do
    begin
      Params.Add(TValue.From<string>(ParamStr(Index).ToLower));
      Inc(Index);
    end;

    Result := Params.ToArray;

  finally
    Params.Free;
  end;

end;

procedure TAlfred.Help;
var
  Par: TPair<string, ICommandRegister>;
begin

  FConsoleIO.WriteInfo('         Alfred - Code Generate for Delphi');
  FConsoleIO.WriteInfo('-----------------------------------------------------');

  FConsoleIO.WriteInfo('Para obter mais informações sobre um comando específico,');
  FConsoleIO.WriteInfo('digite nome_do_comando HELP');
  FConsoleIO.WriteInfo('');

  for Par in FCommands.ToArray do
    FConsoleIO.WriteInfo(Par.Value.GetName.PadRight(15, ' ') + Par.Value.GetDescription);

  FConsoleIO.WriteInfo('');

end;

procedure TAlfred.Register(Cmd: TClass);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  RttiMethods: TArray<TRttiMethod>;
  CmdAttrib: CommandAttribute;
  ActionAttrib: ActionAttribute;
  CmdInstance: TObject;
  CmdRegister: ICommandRegister;
begin

  RttiContext := TRttiContext.Create;

  try

    RttiType := RttiContext.GetType(Cmd.ClassInfo);

    if not RttiType.TryGetCustomAttribute<CommandAttribute>(CmdAttrib) then
      raise Exception.Create('Error command register');

    CmdInstance :=  RttiType.GetMethod('Create').invoke(Cmd, [TValue.From<string>(FAppPath), TValue.From<IConsoleIO>(FConsoleIO), TValue.From<TDprojParser>(FDprojParser)]).AsObject;

    CmdRegister := TCommandRegister.Create(CmdAttrib.Name, CmdAttrib.Description, CmdInstance);

    FCommands.Add(CmdAttrib.Name, CmdRegister);

    RttiMethods := RttiType.GetMethods;

    for Method in RttiMethods do
    begin

      if Method.TryGetCustomAttribute<ActionAttribute>(ActionAttrib) then
        CmdRegister.AddMethod(ActionAttrib.Name, ActionAttrib.Description, Method);

    end;

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
  Cmd: string;
begin

  Cmd := ParamStr(1).ToUpper;

  if not FCommands.ContainsKey(Cmd) then
  begin
    Help;
    Exit;
  end;

  Execute(Cmd);

end;

initialization
  TAlfred.GetInstance;

finalization
  TAlfred.ReleaseInstance;

end.
