unit Eagle.Alfred.Command.Init;

interface
uses
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('init', '', 'Generates package.json')]
  TInitCommand = class(TCommandAbstract)
  private
    function CreateMessage(const Msg: string; Default: string): string;
    procedure ReadProjectName;

    procedure Init; override;
    procedure ReadDBFile;
    procedure ReadDBHost;
    procedure ReadDBPass;
    procedure ReadDBPort;
    procedure ReadDBUser;
    function ReadDir(const Msg: string): string;
    procedure ReadProjectBaseDir;
    procedure ReadProjectDBConfig;
    procedure ReadProjectDescription;
    procedure ReadProjectMigrationDir;
    procedure ReadProjectModular;
    procedure ReadProjectNamespace;
    procedure ReadProjectPackageDir;
    procedure ReadProjectSourceDir;
    procedure ReadProjectTestsDir;
    procedure ReadProjectVersion;
    function ReadRequiredData(const Msg, Default, Alert: string): string;
    procedure ShowGenerationConfirmation;
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ TInitCommand }

destructor TInitCommand.Destroy;
begin
  if Assigned(FPackage) then
    FPackage.Free;
  inherited;
end;

function TInitCommand.CreateMessage(const Msg: string; Default: string): string;
begin
  Result := Msg;

  if not Default.IsEmpty then
    Result := Msg + '(' + Default + ') ';
end;

procedure TInitCommand.Execute;
begin
  inherited;
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Welcome to Alfred config generator');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('This command will guide you through creating you package.json config');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Pres ^C at any time to quit.');
  FConsoleIO.WriteInfo('');

  ReadProjectName;
  ReadProjectNamespace;
  ReadProjectDescription;
  ReadProjectVersion;
  ReadProjectModular;
  ReadProjectBaseDir;
  ReadProjectPackageDir;
  ReadProjectSourceDir;
  ReadProjectTestsDir;
  ReadProjectMigrationDir;
  ReadProjectDBConfig;

  ShowGenerationConfirmation;
end;

procedure TInitCommand.Init;
begin
  inherited;
  FPackage := TPackage.Create;
  FPackage.Version := FConfiguration.Version;
  FPackage.BaseDir := FConfiguration.BaseDir;
  FPackage.MigrationDir := FConfiguration.MigrationDir;
  FPackage.PackagesDir := FConfiguration.PackagesDir;
  FPackage.SourceDir := FConfiguration.SourceDir;
  FPackage.TestsDir := FConfiguration.TestsDir;
  FPackage.Namespace := FConfiguration.Namespace;
  FPackage.Modular := FConfiguration.Modular;
end;

procedure TInitCommand.ReadDBFile;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('file: ', FPackage.DataBase.&File);

  Value := ReadRequiredData(Msg, FPackage.DataBase.&File, 'File required!');

  if not Value.IsEmpty then
    FPackage.DataBase.&File := Value;
end;

procedure TInitCommand.ReadDBHost;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('host: ', FPackage.DataBase.Host);

  Value := ReadRequiredData(Msg, FPackage.DataBase.Host, 'Host required!');

  if not Value.IsEmpty then
    FPackage.DataBase.Host := Value;
end;

procedure TInitCommand.ReadDBPass;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('Password: ', FPackage.DataBase.Pass);

  Value := ReadRequiredData(Msg, FPackage.DataBase.Pass, 'Password required!');

  if not Value.IsEmpty then
    FPackage.DataBase.Pass := Value;
end;

procedure TInitCommand.ReadDBPort;
var
  Msg, Value: string;
  Port: Integer;
begin
  Msg := CreateMessage('Port: ', string.Parse(FPackage.DataBase.Port));

  repeat
    Value := ReadRequiredData(Msg, FPackage.DataBase.Pass, 'Port required!');

    if Value.IsEmpty or Integer.TryParse(Value, Port) then
      Break;

    FConsoleIO.WriteError('Port invalid!');
  until (False);

  if not Value.IsEmpty then
    FPackage.DataBase.Port := Integer.Parse(Value);
end;

procedure TInitCommand.ReadDBUser;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('user: ', FPackage.DataBase.User);

  Value := ReadRequiredData(Msg, FPackage.DataBase.Pass, 'User required!');

  if not Value.IsEmpty then
    FPackage.DataBase.User := Value;
end;

function TInitCommand.ReadDir(const Msg: string): string;
begin
  repeat
    Result := FConsoleIO.ReadData(Msg).Trim;

    if Result.IsEmpty or TFile.Exists(Result) then
      Break;

    FConsoleIO.WriteError('Diretory ' + Result.QuotedString + ' does not exists');
  until (False);
end;

procedure TInitCommand.ReadProjectBaseDir;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('base-dir: ', FPackage.BaseDir);

  Value := ReadDir(Msg);

  if not Value.IsEmpty then
    FPackage.BaseDir := Value;
end;

procedure TInitCommand.ReadProjectDBConfig;
var
  Answer: Boolean;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Define DB configuration');
  FConsoleIO.WriteInfo('');

  Answer := FConsoleIO.ReadBoolean('Would you like to define your DB config (yes)? ', True);

  if not Answer then
    Exit;

  FPackage.DataBase := TDataBase.Create;

  FPackage.DataBase.Host := FConfiguration.DBHost;
  FPackage.DataBase.User := FConfiguration.DBUser;
  FPackage.DataBase.Pass := FConfiguration.DBPass;
  FPackage.DataBase.Port := FConfiguration.DBPort;

  ReadDBHost;
  ReadDBFile;
  ReadDBUser;
  ReadDBPass;
  ReadDBPort;
end;

procedure TInitCommand.ReadProjectDescription;
var
  Value: string;
begin
  Value := FConsoleIO.ReadData('description: ').Trim;

  if not Value.IsEmpty then
    FPackage.Description := Value;
end;

procedure TInitCommand.ReadProjectMigrationDir;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('migration-dir: ', FPackage.MigrationDir);

  Value := ReadDir(Msg);

  if not Value.IsEmpty then
    FPackage.MigrationDir := Value;
end;

procedure TInitCommand.ReadProjectModular;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('modular: ', IfThen(FConfiguration.Modular, 'yes', 'no'));

  FPackage.Modular := FConsoleIO.ReadBoolean(Msg, FConfiguration.Modular);
end;

procedure TInitCommand.ReadProjectName;
var
  CurrentDir, Msg, Value: string;
begin
  Msg := 'name: ';
  CurrentDir := TPath.GetFileName(GetCurrentDir).Trim;

  if not CurrentDir.IsEmpty then
    Msg := Msg + '('+CurrentDir+') ';

  Value := FConsoleIO.ReadData(Msg).Trim;

  if Value.IsEmpty then
    FPackage.Name := CurrentDir
  else
    FPackage.Name := Value;
end;

procedure TInitCommand.ReadProjectNamespace;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('namespace: ', FPackage.Namespace);

  Value := FConsoleIO.ReadData(Msg).Trim;

  if not Value.IsEmpty then
    FPackage.Namespace := Value;
end;

procedure TInitCommand.ReadProjectPackageDir;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('packages-dir: ', FPackage.PackagesDir);

  Value := ReadDir(Msg);

  if not Value.IsEmpty then
    FPackage.PackagesDir := Value;
end;

procedure TInitCommand.ReadProjectSourceDir;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('source-dir: ', FPackage.SourceDir);

  Value := ReadDir(Msg);

  if not Value.IsEmpty then
    FPackage.SourceDir := Value;
end;

procedure TInitCommand.ReadProjectTestsDir;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('tests-dir: ', FPackage.TestsDir);

  Value := ReadDir(Msg);

  if not Value.IsEmpty then
    FPackage.TestsDir := Value;
end;

procedure TInitCommand.ReadProjectVersion;
var
  Msg, Value: string;
begin
  Msg := CreateMessage('version: ', FPackage.Version);

  Value := FConsoleIO.ReadData(Msg).Trim;

  if not Value.IsEmpty then
    FPackage.Version := Value;
end;

function TInitCommand.ReadRequiredData(const Msg, Default, Alert: string): string;
begin
  repeat
    Result := FConsoleIO.ReadData(Msg).Trim;

    if not (Default.IsEmpty and Result.IsEmpty) then
      Break;

    FConsoleIO.WriteError(Alert);
  until (False);
end;

procedure TInitCommand.ShowGenerationConfirmation;
var
  Data: string;
  Answer: Boolean;
begin

  Data := TJSON.Stringify<TPackage>(FPackage, True);

  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo(Data);
  FConsoleIO.WriteInfo('');

  Answer := FConsoleIO.ReadBoolean('Do you confirm generation (yes)? ', True);

  if not Answer then
    Abort;

  TFile.WriteAllText('package.json', Data);
end;

initialization
  TAlfred.GetInstance.Register(TInitCommand);
end.
