unit Eagle.Alfred.Command.Common.Migrate.Repository;

interface

uses
  System.SysUtils, Classes,
  System.Generics.Collections,

  FireDAC.Comp.Client,
  FireDAC.Comp.Script,
  FireDAC.Stan.Param,
  FireDAC.UI.Intf,
  FireDAC.Comp.ScriptCommands,

  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.DB.Common.Connection,
  Eagle.Alfred.Command.DB.Common.FiredacConnection;

type

  IMigrateRepository = interface
    function GetLastScriptExecuted: String;
    procedure ExecuteMigrate(const Migrate: TMigrate; const ExecutionMode: TExecutionModeMigrate; const IsAutoCommit: Boolean = False);
    function GetListMigratesExecuted(): TList<String>;
    function TableMigrateExists: Boolean;
    procedure CreateTableMigrate();
  end;

  TMigrateRepository = class(TInterfacedObject, IMigrateRepository)
  private
    FDQuery: TFDQuery;

    FDScript: TFDScript;

    FFDConnection: IConnection;

    FPackage: TPackage;

    procedure ConsoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
    procedure DeleteCodeMigrate(const MigrateIdentifier: string);
    procedure InsertCodeMigrate(const MigrateIdentifier: string);

  public
    constructor Create(APackage: TPackage);
    destructor Destroy; override;
    procedure CreateTableMigrate;
    procedure ExecuteMigrate(const Migrate: TMigrate; const ExecutionMode: TExecutionModeMigrate; const IsAutoCommit: Boolean = False);
    function GetLastScriptExecuted: string;
    function GetListMigratesExecuted(): TList<string>;
    function TableMigrateExists: Boolean;

  end;

implementation

constructor TMigrateRepository.Create(APackage: TPackage);
var
  DataBase, HostName, UserName, Password, Port: String;
begin
  inherited Create();

  if not Assigned(APackage.DataBase) then
    raise EPackageInvalidException.Create('DataBase property not found in Package File!');

  FPackage := APackage;

  DataBase := FPackage.DataBase.&File;
  HostName := FPackage.DataBase.Host;
  UserName := FPackage.DataBase.User;
  Password := FPackage.DataBase.Pass;
  Port := string.Parse(FPackage.DataBase.Port);

  FFDConnection := TFireDacFirebirdConnection.Create(FPackage.DataBase);

  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FFDConnection.GetConnection;

  FDScript := TFDScript.Create(nil);
  FDScript.Connection := FFDConnection.GetConnection;
  FDScript.ScriptOptions.MacroExpand := False;
  FDScript.OnConsolePut := ConsoleLog;

end;

destructor TMigrateRepository.Destroy;
begin
  inherited;
  FFDConnection.GetConnection.Connected := False;

  FDQuery.Free;
  FDScript.Free;
end;

procedure TMigrateRepository.ExecuteMigrate(const Migrate: TMigrate; const ExecutionMode: TExecutionModeMigrate; const IsAutoCommit: Boolean = False);
var
  SQLList: TArray<String>;
  SQL, SQLFormatado: string;
  Script: TStringList;
begin

  if ExecutionMode = TExecutionModeMigrate.TUp then
    SQLList := Migrate.up
  else
    SQLList := Migrate.down;

  FFDConnection.GetConnection.StartTransaction;

  Script := TStringList.Create();

  try
    try

      for SQL in SQLList do
      begin

        if SQL.Trim().IsEmpty() then
          continue;

        SQLFormatado := SQL.Replace(#9, #13#10);

        Script.Clear();
        Script.Add(SQLFormatado);

        FDScript.ExecuteScript(Script);

        if IsAutoCommit then
          FFDConnection.GetConnection.Commit;

      end;

      if ExecutionMode = TExecutionModeMigrate.TUp then
        InsertCodeMigrate(Migrate.Id)
      else
        DeleteCodeMigrate(Migrate.Id);

      FFDConnection.GetConnection.Commit;

    except

      on E: Exception do
      begin
        FFDConnection.GetConnection.Rollback;
        raise EDataBaseException.Create(Format('Erro ao executar arquivo %s! ||| %s', [Migrate.Id, E.Message]));
      end;

    end;
  finally
    Script.Free;
  end;

end;

function TMigrateRepository.GetLastScriptExecuted: string;
begin
  FDQuery.Open('SELECT MAX(ID) AS ID FROM MIGRATIONS');

  Result := FDQuery.FieldByName('ID').AsString;
end;

procedure TMigrateRepository.ConsoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
begin

  if AKind = SoError then
    raise Exception.Create(AMessage);

end;

procedure TMigrateRepository.CreateTableMigrate;
const
  SQL = 'CREATE TABLE MIGRATIONS(ID VARCHAR(50), CONSTRAINT PK_MIGRATION PRIMARY KEY(ID));';
begin

  FFDConnection.GetConnection.StartTransaction;

  try
    FDQuery.ExecSQL(SQL);
    FFDConnection.GetConnection.Commit;
  except

    on E: Exception do
    begin
      FFDConnection.GetConnection.Rollback;
      raise EDataBaseException.Create(Format('Not wass possible created Migrations Table ||| %s', [E.Message]));
    end;

  end;

end;

procedure TMigrateRepository.DeleteCodeMigrate(const MigrateIdentifier: string);
begin
  FDQuery.ExecSQL(Format('DELETE FROM MIGRATIONS MG WHERE MG.ID = %s;', [MigrateIdentifier.QuotedString]));
end;

function TMigrateRepository.GetListMigratesExecuted: TList<string>;
var
  ListMigratesExecuted: TList<String>;
  SQL: String;
begin

  SQL := 'SELECT * FROM MIGRATIONS';

  FDQuery.Open(SQL);

  if FDQuery.IsEmpty then
    exit(nil);

  ListMigratesExecuted := TList<String>.Create();

  FDQuery.First;

  while not FDQuery.Eof do
  begin
    ListMigratesExecuted.Add(FDQuery.FieldByName('ID').AsString);
    FDQuery.Next;
  end;

  Result := ListMigratesExecuted;

end;

procedure TMigrateRepository.InsertCodeMigrate(const MigrateIdentifier: string);
begin
  FDQuery.ExecSQL(Format('INSERT INTO MIGRATIONS VALUES (%s);', [MigrateIdentifier.QuotedString]));
end;

function TMigrateRepository.TableMigrateExists: Boolean;
var
  TablesNameList: TStringList;
  TableMigrateExists: Boolean;
begin

  TablesNameList := TStringList.Create();

  try

    FFDConnection.GetConnection.GetTableNames('', '', '', TablesNameList);

    TableMigrateExists := TablesNameList.IndexOf('MIGRATIONS') >= 0;

  finally
    TablesNameList.Free();
  end;

  Result := TableMigrateExists;

end;

end.
