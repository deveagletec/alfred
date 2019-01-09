unit Eagle.Alfred.Migrate.Repository.MigrateRepository;

interface

uses
  System.SysUtils, Classes,
  System.Generics.Collections,

  FireDAC.Comp.Client,
  FireDAC.Comp.Script,
  FireDAC.UI.Intf,
  FireDAC.Comp.ScriptCommands,

  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.DB.Connection,
  Eagle.Alfred.DB.Impl.FiredacConnection;

type

  IMigrateRepository = interface
    function getLastScriptExecuted: String;
    procedure executeMigrate(const Migrate: TMigrate; const executionMode: TExecutionModeMigrate; const isAutoCommit: Boolean = False);
    function getListMigratesExecuted(): TList<String>;
  end;

  TMigrateRepository = class(TInterfacedObject, IMigrateRepository)
  private
    FDQuery: TFDQuery;
    FFDConnection: IConnection;

    procedure consoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
    procedure deleteCodeMigrate(const migrateIdentifier: String);
    procedure insertCodeMigrate(const migrateIdentifier: String);

  public
    constructor Create;
    destructor Destroy; override;
    procedure executeMigrate(const Migrate: TMigrate; const executionMode: TExecutionModeMigrate; const isAutoCommit: Boolean = False);
    function getLastScriptExecuted: String;
    function getListMigratesExecuted(): TList<String>;
  end;

implementation

constructor TMigrateRepository.Create;
begin
  inherited;

  FFDConnection := TFireDacFirebirdConnection.Create;

  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FFDConnection.GetConnection;

end;

destructor TMigrateRepository.Destroy;
begin
  inherited;

  FDQuery.Free;
end;

procedure TMigrateRepository.executeMigrate(const Migrate: TMigrate; const executionMode: TExecutionModeMigrate; const isAutoCommit: Boolean = False);
var
  SQLList: TArray<String>;
  SQL: String;
begin

  if executionMode = TExecutionModeMigrate.TUp then
    SQLList := Migrate.up
  else
    SQLList := Migrate.down;

  FFDConnection.GetConnection.StartTransaction;

  try

    for SQL in SQLList do
    begin

      if SQL.Trim().IsEmpty() then
        continue;

      FDQuery.ExecSQL(SQL);

      if isAutoCommit then
        FFDConnection.GetConnection.Commit;

    end;

    if executionMode = TExecutionModeMigrate.TUp then
      insertCodeMigrate(Migrate.UnixIdentifier)
    else
      deleteCodeMigrate(Migrate.UnixIdentifier);

    FFDConnection.GetConnection.Commit;

  except

    on e: Exception do
    begin
      FFDConnection.GetConnection.Rollback;
      raise EDataBaseException.Create(Format('Erro ao executar arquivo %s! ||| %s', [Migrate.UnixIdentifier, e.Message]));
    end;

  end;

end;

function TMigrateRepository.getLastScriptExecuted: String;
var
  status: Boolean;
begin

  status := FDQuery.Connection.Connected;

  FDQuery.Open('SELECT MAX(ID) AS ID FROM MIGRATIONS');

  Result := FDQuery.FieldByName('ID').AsString;

end;

procedure TMigrateRepository.consoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
begin

  if AKind = SoError then
    raise Exception.Create(AMessage);

end;

procedure TMigrateRepository.deleteCodeMigrate(const migrateIdentifier: String);
begin
  FDQuery.ExecSQL(Format('DELETE FROM MIGRATIONS MG WHERE MG.ID = %s;', [migrateIdentifier.QuotedString]));
end;

function TMigrateRepository.getListMigratesExecuted: TList<String>;
var
  listMigratesExecuted: TList<String>;
  SQL: String;
begin

  SQL := 'SELECT * FROM MIGRATIONS';

  FDQuery.Open(SQL);

  if FDQuery.IsEmpty then
    exit(nil);

  listMigratesExecuted := TList<String>.Create();

  FDQuery.First;

  while not FDQuery.Eof do
  begin
    listMigratesExecuted.Add(FDQuery.FieldByName('ID').AsString);
    FDQuery.Next;
  end;

  Result := listMigratesExecuted;

end;

procedure TMigrateRepository.insertCodeMigrate(const migrateIdentifier: String);
begin
  FDQuery.ExecSQL(Format('INSERT INTO MIGRATIONS VALUES (%s);', [migrateIdentifier.QuotedString]));
end;

end.
