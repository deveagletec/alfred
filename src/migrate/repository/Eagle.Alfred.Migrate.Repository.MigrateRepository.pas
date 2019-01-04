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
    procedure executeFileScript(const Migrate: IMigrate; const executionMode: TExecutionModeMigrate);
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
    procedure executeFileScript(const Migrate: IMigrate; const executionMode: TExecutionModeMigrate);
    function getLastScriptExecuted: String;
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

procedure TMigrateRepository.executeFileScript(const Migrate: IMigrate; const executionMode: TExecutionModeMigrate);
var
  SQL: String;
begin

  if executionMode = TExecutionModeMigrate.TUp then
    SQL := Migrate.Up
  else
    SQL := Migrate.Down;

  if SQL.Trim().IsEmpty() then
    exit;

  FFDConnection.GetConnection.StartTransaction;

  try

    FDQuery.ExecSQL(SQL);

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

procedure TMigrateRepository.insertCodeMigrate(const migrateIdentifier: String);
begin
  FDQuery.ExecSQL(Format('INSERT INTO MIGRATIONS VALUES (%s);', [migrateIdentifier.QuotedString]));
end;

end.
