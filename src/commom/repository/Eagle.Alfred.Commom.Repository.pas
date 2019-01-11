unit Eagle.Alfred.Commom.Repository;

interface

uses
  System.SysUtils,

  FireDAC.Comp.Client,
  FireDAC.Comp.Script,
  FireDAC.UI.Intf,
  FireDAC.Comp.ScriptCommands,

  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.DB.Connection,
  Eagle.Alfred.DB.Impl.FiredacConnection;

type

  IRepository = interface
    procedure runFile(const fileName: String);
    procedure runSQL(const SQL: String);
  end;

  TRepository = class(TInterfacedObject, IRepository)
  private
    FDScript: TFDScript;
    FFDConnection: IConnection;
    procedure consoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);

  public
    constructor Create;
    destructor Destroy; override;
    procedure runFile(const fileName: String);
    procedure runSQL(const SQL: String);
  end;

implementation

constructor TRepository.Create;
begin
  inherited;

  FFDConnection := TFireDacFirebirdConnection.Create;

  FFDConnection.GetConnection.TxOptions.AutoCommit := False;

  FDScript := TFDScript.Create(nil);

  FDScript.ScriptOptions.FeedbackScript := True;
  FDScript.OnConsolePut := consoleLog;

  FDScript.Connection := FFDConnection.GetConnection;

end;

destructor TRepository.Destroy;
begin
  inherited;

  FDScript.Free();

end;

procedure TRepository.consoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
begin

  if AKind = SoError then
    raise Exception.Create(AMessage);

end;

procedure TRepository.runFile(const fileName: String);
begin

  if not FileExists(fileName) then
    raise EAlfredFileNotFoundException.Create('File not found');

  FFDConnection.GetConnection.StartTransaction;

  try

    FDScript.SQLScriptFileName := fileName;

    FDScript.ValidateAll();

    FDScript.ExecuteAll();

  except

    on e: Exception do
    begin
      FFDConnection.GetConnection.Rollback();
      raise;
    end;

  end;

end;

procedure TRepository.runSQL(const SQL: String);
begin
  FFDConnection.GetConnection.ExecSQL(SQL);
end;

end.
