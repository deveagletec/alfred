unit Eagle.Alfred.Command.DB.Common.Repository;

interface

uses
  System.SysUtils,

  FireDAC.Comp.Client,
  FireDAC.Comp.Script,
  FireDAC.UI.Intf,
  FireDAC.Comp.ScriptCommands,

  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Command.DB.Common.Connection,
  Eagle.Alfred.Command.DB.Common.FiredacConnection;

type

  IRepository = interface
    procedure runFile(const fileName: String);
    procedure runSQL(const SQL: String);
  end;

  TRepository = class(TInterfacedObject, IRepository)
  private

    FDScript: TFDScript;
    FFDConnection: IConnection;

    FPackage: TPackage;

    procedure ConsoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);

  public
    constructor Create(const APackage: TPackage);
    destructor Destroy; override;
    procedure runFile(const fileName: string);
    procedure runSQL(const SQL: string);
  end;

implementation

constructor TRepository.Create(const APackage: TPackage);
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

  FFDConnection := TFireDacFirebirdConnection.Create(HostName, DataBase, UserName, Password, Port);

  FFDConnection.GetConnection.TxOptions.AutoCommit := False;

  FDScript := TFDScript.Create(nil);

  FDScript.ScriptOptions.FeedbackScript := True;
  FDScript.OnConsolePut := ConsoleLog;

  FDScript.Connection := FFDConnection.GetConnection;

end;

destructor TRepository.Destroy;
begin
  inherited;

  FDScript.Free();

end;

procedure TRepository.ConsoleLog(AEngine: TFDScript; const AMessage: string; AKind: TFDScriptOutputKind);
begin

  if AKind = SoError then
    raise Exception.Create(AMessage);

end;

procedure TRepository.runFile(const fileName: string);
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

procedure TRepository.runSQL(const SQL: string);
begin
  FFDConnection.GetConnection.ExecSQL(SQL);
end;

end.
