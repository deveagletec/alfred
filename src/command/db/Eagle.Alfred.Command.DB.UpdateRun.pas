unit Eagle.Alfred.Command.DB.UpdateRun;

interface

uses
  System.Classes,
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,

  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Commom.Repository;

type

  [Command('db:update', 'run', 'Executa arquivos de atualização de banco')]
  TUpdateRun = class(TCommandAbstract)
  private

    FVersion: String;

    FRepository: IRepository;

    procedure showMessageFileNotFound;
    procedure showMessageError(const error: String);
    procedure showMessageSucessFull;
    procedure showMessageWait;

  public
    destructor Destroy; override;

    procedure execute; override;
    procedure init; override;

    [ParamAttribute(1, 'Versão atual')]
    procedure setVersion(const version: String);

  end;

implementation

destructor TUpdateRun.Destroy;
begin

  inherited;
end;

procedure TUpdateRun.execute;
var
  fileName, filePath: String;
  SQL: String;
begin
  inherited;

  filePath := Format('%s%s%s', [FPackage.BaseDir, FPackage.MigrationDir, 'Updates\']);

  fileName := Format('%s%s%s', ['Update_', FVersion, '.sql']);

  if not FileExists(filePath + fileName) then
  begin
    showMessageFileNotFound();
    exit;
  end;

  showMessageWait();

  try

    FRepository.runFile(filePath + fileName);

    showMessageSucessFull();

  except

    on e: Exception do
      showMessageError(e.Message);

  end;

end;

procedure TUpdateRun.init;
begin
  inherited;

  FRepository := TRepository.Create();

end;

procedure TUpdateRun.setVersion(const version: String);
begin
  FVersion := version;
end;

procedure TUpdateRun.showMessageFileNotFound;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteAlert('* ------- ');
  FConsoleIO.WriteAlert('| Update File Not Found! ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateRun.showMessageError(const error: String);
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('| %s', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateRun.showMessageSucessFull;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteSucess('* ------- ');
  FConsoleIO.WriteSucess('| Update File Executed Sucessfull! ');
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TUpdateRun.showMessageWait;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| Wait while update script is executing ... ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TUpdateRun);

end.
