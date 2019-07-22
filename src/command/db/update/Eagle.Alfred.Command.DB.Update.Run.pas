unit Eagle.Alfred.Command.DB.Update.Run;

interface

uses
  System.Classes,
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,

  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Command.DB.Common.Repository;

type

  [Command('db:update', 'run', 'Run update file')]
  TUpdateRun = class(TCommandAbstract)
  private

    FVersion: String;

    FRepository: IRepository;

    procedure ShowMessageFileNotFound;
    procedure ShowMessageError(const error: String);
    procedure ShowMessageSucessFull;
    procedure ShowMessageWait;

  public
    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute(1, 'version')]
    procedure SetVersion(const Version: string);

  end;

implementation

destructor TUpdateRun.Destroy;
begin

  inherited;
end;

procedure TUpdateRun.Execute;
var
  FileName: String;
  SQL: String;
begin
  inherited;

  FileName := Format('%s%s%s', ['Update_', FVersion, '.sql']);

  if not FileExists(FPackage.UpdateScriptDir + FileName) then
  begin
    ShowMessageFileNotFound();
    exit;
  end;

  ShowMessageWait();

  try

    FRepository.RunFile(FPackage.UpdateScriptDir + fileName);

    ShowMessageSucessFull();

  except

    on E: Exception do
      ShowMessageError(E.Message);

  end;

end;

procedure TUpdateRun.Init;
begin
  inherited;

  FRepository := TRepository.Create(FPackage);
end;

procedure TUpdateRun.SetVersion(const Version: string);
begin
  FVersion := Version;
end;

procedure TUpdateRun.ShowMessageFileNotFound;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteAlert('* ------- ');
  FConsoleIO.WriteAlert('| Update File Not Found! ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateRun.ShowMessageError(const error: String);
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('| %s', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateRun.ShowMessageSucessFull;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteSuccess('* ------- ');
  FConsoleIO.WriteSuccess('| Update File Executed Successful! ');
  FConsoleIO.WriteSuccess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TUpdateRun.ShowMessageWait;
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
