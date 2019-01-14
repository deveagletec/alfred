unit Eagle.Alfred.Command.Generate.Migrate;

interface

uses
  Classes,
  System.DateUtils,
  System.SysUtils,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Model.Migrate;

type

  [Command('generate', 'migrate', 'Generate a Migrate')]
  TGenerateMigrateCommand = class(TCommandAbstract)
  private

    FMigrateName: String;
    FVersion: String;

    FMigrateService: IMigrateService;
    procedure showMessageError(const error: String);
    procedure showMessageSucessfull;

  public

    procedure Execute; override;
    procedure init; override;

    [ParamAttribute(1, 'Migrate Name')]
    procedure setName(const name: String);

    [ParamAttribute(2, 'Migrate Version', False)]
    procedure setVersion(const version: String);

  end;

implementation

procedure TGenerateMigrateCommand.Execute;
var
  TimeStamp: String;
  Migrate: TMigrate;
begin

  TimeStamp := DateTimeToUnix(Now).ToString;

  if FVersion.Trim().IsEmpty() then
    FVersion := FPackage.version;

  Migrate.issueIdentifier := FMigrateName.ToUpper;
  Migrate.version := FVersion;
  Migrate.unixIdentifier := TimeStamp;

  try

    FMigrateService.createNewMigrate(Migrate);

    showMessageSucessfull();

  except

    on e: EAlfredException do
      showMessageError(e.Message)
    else
      raise;

  end;

end;

procedure TGenerateMigrateCommand.init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);

end;

procedure TGenerateMigrateCommand.setName(const name: String);
begin
  FMigrateName := name;
end;

procedure TGenerateMigrateCommand.setVersion(const version: String);
begin
  FVersion := version;
end;

procedure TGenerateMigrateCommand.showMessageError(const error: String);
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('| %s :( ', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TGenerateMigrateCommand.showMessageSucessfull;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteSucess('* ------- ');
  FConsoleIO.WriteSucess('| Migrate Created Sucessfull ;) ');
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TGenerateMigrateCommand);

end.
