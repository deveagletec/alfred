unit Eagle.Alfred.Command.Generate.Migrate;

interface

uses
  Classes,
  System.DateUtils,
  System.SysUtils,
  System.StrUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.IOUtils,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Model.Migrate;

type

  [Command('generate', 'migrate', 'Generates a Migrate')]
  TGenerateMigrateCommand = class(TCommandAbstract)
  private
    FAuthor: string;
    FName: string;
    FVersion: string;
    FDescription: string;

    FMigrateService: IMigrateService;
    FOpen: Boolean;

    procedure OpenMigrate(const Migrate: TMigrate);
  public

    procedure Execute; override;
    procedure Init; override;

    [Param(1, 'Migrate Name')]
    procedure SetName(const Name: string);

    [Param(2, 'Migrate Version', False)]
    procedure SetVersion(const Version: string);

    [Param('author', 'Migrate Author', False)]
    procedure SetAuthor(const Value: string);

    [Param('desc', 'Migrate Description', False)]
    procedure SetDescription(const Value: string);

    [Option('open', 'o', 'Active migration opening after generation')]
    procedure EnableOpen;
  end;

implementation

procedure TGenerateMigrateCommand.Execute;
var
  Migrate: TMigrate;
begin

  Migrate.Id := DateTimeToUnix(Now).ToString;
  Migrate.Issue := FName;
  Migrate.Version := IfThen(FVersion.IsEmpty, FPackage.Version, FVersion);
  Migrate.Author := IfThen(FAuthor.IsEmpty, FConfiguration.Author, FAuthor);
  Migrate.Description := FDescription;

  FMigrateService.CreateNewMigrate(Migrate);

  DoShowMessageSuccessful('Migrate Created Sucessfull');

  OpenMigrate(Migrate);
end;

procedure TGenerateMigrateCommand.Init;
begin
  inherited;
  FMigrateService := TMigrateService.Create(FPackage);
  FOpen := FConfiguration.AutoOpen;
end;

procedure TGenerateMigrateCommand.OpenMigrate(const Migrate: TMigrate);
var
  FileName: string;
begin
  if not FOpen then
    Exit;

  FileName := Format('%s%s_%s.json', [FPackage.MigrationDir, Migrate.Id, Migrate.Issue]);

  TIOUtils.OpenFile(FileName, FConfiguration.DefaultEditor);
end;

procedure TGenerateMigrateCommand.SetAuthor(const Value: string);
begin
  FAuthor := Value.Trim;
end;

procedure TGenerateMigrateCommand.SetDescription(const Value: string);
begin
  FDescription := Value.Trim;
end;

procedure TGenerateMigrateCommand.SetName(const Name: String);
begin
  FName := Name.Trim().ToUpper;
end;

procedure TGenerateMigrateCommand.EnableOpen;
begin
  FOpen := True;
end;

procedure TGenerateMigrateCommand.SetVersion(const Version: string);
begin
  FVersion := Version.Trim;
end;

initialization

TAlfred.GetInstance.Register(TGenerateMigrateCommand);

end.
