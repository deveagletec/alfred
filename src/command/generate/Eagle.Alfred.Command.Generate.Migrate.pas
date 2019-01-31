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

  Eagle.Alfred.Command.Common.Migrate.Service,
  Eagle.Alfred.Command.Common.Migrate.Model;

type

  [Command('generate', 'migrate', 'Generates a Migrate')]
  TGenerateMigrateCommand = class(TCommandAbstract)
  private
    FAuthor: string;
    FName: string;
    FVersion: string;
    FDescription: string;
    FIssue: string;
    FOpen: Boolean;
    FMigrateService: IMigrateService;

    procedure OpenMigrate(const Migrate: TMigrate);
  public

    procedure Execute; override;
    procedure Init; override;

    [Param(1, 'Migrate Name')]
    procedure SetName(const Value: string);

    [Param('issue', 'Migrate Issue', False)]
    procedure SetIssue(const Value: string);

    [Param('version', 'Migrate Version', False)]
    procedure SetVersion(const Value: string);

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
  Migrate.Name := Migrate.Id + '_' + FName;
  Migrate.Issue := FIssue;
  Migrate.Version := IfThen(FVersion.IsEmpty, FPackage.Version, FVersion);
  Migrate.Author := IfThen(FAuthor.IsEmpty, FConfiguration.Author, FAuthor);
  Migrate.Description := FDescription;

  FMigrateService.CreateNewMigrate(Migrate);

  DoShowMessageSuccessful('Migrate Created Successfull');

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

  FileName := FPackage.MigrationDir + Migrate.Name+ '.json';

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

procedure TGenerateMigrateCommand.SetIssue(const Value: string);
begin
  FIssue := Value.Trim.ToUpper;
end;

procedure TGenerateMigrateCommand.SetName(const Value: string);
begin
  FName := Value.Trim().ToUpper;
end;

procedure TGenerateMigrateCommand.EnableOpen;
begin
  FOpen := True;
end;

procedure TGenerateMigrateCommand.SetVersion(const Value: string);
begin
  FVersion := Value.Trim;
end;

initialization

TAlfred.GetInstance.Register(TGenerateMigrateCommand);

end.
