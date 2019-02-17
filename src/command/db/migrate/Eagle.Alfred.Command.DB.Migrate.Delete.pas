unit Eagle.Alfred.Command.DB.Migrate.Delete;

interface

uses
  SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Enums,

  Eagle.Alfred.Command.Common.Migrate.Model,

  Eagle.Alfred.Command.Common.Migrate.Service,
  Eagle.Alfred.Command.Common.Migrate.Repository;

type

  [Command('db:migrate', 'delete', 'Delete Migrates')]
  TMigrateDeleteCommand = class(TCommandAbstract)
  private

    FMigrateIdentifier: string;
    FMigrateService: IMigrateService;
    FMigrateRepository: IMigrateRepository;

  public

    procedure Execute(); override;
    procedure Init(); override;

    [Param(1, 'Migrate Identifier', True)]
    procedure SetMigrateIdentifier(const MigrateIdentifier: string);

  end;

implementation

procedure TMigrateDeleteCommand.Execute;
const
  AUTO_COMMIT = True;
var
  Migrate: TMigrate;
  Answer: Boolean;
  FileName: string;
begin
  inherited;

  Answer := False;
  Migrate := FMigrateService.GetMigrateByIdentifier(FMigrateIdentifier);

  if Migrate.Id.IsEmpty then
    raise EMigrationsNotFoundException.Create('No migration found');

  if Migrate.WasExecuted then
    Answer := FConsoleIO.ReadBoolean(sLineBreak + 'This Migrate already executed. Do you want to run the migrate in down mode?', False);

  if Answer then
    FMigrateRepository.ExecuteMigrate(Migrate, TDown, AUTO_COMMIT);

  FileName := Format('%s%s', [FPackage.MigrationDir, Migrate.Name]);

  try
    DeleteFile(FileName);
  except

    on E: Exception do
      raise EAlfredDeleteFileException.Create(E.Message);

  end;

  DoShowMessageSuccessful('Migrate removed sucessfull.');

end;

procedure TMigrateDeleteCommand.Init;
begin
  inherited;
  FMigrateService := TMigrateService.Create(FPackage);
  FMigrateRepository := TMigrateRepository.Create(FPackage);
end;

procedure TMigrateDeleteCommand.SetMigrateIdentifier(const MigrateIdentifier: string);
begin
  FMigrateIdentifier := MigrateIdentifier;
end;

initialization

TAlfred.GetInstance.Register(TMigrateDeleteCommand);

end.
