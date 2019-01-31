unit Eagle.Alfred.Command.DB.Migrate.Execute;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.Common.Migrate.Service,
  Eagle.Alfred.Command.Common.Migrate.Repository;

type

  [Command('db:migrate', 'up', 'Execute Migrates on Up Mode')]
  TMigrateExecute = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: string;
    FIsAutoCommit: Boolean;

    FMigrates: TList<TMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure ExecuteMigrates;
  protected
    procedure Init; override;
  public

    destructor Destroy; override;
    procedure Execute; override;

    [Param('version', 'Version filter', False)]
    procedure SetVersion(const Version: string);

    [Param('migrate', 'Limit execution migrate', False)]
    procedure SetMigrate(const Migrate: string);

    [Option('iterative', 'i', 'Execution in interactive mode')]
    procedure SetInteractive;

    [Option('auto-commit', 'c', 'Auto Commit')]
    procedure SetAutoCommit;
  end;

implementation

destructor TMigrateExecute.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TMigrateExecute.Execute;
var
  ListMigratesExecuted: TList<String>;
begin

  FMigrateService := TMigrateService.Create(FPackage);

  try

    ListMigratesExecuted := FMigrateRepository.GetListMigratesExecuted();

    FMigrates := FMigrateService.getMigratesByMigrationDir();

    if (Assigned(ListMigratesExecuted)) and (ListMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
      FMigrateService.RemoveMigratesUnusableList(TExecutionModeMigrate.TUp, FMigrates, ListMigratesExecuted);

    if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
      raise EMigrationsNotFoundException.Create('No migration found');

    ExecuteMigrates();

    DoShowMessageSuccessful('Migrates Executed Successful');

  finally
    if Assigned(ListMigratesExecuted) then
      ListMigratesExecuted.Free();
  end;
end;

procedure TMigrateExecute.ExecuteMigrates;
var
  Migrate: TMigrate;
  CanExecute: Boolean;
begin

  FConsoleIO.WriteInfo('');

  for Migrate in FMigrates do
  begin

    if FIsInteractiveMode then
    begin
      if not FConsoleIO.ReadBoolean(Format('Wish execute the file %s? <Y>es | <N>o', [Migrate.Issue]), False) then
        Exit;
    end;

    CanExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      CanExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      CanExecute := Migrate.version.Equals(FFilter)
    else
    begin
      CanExecute := Migrate.Id <= FFilter;

      if not CanExecute then
        Exit;

    end;

    if CanExecute then
    begin

      FMigrateRepository.ExecuteMigrate(Migrate, TExecutionModeMigrate.TUp, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s_%s executed', [Migrate.Id, Migrate.Issue]));

    end;

  end;

end;

procedure TMigrateExecute.Init;
begin
  inherited;

  FMigrateRepository := TMigrateRepository.Create(FPackage);
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInteractiveMode := False;
  FIsAutoCommit := False;
  FFilterTypeExecution := TMigrateFilterTypeExecution.TAll;

end;

procedure TMigrateExecute.SetAutoCommit;
begin
  FIsAutoCommit := True;
end;

procedure TMigrateExecute.SetInteractive;
begin
  FIsInteractiveMode := True;
end;

procedure TMigrateExecute.SetMigrate(const Migrate: string);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByMigrate;

  FFilter := Migrate;

end;

procedure TMigrateExecute.SetVersion(const Version: string);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByVersion;

  FFilter := Version;

end;

initialization

TAlfred.GetInstance.Register(TMigrateExecute);

end.
