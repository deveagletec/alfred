unit Eagle.Alfred.Command.DB.Migrate.Rollback;

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

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository;

type

  [Command('db:migrate', 'down', 'Execute Migrates on Down Mode')]
  TMigrateRollback = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FIsAutoCommit: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;

    FMigrates: TList<TMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure ExecuteMigrates;
  public

    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

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

destructor TMigrateRollback.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TMigrateRollback.Execute;
var
  ListMigratesExecuted: TList<String>;
begin

  FMigrateService := TMigrateService.Create(FPackage);

  try

    ListMigratesExecuted := FMigrateRepository.GetListMigratesExecuted();

    if not(Assigned(ListMigratesExecuted)) or (ListMigratesExecuted.Count <= 0) then
      raise ENoExecutedMigrationFoundException.Create('No Executed Migrations Found');

    FMigrates := FMigrateService.GetMigratesByMigrationDir();

    if (Assigned(ListMigratesExecuted)) and (ListMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
      FMigrateService.RemoveMigratesUnusableList(TExecutionModeMigrate.TDown, FMigrates, ListMigratesExecuted);

    if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
      raise EMigrationsNotFoundException.Create('No Migration Found');

    ExecuteMigrates();

    DoShowMessageSuccessful('Migrates Executed Successful');

  finally
    if Assigned(ListMigratesExecuted) then
      ListMigratesExecuted.Free();
  end;
end;

procedure TMigrateRollback.ExecuteMigrates;
var
  Migrate: TMigrate;
  CanExecute: Boolean;
  Index: Integer;
begin

  FConsoleIO.WriteInfo('');

  for Index := FMigrates.Count - 1 downto 0 do
  begin

    Migrate := FMigrates.Items[Index];

    if FIsInteractiveMode then
    begin
      if not FConsoleIO.ReadBoolean(Format('Wish execute the file %s? <Y>es | <N>o', [Migrate.Issue]), False) then
        Exit;
    end;

    CanExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      CanExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      CanExecute := Migrate.Version.Equals(FFilter)
    else
    begin
      CanExecute := Migrate.Id >= FFilter;

      if not CanExecute then
        exit;

    end;

    if CanExecute then
    begin
      FMigrateRepository.ExecuteMigrate(Migrate, TExecutionModeMigrate.TDown, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s_%s executed', [Migrate.Id, Migrate.Issue]));

    end;

  end;

end;

procedure TMigrateRollback.Init;
begin
  inherited;

  FMigrateRepository := TMigrateRepository.Create(FPackage);
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInteractiveMode := False;
  FIsAutoCommit := False;
  FFilterTypeExecution := TMigrateFilterTypeExecution.TAll;

end;

procedure TMigrateRollback.SetAutoCommit;
begin
  FIsAutoCommit := True;
end;

procedure TMigrateRollback.SetInteractive;
begin
  FIsInteractiveMode := True;
end;

procedure TMigrateRollback.SetMigrate(const Migrate: string);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByMigrate;

  FFilter := Migrate;

end;

procedure TMigrateRollback.SetVersion(const Version: string);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByVersion;

  FFilter := Version;

end;

initialization

TAlfred.GetInstance.Register(TMigrateRollback);

end.
