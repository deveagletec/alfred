unit Eagle.Alfred.Command.DB.MigrateRollback;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,

  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Enums,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository;

type

  [Command('db:migrate', 'down', 'Execute the migrates in database on Down Mode')]
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
    procedure ShowMessageError(const error: String);
    procedure ShowMessageNoneMigrateExecutedFounded;
    procedure ShowMessageNoneMigrateFounded;
    procedure ShowMessageSucessFull;

  public

    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute('version', 'Version filter', False)]
    procedure SetVersion(const Version: string);

    [ParamAttribute('migrate', 'Limit execution migrate', False)]
    procedure SetMigrate(const Migrate: string);

    [OptionAttribute('IsInteractiveMode', 'i', 'Execution in interactive mode')]
    procedure SetInteractive;

    [OptionAttribute('IsAutoCommit', 'c', 'Auto Commit')]
    procedure SetAutoCommit;

  end;

const
  SIM = 'Y';

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

    try

      ListMigratesExecuted := FMigrateRepository.GetListMigratesExecuted();

      if not(Assigned(ListMigratesExecuted)) or (ListMigratesExecuted.Count <= 0) then
      begin
        ShowMessageNoneMigrateExecutedFounded();
        exit;
      end;

      FMigrates := FMigrateService.GetMigratesByMigrationDir();

      if (Assigned(ListMigratesExecuted)) and (ListMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
        FMigrateService.RemoveMigratesUnusableList(TExecutionModeMigrate.TDown, FMigrates, ListMigratesExecuted);

      if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
      begin
        ShowMessageNoneMigrateFounded();
        exit;
      end;

      ExecuteMigrates();

      ShowMessageSucessFull();

    except

      on E: Exception do
        ShowMessageError(E.Message);

    end;

  finally

    if Assigned(ListMigratesExecuted) then
      ListMigratesExecuted.Free();

  end;

end;

procedure TMigrateRollback.ExecuteMigrates;
var
  Migrate: TMigrate;
  Answer: string;
  CanExecute: Boolean;
  Index: Integer;
begin

  FConsoleIO.WriteInfo('');

  for Index := FMigrates.Count - 1 downto 0 do
  begin

    Migrate := FMigrates.Items[Index];

    if FIsInteractiveMode then
    begin

      Answer := FConsoleIO.ReadInfo(Format('Wish execute the file %s? <Y>es | <N>o', [Migrate.IssueIdentifier]));

      if not Answer.ToUpper.Equals(SIM) then
        exit;

    end;

    CanExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      CanExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      CanExecute := Migrate.Version.Equals(FFilter)
    else
    begin
      CanExecute := Migrate.UnixIdentifier >= FFilter;

      if not CanExecute then
        exit;

    end;

    if CanExecute then
    begin
      FMigrateRepository.ExecuteMigrate(Migrate, TExecutionModeMigrate.TDown, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s executed', [Migrate.UnixIdentifier]));

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

procedure TMigrateRollback.ShowMessageError(const error: String);
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('| %s :( ', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateRollback.ShowMessageNoneMigrateExecutedFounded;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Executed Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateRollback.ShowMessageNoneMigrateFounded;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateRollback.ShowMessageSucessFull;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteSucess('* ------- ');
  FConsoleIO.WriteSucess('| Migrates Executed Sucessfull ;) ');
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TMigrateRollback);

end.
