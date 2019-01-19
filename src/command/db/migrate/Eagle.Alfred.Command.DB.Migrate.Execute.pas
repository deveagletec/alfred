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

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository;

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
    procedure ShowMessageError(const error: string);
    procedure ShowMessageNoneMigrateFounded;
    procedure ShowMessageSucessfull;


  public

    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute('version', 'Version filter', False)]
    procedure SetVersion(const version: string);

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

    try

      ListMigratesExecuted := FMigrateRepository.GetListMigratesExecuted();

      FMigrates := FMigrateService.getMigratesByMigrationDir();

      if (Assigned(ListMigratesExecuted)) and (ListMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
        FMigrateService.RemoveMigratesUnusableList(TExecutionModeMigrate.TUp, FMigrates, ListMigratesExecuted);

      if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
      begin
        ShowMessageNoneMigrateFounded();
        Exit;
      end;

      ExecuteMigrates();

      ShowMessageSucessfull();

    except

      on E: Exception do
        ShowMessageError(E.Message);

    end;

  finally

    if Assigned(ListMigratesExecuted) then
      ListMigratesExecuted.Free();

  end;

end;

procedure TMigrateExecute.ExecuteMigrates;
var
  Migrate: TMigrate;
  Answer: string;
  CanExecute: Boolean;
begin

  FConsoleIO.WriteInfo('');

  for Migrate in FMigrates do
  begin

    if FIsInteractiveMode then
    begin

      Answer := FConsoleIO.ReadInfo(Format('Wish execute the file %s? <Y>es | <N>o', [Migrate.IssueIdentifier]));

      if not Answer.ToUpper.Equals(SIM) then
        Exit;

    end;

    CanExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      CanExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      CanExecute := Migrate.version.Equals(FFilter)
    else
    begin
      CanExecute := Migrate.UnixIdentifier <= FFilter;

      if not CanExecute then
        Exit;

    end;

    if CanExecute then
    begin

      FMigrateRepository.ExecuteMigrate(Migrate, TExecutionModeMigrate.TUp, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s_%s executed', [Migrate.UnixIdentifier, Migrate.IssueIdentifier]));

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

procedure TMigrateExecute.SetVersion(const version: string);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByVersion;

  FFilter := version;

end;

procedure TMigrateExecute.ShowMessageError(const error: String);
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('| %s :( ', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateExecute.ShowMessageNoneMigrateFounded;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateExecute.ShowMessageSucessfull;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteSucess('* ------- ');
  FConsoleIO.WriteSucess('| Migrates Executed Sucessfull ;) ');
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TMigrateExecute);

end.
