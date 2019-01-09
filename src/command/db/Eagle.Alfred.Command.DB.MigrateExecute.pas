unit Eagle.Alfred.Command.DB.MigrateExecute;

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
  Eagle.Alfred.Migrate.Repository.MigrateRepository,
  Eagle.ConsoleIO;

type

  [Command('db:migrate', 'up', 'Realiza a execução dos migrates no banco de dados')]
  TMigrateExecute = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;
    FIsAutoCommit: Boolean;

    FMigrates: TList<TMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure executeMigrates;

  public

    destructor Destroy; override;

    procedure execute; override;
    procedure Init; override;

    [ParamAttribute('version', 'Filtro de versão', False)]
    procedure setVersion(const version: String);

    [ParamAttribute('migrate', 'Migrate limite de execução', False)]
    procedure setMigrate(const Migrate: String);

    [OptionAttribute('IsInteractiveMode', 'i', 'Informar se a execução será em modo interativo')]
    procedure setInteractive;

    [OptionAttribute('IsAutoCommit', 'c', 'Commit cadas script do migrate automaticamente')]
    procedure setAutoCommit;

  end;

const
  SIM = 'S';

implementation

destructor TMigrateExecute.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TMigrateExecute.execute;
var
  listMigratesExecuted: TList<String>;
begin

  FMigrateService := TMigrateService.Create(FPackage);

  listMigratesExecuted := FMigrateRepository.getListMigratesExecuted();

  try

    FMigrates := FMigrateService.getMigratesByMigrationDir();

    if (Assigned(listMigratesExecuted)) and (listMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
      FMigrateService.removeMigratesUnusableList(TExecutionModeMigrate.TUp, FMigrates, listMigratesExecuted);

    if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
    begin
      FConsoleIO.WriteInfo('* ------- ');
      FConsoleIO.WriteInfo('| None Migrate Founded! ');
      FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
      exit;
    end;

    executeMigrates();

    FConsoleIO.WriteInfo(' * ------- ');
    FConsoleIO.WriteInfo(' | Migrates Executed Sucessfull ;) ');
    FConsoleIO.WriteInfo(' * ----------------------------------------------------- ');

  finally

    if Assigned(listMigratesExecuted) then
      listMigratesExecuted.Free();

  end;

end;

procedure TMigrateExecute.executeMigrates;
var
  Migrate: TMigrate;
  answer: string;
  canExecute: Boolean;
begin

  for Migrate in FMigrates do
  begin

    if FIsInteractiveMode then
    begin

      answer := FConsoleIO.ReadInfo(Format('Deseja executar o arquivo %s? <S>im | <N>ao', [Migrate.IssueIdentifier]));

      if not answer.ToUpper.Equals(SIM) then
        exit;

    end;

    canExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      canExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      canExecute := Migrate.version.Equals(FFilter)
    else
    begin
      canExecute := Migrate.UnixIdentifier <= FFilter;

      if not canExecute then
        exit;

    end;

    if canExecute then
    begin

      FMigrateRepository.executeMigrate(Migrate, TExecutionModeMigrate.TUp, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s executed', [Migrate.UnixIdentifier]));

    end;

  end;

end;

procedure TMigrateExecute.Init;
begin
  inherited;

  FMigrateRepository := TMigrateRepository.Create();
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInteractiveMode := False;
  FIsAutoCommit := False;
  FFilterTypeExecution := TMigrateFilterTypeExecution.TAll;

end;

procedure TMigrateExecute.setAutoCommit;
begin
  FIsAutoCommit := True;
end;

procedure TMigrateExecute.setInteractive;
begin
  FIsInteractiveMode := True;
end;

procedure TMigrateExecute.setMigrate(const Migrate: String);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByMigrate;

  FFilter := Migrate;

end;

procedure TMigrateExecute.setVersion(const version: String);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByVersion;

  FFilter := version;

end;

initialization

TAlfred.GetInstance.Register(TMigrateExecute);

end.
