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

  [Command('db:migrate', 'down', 'Realiza a execução dos migrates no modo Down no banco de dados')]
  TMigrateRollback = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FIsAutoCommit: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;

    FMigrates: TList<TMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure executeMigrates;
    procedure showMessageNoneMigrateExecutedFounded;
    procedure showMessageNoneMigrateFounded;
    procedure showMessageSucessFull;

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

destructor TMigrateRollback.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TMigrateRollback.execute;
var
  listMigratesExecuted: TList<String>;
begin

  FMigrateService := TMigrateService.Create(FPackage);

  listMigratesExecuted := FMigrateRepository.getListMigratesExecuted();

  try

    if not(Assigned(listMigratesExecuted)) or (listMigratesExecuted.Count <= 0) then
    begin
      showMessageNoneMigrateExecutedFounded();
      exit;
    end;

    FMigrates := FMigrateService.getMigratesByMigrationDir();

    if (Assigned(listMigratesExecuted)) and (listMigratesExecuted.Count > 0) and (Assigned(FMigrates)) then
      FMigrateService.removeMigratesUnusableList(TExecutionModeMigrate.TDown, FMigrates, listMigratesExecuted);

    if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
    begin
      showMessageNoneMigrateFounded();
      exit;
    end;

    executeMigrates();

    showMessageSucessFull();

  finally

    if Assigned(listMigratesExecuted) then
      listMigratesExecuted.Free();

  end;

end;

procedure TMigrateRollback.executeMigrates;
var
  Migrate: TMigrate;
  answer: string;
  canExecute: Boolean;
  index: Integer;
begin

  FConsoleIO.WriteInfo('');

  for index := FMigrates.Count - 1 downto 0 do
  begin

    Migrate := FMigrates.Items[index];

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
      canExecute := Migrate.UnixIdentifier >= FFilter;

      if not canExecute then
        exit;

    end;

    if canExecute then
    begin
      FMigrateRepository.executeMigrate(Migrate, TExecutionModeMigrate.TDown, FIsAutoCommit);

      FConsoleIO.WriteInfo(Format('| Migrate %s executed', [Migrate.UnixIdentifier]));

    end;

  end;

end;

procedure TMigrateRollback.Init;
begin
  inherited;

  FMigrateRepository := TMigrateRepository.Create();
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInteractiveMode := False;
  FIsAutoCommit := False;
  FFilterTypeExecution := TMigrateFilterTypeExecution.TAll;

end;

procedure TMigrateRollback.setAutoCommit;
begin
  FIsAutoCommit := True;
end;

procedure TMigrateRollback.setInteractive;
begin
  FIsInteractiveMode := True;
end;

procedure TMigrateRollback.setMigrate(const Migrate: String);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByMigrate;

  FFilter := Migrate;

end;

procedure TMigrateRollback.setVersion(const version: String);
begin

  FFilterTypeExecution := TMigrateFilterTypeExecution.TByVersion;

  FFilter := version;

end;

procedure TMigrateRollback.showMessageNoneMigrateExecutedFounded;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Executed Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateRollback.showMessageNoneMigrateFounded;
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TMigrateRollback.showMessageSucessFull;
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
