unit Eagle.Alfred.Command.DB.MigrateExecute;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Enums,

  Eagle.Alfred.Commom.Utils.IOUtils,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository;

type

  [Command('DB:Migrate', 'execute', 'Realiza a execução dos migrates no banco de dados')]
  TMigrateExecute = class(TCommandAbstract)
  private

    FInteractiveMode: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;

    FMigrates: TList<IMigrate>;

    procedure checkFilesEncodedUFT8;

  public

    procedure execute; override;

    [ParamAttribute('version', 'Filtro de versão')]
    procedure setVersion(const version: String);

    [ParamAttribute('migrate', 'Migrate limite de execução')]
    procedure setMigrate(const Migrate: String);

  end;

const
  SIM = 'S';

implementation

procedure TMigrateExecute.checkFilesEncodedUFT8;
var
  filesEncodedUTF8: TList<String>;
  Migrate: IMigrate;
  fileName, fileEncodedUTF8, answer: String;
begin

  filesEncodedUTF8 := TList<String>.Create();

  for Migrate in FMigrates do
  begin

    fileName := Format('%s%s%s%s%s%s', [FPackage.BaseDir, FPackage.MigrationDir, Migrate.UnixIdentifier, '_', Migrate.IssueIdentifier, '.json']);

    if TIOUtils.FileIsEncodedUTF8(fileName) then
      filesEncodedUTF8.Add(Format('%s%s%s', [Migrate.UnixIdentifier, '_', Migrate.IssueIdentifier]));

  end;

  if filesEncodedUTF8.Count <= 0 then
    exit;

  FConsoleIO.WriteInfo('Foram detectados arquivos codificados em UTF-8. Podendo gerar corrupção a dos metadatas do banco.');
  FConsoleIO.WriteInfo('Arquivos codificados em UTF-8:');
  FConsoleIO.WriteInfo('---------------------');

  for fileEncodedUTF8 in filesEncodedUTF8 do
    FConsoleIO.WriteInfo(' + ' + fileEncodedUTF8);

  FConsoleIO.WriteInfo('---------------------');
  answer := FConsoleIO.ReadInfo('Deseja continuar? <S>im | <N>ao');

  if not answer.ToUpper.Equals(SIM) then
    abort;

end;

procedure TMigrateExecute.execute;
var
  MigrateService: IMigrateService;
  MigrateRepository: IMigrateRepository;
  Migrate: IMigrate;
  answer, lastScriptExecuted: string;
  canExecute: Boolean;
begin

  MigrateService := TMigrateService.Create();
  MigrateRepository := TMigrateRepository.Create();

  lastScriptExecuted := MigrateRepository.getLastScriptExecuted();

  FMigrates := MigrateService.getMigratesByMigrationDir(TExecutionModeMigrate.TUp);

  MigrateService.removeMigratesUnusableList(TExecutionModeMigrate.TUp, FMigrates, lastScriptExecuted);

  if FMigrates.Count = 0 then
    exit(True);

  checkFilesEncodedUFT8();

  for Migrate in FMigrates do
  begin

    if FInteractiveMode then
    begin

      answer := FConsoleIO.ReadInfo(Format('Deseja executar o arquivo %s? <S>im | <N>ao', [Migrate.IssueIdentifier]));

      if not answer.ToUpper.Equals(SIM) then
        exit(True);

    end;

    canExecute := True;

    if FFilterTypeExecution = TMigrateFilterTypeExecution.TAll then
      canExecute := True
    else if FFilterTypeExecution = TMigrateFilterTypeExecution.TByVersion then
      canExecute := Migrate.version.Equals(FFilter)
    else
    begin
      canExecute := Migrate.IssueIdentifier <= FFilter;

      if not canExecute then
        exit;

    end;

    if canExecute then
      MigrateRepository.executeFileScript(Migrate, TExecutionModeMigrate.TUp);

  end;

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
