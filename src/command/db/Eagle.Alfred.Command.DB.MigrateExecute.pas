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
  Eagle.Alfred.Migrate.Repository.MigrateRepository, Eagle.Alfred.Data,
  Eagle.ConsoleIO;

type

  [Command('db:migrate', 'execute', 'Realiza a execução dos migrates no banco de dados')]
  TMigrateExecute = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;

    FMigrates: TList<IMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure checkFilesEncodedUFT8;
    procedure executeMigrates;

  public

    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);

    procedure execute; override;
    procedure help; override;

    [ParamAttribute('version', 'Filtro de versão', False)]
    procedure setVersion(const version: String);

    [ParamAttribute('migrate', 'Migrate limite de execução', False)]
    procedure setMigrate(const Migrate: String);

    [OptionAttribute('IsInteractiveMode', 'i', 'Informar se a execução será em modo interativo')]
    procedure setInteractive;

  end;

const
  SIM = 'S';

implementation

constructor TMigrateExecute.Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  inherited Create(AppPath, APackage, ConsoleIO);

  FMigrateRepository := TMigrateRepository.Create();
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInteractiveMode := False;
  FFilterTypeExecution := TMigrateFilterTypeExecution.TAll;

end;

procedure TMigrateExecute.checkFilesEncodedUFT8;
var
  filesEncodedUTF8: TList<String>;
  Migrate: IMigrate;
  fileName, fileEncodedUTF8, answer: String;
begin

  filesEncodedUTF8 := TList<String>.Create();

  try

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

  finally
    filesEncodedUTF8.Free();
  end;

end;

procedure TMigrateExecute.execute;
var
  listMigratesExecuted: TList<String>;
begin

  FMigrateService := TMigrateService.Create(FPackage);

  listMigratesExecuted := FMigrateRepository.getListMigratesExecuted();

  try

    FMigrates := FMigrateService.getMigratesByMigrationDir(TExecutionModeMigrate.TUp);

    if (Assigned(listMigratesExecuted)) and (listMigratesExecuted.Count > 0) then
      FMigrateService.removeMigratesUnusableList(TExecutionModeMigrate.TUp, FMigrates, listMigratesExecuted);

    if FMigrates.Count = 0 then
      exit;

    checkFilesEncodedUFT8();

    executeMigrates();

  finally

    if Assigned(listMigratesExecuted) then
      listMigratesExecuted.Free();

  end;

end;

procedure TMigrateExecute.executeMigrates;
var
  Migrate: IMigrate;
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
      FMigrateRepository.executeFileScript(Migrate, TExecutionModeMigrate.TUp);

  end;

end;

procedure TMigrateExecute.help;
begin
  inherited;
  // TODO -cMM: TMigrateExecute.help default body inserted
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
