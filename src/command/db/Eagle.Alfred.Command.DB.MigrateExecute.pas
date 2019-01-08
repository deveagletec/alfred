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

  Eagle.Alfred.Commom.Utils.IOUtils,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository,
  Eagle.ConsoleIO;

type

  [Command('db:migrate', 'up', 'Realiza a execu��o dos migrates no banco de dados')]
  TMigrateExecute = class(TCommandAbstract)
  private

    FIsInteractiveMode: Boolean;
    FFilterTypeExecution: TMigrateFilterTypeExecution;
    FFilter: String;
    FIsAutoCommit: Boolean;

    FMigrates: TList<TMigrate>;
    FMigrateRepository: IMigrateRepository;
    FMigrateService: IMigrateService;

    procedure checkFilesEncodedUFT8;
    procedure executeMigrates;

  public

    destructor Destroy; override;

    procedure execute; override;
    procedure Init; override;

    [ParamAttribute('version', 'Filtro de vers�o', False)]
    procedure setVersion(const version: String);

    [ParamAttribute('migrate', 'Migrate limite de execu��o', False)]
    procedure setMigrate(const Migrate: String);

    [OptionAttribute('IsInteractiveMode', 'i', 'Informar se a execu��o ser� em modo interativo')]
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

procedure TMigrateExecute.checkFilesEncodedUFT8;
var
  filesEncodedUTF8: TList<String>;
  Migrate: TMigrate;
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

    FConsoleIO.WriteInfo('Foram detectados arquivos codificados em UTF-8. Podendo gerar corrup��o a dos metadatas do banco.');
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
  teste: TArray<String>;
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

      teste := migrate.Up;

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
