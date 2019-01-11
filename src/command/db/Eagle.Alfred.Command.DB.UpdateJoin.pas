unit Eagle.Alfred.Command.DB.UpdateJoin;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,

  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,

  Eagle.Alfred.Update.Service.UpdateService;

type

  [Command('db:update', 'join', 'Realiza a união de migrates')]
  TUpdateJoin = class(TCommandAbstract)
  private

    FCurrentVersion: String;
    FDestinyVersion: String;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    FScripts: TStringList;

    procedure joinMigrates;
    procedure mountBody;
    procedure mountFooter;
    procedure mountHeader;
    procedure saveFileUpdate;
    procedure showMessageFounded(conflictsMigrates: TDictionary < String, TList < String >> );
    procedure showMessageMigratesNotFounded;
    procedure showMessageSucessFull;

  public
    destructor Destroy; override;

    procedure execute; override;
    procedure init; override;

    [ParamAttribute(1, 'Versão atual')]
    procedure setCurrentVersion(const version: String);

    [ParamAttribute(2, 'Versão a ser gerada')]
    procedure setDestinyVersion(const version: String);

  end;

implementation

destructor TUpdateJoin.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  FScripts.Free();

  inherited;
end;

procedure TUpdateJoin.execute;
var
  conflictsMigrates: TDictionary<String, TList<String>>;
begin

  FMigrates := FMigrateService.getMigratesByVersion(FDestinyVersion.Trim());

  if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
  begin
    showMessageMigratesNotFounded();
    exit;
  end;

  conflictsMigrates := FUpdateService.getConflictMigrates(FMigrates);

  try

    if Assigned(conflictsMigrates) and (conflictsMigrates.Count > 0) then
    begin
      showMessageFounded(conflictsMigrates);
      exit;
    end;

    joinMigrates();

    saveFileUpdate();

    showMessageSucessFull();

  finally

    if Assigned(conflictsMigrates) then
      conflictsMigrates.Free();

  end;

end;

procedure TUpdateJoin.init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);
  FUpdateService := TUpdateService.Create();

  FScripts := TStringList.Create();

end;

procedure TUpdateJoin.joinMigrates;
begin

  mountHeader();

  mountBody();

  mountFooter();

end;

procedure TUpdateJoin.mountBody;
var
  Migrate: TMigrate;
  index: Integer;
  SQL: String;
begin

  for Migrate in FMigrates do
  begin

    index := 0;

    FScripts.Add('');
    FScripts.Add('/* *********************************************************************** */');
    FScripts.Add(Format('/* ************************* Migrate %s ************************** */', [Migrate.unixIdentifier]));
    FScripts.Add('/* *********************************************************************** */');
    FScripts.Add('');

    for SQL in Migrate.up do
    begin

      if FUpdateService.indexIsIgnored(index, Migrate) then
        continue;

      FScripts.Add(SQL);

      Inc(index);

    end;

  end;

end;

procedure TUpdateJoin.mountFooter;
begin

  FScripts.Add('/*');
  FScripts.Add('');
  FScripts.Add('    FINALIZANDO ...');
  FScripts.Add('');
  FScripts.Add('*/');
  FScripts.Add('');
  FScripts.Add('EXECUTE PROCEDURE UTIL_ACERTA_GENERATORS;');
  FScripts.Add('');
  FScripts.Add('COMMIT WORK;');
  FScripts.Add('');
  FScripts.Add('EXECUTE PROCEDURE UTIL_METADATAINFO;');
  FScripts.Add('');
  FScripts.Add('COMMIT WORK;');
  FScripts.Add('');
  FScripts.Add(Format('UPDATE VERSAO A SET A.VERSAO = '#39'%s'#39';', [FDestinyVersion]));
  FScripts.Add('');
  FScripts.Add('COMMIT WORK;');
  FScripts.Add('');
  FScripts.Add('EXECUTE PROCEDURE UTIL_RECOMPILE_VIEWS;');
  FScripts.Add('');
  FScripts.Add('COMMIT WORK;');

end;

procedure TUpdateJoin.mountHeader;
begin

  FScripts.Add('/*');
  FScripts.Add('');
  FScripts.Add('SCRIPT DE ATUALIZACAO OU CORRECAO DE BUGS');
  FScripts.Add('');
  FScripts.Add(Format('VERSAO DE ORIGEM ....: %s', [FCurrentVersion]));
  FScripts.Add(Format('VERSAO DE DESTINO ...: %s', [FDestinyVersion]));
  FScripts.Add('SOFTWARE ............: EAGLE ERP');
  FScripts.Add(Format('DATA ................: %s', [DateTimeToStr(Now)]));
  FScripts.Add('');
  FScripts.Add('*/');
  FScripts.Add('');
  FScripts.Add('');
  FScripts.Add('/*');
  FScripts.Add('    VERIFICAR A VERSAO ATUAL DO BANCO DE DADOS');
  FScripts.Add('    A VERSAO ATUAL DEVE SER VERSAO_ANTERIOR');
  FScripts.Add('*/');
  FScripts.Add('');
  FScripts.Add('SET TERM ^ ;');
  FScripts.Add('');
  FScripts.Add('EXECUTE BLOCK');
  FScripts.Add('AS');
  FScripts.Add('    DECLARE VARIABLE VERSAO VARCHAR(10);');
  FScripts.Add('BEGIN');
  FScripts.Add('');
  FScripts.Add('   /* OBTER A VERSAO ATUAL */');
  FScripts.Add('');
  FScripts.Add('    SELECT FIRST 1');
  FScripts.Add('        A.VERSAO');
  FScripts.Add('    FROM');
  FScripts.Add('        VERSAO A');
  FScripts.Add('    INTO');
  FScripts.Add('        :VERSAO;');
  FScripts.Add('');
  FScripts.Add('    VERSAO = COALESCE (:VERSAO, '#39'NULL'#39' );');
  FScripts.Add('');
  FScripts.Add('    /* A EXECUCAO DO SCRIPT NAO PODE CONTINUAR CASO A VERSAO SEJA DIFERENTE! */ ');
  FScripts.Add('');
  FScripts.Add(Format('    IF (:VERSAO <> '#39'%s'#39' ) THEN', [FCurrentVersion]));
  FScripts.Add('    BEGIN');
  FScripts.Add(Format('        EXCEPTION EAGLEEXCEPTION '#39'@A versao do Banco de Dados deve ser %s. A versao atual e '#39' || :VERSAO || '#39'!@'#39';', [FCurrentVersion]));
  FScripts.Add('    END');
  FScripts.Add('');
  FScripts.Add('    EXIT;');
  FScripts.Add('');
  FScripts.Add('END ^');
  FScripts.Add('');
  FScripts.Add('SET TERM ; ^');
  FScripts.Add('');
  FScripts.Add('');
  FScripts.Add('/*');
  FScripts.Add('   ANTES DE INICIAR O PROCESSO, MARCAR VERSAO COMO ATUALIZANDO...');
  FScripts.Add('   ISTO E IMPORTANTE PARA QUE O SISTEMA NAO INICIE CASO OCORRA ALGUMA FALHA NA ATUALIZACAO.');
  FScripts.Add('*/');
  FScripts.Add('');
  FScripts.Add('UPDATE');
  FScripts.Add('    VERSAO A');
  FScripts.Add('SET');
  FScripts.Add('    A.VERSAO = '#39'ATUALIZANDO'#39';');
  FScripts.Add('');
  FScripts.Add('COMMIT WORK;');

end;

procedure TUpdateJoin.saveFileUpdate;
var
  path, fileName: String;
begin

  path := Format('%s%s%s', [FPackage.BaseDir, FPackage.MigrationDir, 'Updates\']);

  CreateDiretories([path]);

  fileName := Format('update_%s', [FDestinyVersion]);


  FScripts.SaveToFile(Format('%s%s%s', [path, fileName, '.sql']));

end;

procedure TUpdateJoin.setCurrentVersion(const version: String);
begin
  FCurrentVersion := version;
end;

procedure TUpdateJoin.setDestinyVersion(const version: String);
begin
  FDestinyVersion := version;
end;

procedure TUpdateJoin.showMessageFounded(conflictsMigrates: TDictionary < String, TList < String >> );
var
  key, listMigrates: String;
  conflictMigrateList: TList<String>;
begin

  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError('| Conflicts founded :( ');
  FConsoleIO.WriteError('* ----------------------------------------------------- ');

  try

    for key in conflictsMigrates.Keys do
    begin

      conflictsMigrates.TryGetValue(key, conflictMigrateList);

      if not conflictsMigrates.Count > 1 then
        continue;

      listMigrates := String.Join(', ', conflictMigrateList.ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [key, listMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    if Assigned(conflictMigrateList) then
      conflictMigrateList.Free();

  end;

end;

procedure TUpdateJoin.showMessageMigratesNotFounded;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateJoin.showMessageSucessFull;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteSucess('* ------- ');
  FConsoleIO.WriteSucess('| File Update Created Sucessfull! ');
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TUpdateJoin);

end.
