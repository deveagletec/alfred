unit Eagle.Alfred.Command.DB.Update.Join;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,

  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.Common.Migrate.Service,

  Eagle.Alfred.Commond.DB.Update.UpdateService;

type

  [Command('db:update', 'join', 'Join migrates by version')]
  TUpdateJoin = class(TCommandAbstract)
  private

    FCurrentVersion: String;
    FDestinyVersion: String;
    FForceGeneration: Boolean;
    FSaveFileWithPath: Boolean;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    FScripts: TStringList;

    procedure JoinMigrates;
    procedure MountBody;
    procedure MountFooter;
    procedure MountHeader;
    procedure SaveFileUpdate;
    procedure SavePathInFile(const FullFileName: string);
    procedure ShowMessageFounded(ConflictsMigrates: TDictionary < string, TList < string >> );
    procedure ShowMessageMigratesNotFounded;
    procedure ShowMessageSucessFull;
    procedure ValidateExistsMigrates;
    procedure VerifyConflicts();

  public
    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute(1, 'Current version')]
    procedure SetCurrentVersion(const Version: string);

    [ParamAttribute(2, 'Destiny version')]
    procedure SetDestinyVersion(const Version: string);

    [Option('force', 'f', 'Force generation of script update even without migrates founded')]
    procedure SetForceGeneration;

    [Option('save', 's', 'Save path in file on current diretory')]
    procedure SetOptionSaveFileWithPath;

  end;

implementation

destructor TUpdateJoin.Destroy;
begin
  if Assigned(FMigrates) then
    FMigrates.Free();

  FScripts.Free();

  inherited;
end;

procedure TUpdateJoin.Execute;
begin
  FMigrates := FMigrateService.GetMigratesByVersion(FDestinyVersion.Trim());

  ValidateExistsMigrates();

  VerifyConflicts();

  JoinMigrates();

  SaveFileUpdate();

  ShowMessageSucessFull();
end;

procedure TUpdateJoin.Init;
begin
  inherited;
  FMigrateService := TMigrateService.Create(FPackage);
  FUpdateService := TUpdateService.Create();

  FScripts := TStringList.Create();

  FForceGeneration := False;
  FSaveFileWithPath := False;
end;

procedure TUpdateJoin.JoinMigrates;
begin

  MountHeader();

  MountBody();

  MountFooter();

end;

procedure TUpdateJoin.MountBody;
var
  Migrate: TMigrate;
  Index: Integer;
  SQL, SQLReplaced: String;
begin

  if not Assigned(FMigrates) or (FMigrates.Count <= 0) then
    Exit;

  for Migrate in FMigrates do
  begin

    Index := 0;

    FScripts.Add('');
    FScripts.Add('/* *********************************************************************** */');
    FScripts.Add(Format('/* ************************* Migrate %s ************************** */', [Migrate.Id]));
    FScripts.Add('/* *********************************************************************** */');
    FScripts.Add('');

    for SQL in Migrate.Up do
    begin

      if FUpdateService.IndexIsIgnored(Index, Migrate) then
      begin
        Inc(Index);
        continue;
      end;

      SQLReplaced := SQL.Replace(#9, #13#10);

      FScripts.Add(SQLReplaced);
      FScripts.Add(#10'/* *********************************************************************** */'#10);

      Inc(Index);

    end;

    FScripts.Add('');
    FScripts.Add(Format('INSERT INTO MIGRATIONS VALUES (%s);', [Migrate.Id.QuotedString]));
    FScripts.Add('COMMIT WORK;');
    FScripts.Add('');

  end;

end;

procedure TUpdateJoin.MountFooter;
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

procedure TUpdateJoin.MountHeader;
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

procedure TUpdateJoin.SaveFileUpdate;
var
  FileName, FullFileName: String;
begin

  CreateDiretories([FPackage.UpdateScriptDir]);

  FileName := Format('update_%s', [FDestinyVersion]);

  FullFileName := Format('%s%s%s', [FPackage.UpdateScriptDir, FileName, '.sql']);

  FScripts.SaveToFile(FullFileName);

  if FSaveFileWithPath then
    SavePathInFile(FullFileName);

end;

procedure TUpdateJoin.SavePathInFile(const FullFileName: string);
var
  Data: TStringList;
begin

  Data := TStringList.Create;

  try

    Data.Add(FullFileName);

    Data.SaveToFile('FullFileNameUpdateScript.txt');

  finally
    Data.Free;
  end;

end;

procedure TUpdateJoin.SetCurrentVersion(const Version: string);
begin
  FCurrentVersion := Version;
end;

procedure TUpdateJoin.SetDestinyVersion(const Version: string);
begin
  FDestinyVersion := Version;
end;

procedure TUpdateJoin.SetForceGeneration;
begin
  FForceGeneration := True;
end;

procedure TUpdateJoin.SetOptionSaveFileWithPath;
begin
  FSaveFileWithPath := True;
end;

procedure TUpdateJoin.ShowMessageFounded(ConflictsMigrates: TDictionary < string, TList < string >> );
var
  Key, ListMigrates: String;
begin

  FConsoleIO.NewEmptyLine;
  FConsoleIO.WriteAlert('* ------- ');
  FConsoleIO.WriteAlert('| Conflicts founded :( ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');

  try

    for Key in ConflictsMigrates.Keys do
    begin

      ListMigrates := String.Join(', ', ConflictsMigrates.Items[Key].ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [Key, ListMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    for Key in ConflictsMigrates.Keys do
      ConflictsMigrates.Items[Key].Free;

  end;

end;

procedure TUpdateJoin.ShowMessageMigratesNotFounded;
begin
  FConsoleIO.NewEmptyLine;
  FConsoleIO.WriteAlert('* ------- ');
  FConsoleIO.WriteAlert('| None Migrate Founded! ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');
  FConsoleIO.NewEmptyLine;
end;

procedure TUpdateJoin.ShowMessageSucessFull;
begin
  DoShowMessageSuccessful('File Update Created Successful!');
end;

procedure TUpdateJoin.ValidateExistsMigrates;
begin

  if FForceGeneration then
    Exit;

  if Assigned(FMigrates) and (FMigrates.Count > 0) then
    Exit;

  ShowMessageMigratesNotFounded();
  Abort;

end;

procedure TUpdateJoin.VerifyConflicts();
var
  ConflictsMigrates: TDictionary<String, TList<String>>;
begin

  if not Assigned(FMigrates) or (FMigrates.Count <= 0) then
    Exit;

  ConflictsMigrates := FUpdateService.getConflictMigrates(FMigrates);

  try

    if Assigned(ConflictsMigrates) and (ConflictsMigrates.Count > 0) then
    begin
      ShowMessageFounded(ConflictsMigrates);
      Abort;
    end;

  finally

    if Assigned(ConflictsMigrates) then
      ConflictsMigrates.Free();

  end;

end;

initialization

TAlfred.GetInstance.Register(TUpdateJoin);

end.
