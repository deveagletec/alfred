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

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,

  Eagle.Alfred.Commond.DB.Update.UpdateService;

type

  [Command('db:update', 'join', 'Join migrates by version')]
  TUpdateJoin = class(TCommandAbstract)
  private

    FCurrentVersion: String;
    FDestinyVersion: String;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    FScripts: TStringList;

    procedure JoinMigrates;
    procedure MountBody;
    procedure MountFooter;
    procedure MountHeader;
    procedure SaveFileUpdate;
    function InsertBreakLine(const Value: string): string;
    procedure ShowMessageFounded(ConflictsMigrates: TDictionary<string,TList<string >> );
    procedure ShowMessageMigratesNotFounded;
    procedure ShowMessageSucessFull;

  public
    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute(1, 'Current version')]
    procedure SetCurrentVersion(const Version: string);

    [ParamAttribute(2, 'Destiny version')]
    procedure SetDestinyVersion(const Version: string);

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
var
  ConflictsMigrates: TDictionary<String, TList<String>>;
begin

  FMigrates := FMigrateService.GetMigratesByVersion(FDestinyVersion.Trim());

  if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
  begin
    ShowMessageMigratesNotFounded();
    Exit;
  end;

  ConflictsMigrates := FUpdateService.getConflictMigrates(FMigrates);

  try

    if Assigned(ConflictsMigrates) and (ConflictsMigrates.Count > 0) then
    begin
      ShowMessageFounded(ConflictsMigrates);
      Exit;
    end;

    JoinMigrates();

    SaveFileUpdate();

    ShowMessageSucessFull();

  finally

    if Assigned(conflictsMigrates) then
      ConflictsMigrates.Free();

  end;

end;

procedure TUpdateJoin.Init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);
  FUpdateService := TUpdateService.Create();

  FScripts := TStringList.Create();

end;

function TUpdateJoin.InsertBreakLine(const Value: string): string;
var
  Char, PreviousChar: string;
  NewValue: string;
  I: Integer;
begin

  NewValue := Value;
  I := 1;

  while I < NewValue.Length do
  begin

    Char := NewValue[I];
    PreviousChar := NewValue[I - 1];

    if Char.Equals(#9) and (not PreviousChar.Equals(#10)) and (not PreviousChar.Equals(#9)) then
      NewValue.Insert(I - 1, #10)
    else
      Inc(I);

  end;

  NewValue := NewValue.Replace(#10#9, #10);

  Result := NewValue;

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
        continue;

      SQLReplaced := InsertBreakLine(SQL);

      FScripts.Add(SQLReplaced);
      FScripts.Add(#10'/* *********************************************************************** */'#10);

      Inc(Index);

    end;

    FScripts.Add('');
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
  Path, FileName: String;
begin

  Path := Format('%s%s', [FPackage.MigrationDir, 'Updates\']);

  CreateDiretories([path]);

  FileName := Format('update_%s', [FDestinyVersion]);


  FScripts.SaveToFile(Format('%s%s%s', [Path, FileName, '.sql']));

end;

procedure TUpdateJoin.SetCurrentVersion(const Version: string);
begin
  FCurrentVersion := Version;
end;

procedure TUpdateJoin.SetDestinyVersion(const Version: string);
begin
  FDestinyVersion := version;
end;

procedure TUpdateJoin.ShowMessageFounded(ConflictsMigrates: TDictionary<string, TList<string >>);
var
  Key, ListMigrates: String;
  ConflictMigrateList: TList<String>;
begin

  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError('| Conflicts founded :( ');
  FConsoleIO.WriteError('* ----------------------------------------------------- ');

  try

    for Key in ConflictsMigrates.Keys do
    begin

      ConflictsMigrates.TryGetValue(key, ConflictMigrateList);

      if not conflictsMigrates.Count > 1 then
        continue;

      ListMigrates := String.Join(', ', ConflictMigrateList.ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [Key, listMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    if Assigned(ConflictMigrateList) then
      ConflictMigrateList.Free();

  end;

end;

procedure TUpdateJoin.ShowMessageMigratesNotFounded;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateJoin.ShowMessageSucessFull;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteSuccess('* ------- ');
  FConsoleIO.WriteSuccess('| File Update Created Successful! ');
  FConsoleIO.WriteSuccess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

initialization

TAlfred.GetInstance.Register(TUpdateJoin);

end.
