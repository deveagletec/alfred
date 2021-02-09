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
    procedure LoadByTemplate(const TemplateName: string);
    procedure MountBody;
    procedure MountFooter;
    procedure MountHeader;
    procedure SaveFileUpdate;
    procedure SavePathInFile(const FullFileName: string);
    function SetVariablesValue(const Value: string): string;
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
  FUpdateService := TUpdateService.Create(FPackage.DataBase.Driver);

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
const
  TEMPLATE_NAME='template_footer_file_merge_migrates';
begin
  LoadByTemplate(TEMPLATE_NAME);
end;

procedure TUpdateJoin.MountHeader;
const
  TEMPLATE_NAME='template_header_file_merge_migrates';
begin
  LoadByTemplate(TEMPLATE_NAME);
end;

procedure TUpdateJoin.LoadByTemplate(const TemplateName: string);
var
  FileName: string;
  Footer: TStringList;
  FooterValue: string;
begin

  if not DirectoryExists(FPackage.ConfigDir) then
    Exit;

  FileName := Format('%s%s.txt', [FPackage.ConfigDir, TemplateName]);

  if not FileExists(FileName) then
    Exit;

  Footer := TStringList.Create();

  try
    Footer.LoadFromFile(FileName);
    FooterValue := Footer.Text;
  finally
    Footer.Free();
  end;

  FooterValue := SetVariablesValue(FooterValue);

  FScripts.Add(FooterValue);
end;

function TUpdateJoin.SetVariablesValue(const Value: string): string;
var
  ProcessedValue: string;
begin

  ProcessedValue := Value;

  ProcessedValue := ProcessedValue.Replace('{old_version}', FCurrentVersion);
  ProcessedValue := ProcessedValue.Replace('{new_version}', FDestinyVersion);
  ProcessedValue := ProcessedValue.Replace('{current_date}', DateTimeToStr(Now));

  Result := ProcessedValue;

end;

procedure TUpdateJoin.SaveFileUpdate;
var
  FileName, FullFileName: String;
begin

  CreateDiretories([FPackage.UpdateScriptDir]);

  FileName := Format('update%s', [FDestinyVersion.Replace('.', '')]);

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
