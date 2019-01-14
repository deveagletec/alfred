unit Eagle.Alfred.Migrate.Service.MigrateService;

interface

uses
  Math,
  System.Classes,
  System.DateUtils,
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  System.RegularExpressions,

  Eagle.Alfred.Data,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Migrate.Model.Migrate,

  XSuperObject;

type

  TExecuteMode = (TByVersion, TByMigrate, TAll);

  IMigrateService = interface
    ['{A31CC6DC-9FE4-4FC1-9610-E476806C6D33}']
    function getMigratesByMigrationDir(): TList<TMigrate>;
    function getMigratesByVersion(const version: String): TList<TMigrate>;
    procedure removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<TMigrate>; const listMigratesExecuted: TList<String>);
    procedure createNewMigrate(const Migrate: TMigrate);
  end;

  TMigrateService = class(TInterfacedObject, IMigrateService)
  private

    FPackage: TPackage;

    procedure filterMigratesByVersion(var migrates: TList<TMigrate>; const version: String);
    function getListFilesMigrate: TList<String>;

  public
    constructor Create(const package: TPackage);
    procedure createNewMigrate(const Migrate: TMigrate);
    function getMigratesByMigrationDir: TList<TMigrate>;
    function getMigratesByVersion(const version: String): TList<TMigrate>;

    procedure removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<TMigrate>; const listMigratesExecuted: TList<String>);
  end;

implementation

constructor TMigrateService.Create(const package: TPackage);
begin
  inherited Create();
  FPackage := package;
end;

procedure TMigrateService.createNewMigrate(const Migrate: TMigrate);
var
  fileValue, fileName: String;
begin

  CreateDiretories([FPackage.MigrationDir]);

  fileValue := TJSON.Stringify(Migrate, True);

  fileName := Format('%s%s_%s.json', [FPackage.MigrationDir, Migrate.unixIdentifier, Migrate.issueIdentifier]);

  TFile.WriteAllText(fileName, fileValue);

end;

procedure TMigrateService.filterMigratesByVersion(var migrates: TList<TMigrate>; const version: String);
var
  index: Integer;
  canRemove: Boolean;
begin

  index := 0;

  while index < migrates.Count do
  begin

    canRemove := not migrates[index].version.Equals(version);

    if canRemove then
      migrates.Delete(index)
    else
      Inc(index);

  end;

end;

function TMigrateService.getListFilesMigrate: TList<String>;
var
  listFiles: TList<String>;
  search: TSearchRec;
  index: Integer;
begin

  listFiles := TList<String>.Create;

  index := FindFirst(Format('%s%s*.json', [FPackage.BaseDir, FPackage.MigrationDir]), faAnyFile, search);

  while index = 0 do
  begin

    listFiles.Add(search.Name);
    index := FindNext(search);

  end;

  Result := listFiles;

end;

function TMigrateService.getMigratesByMigrationDir(): TList<TMigrate>;
var
  listFiles: TList<String>;
  listMigrates: TList<TMigrate>;
  fileName, fileValue: String;
begin

  listFiles := getListFilesMigrate();

  try

    listFiles.Sort();

    if (listFiles.Count = 0) then
      exit(nil);

    listMigrates := TList<TMigrate>.Create();

    try

      for fileName in listFiles do
      begin

        fileValue := TFile.ReadAllText(Format('%s%s', [FPackage.MigrationDir, fileName]));
        fileValue := fileValue.Replace(#13#10, '');

        listMigrates.Add(TJSON.Parse<TMigrate>(fileValue));

      end;

    except

      on e: Exception do
        raise EJSONReadException.Create(Format('Não foi possível carregar os arquivos JSON!Erro: %s', [e.Message]));

    end;

    Result := listMigrates;

  finally

    if Assigned(listFiles) then
      listFiles.Free();

  end;

end;

function TMigrateService.getMigratesByVersion(const version: String): TList<TMigrate>;
var
  migrates: TList<TMigrate>;
begin

  migrates := getMigratesByMigrationDir();

  if Assigned(migrates) then
    filterMigratesByVersion(migrates, version);

  Result := migrates;

end;

procedure TMigrateService.removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<TMigrate>; const listMigratesExecuted: TList<String>);
var
  Migrate: TMigrate;
  canRemove: Boolean;
  index: Integer;
begin

  if listMigratesExecuted.Count <= 0 then
    exit;

  index := 0;

  while index < migrates.Count do
  begin

    Migrate := migrates[index];

    canRemove := False;

    if executionMode = TExecutionModeMigrate.TUp then
      canRemove := listMigratesExecuted.Contains(Migrate.unixIdentifier)
    else
      canRemove := not listMigratesExecuted.Contains(Migrate.unixIdentifier);

    if canRemove then
      migrates.Delete(index)
    else
      Inc(index);

  end;

end;

end.
