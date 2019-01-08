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

  Eagle.Alfred.Data,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Migrate.Model.Migrate,

  XSuperObject;

type

  migrateWrapper = record
    Up: TArray<String>;
  end;

  TExecuteMode = (TByVersion, TByMigrate, TAll);

  IMigrateService = interface
    ['{A31CC6DC-9FE4-4FC1-9610-E476806C6D33}']
    function getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<TMigrate>;
    procedure removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<TMigrate>; const listMigratesExecuted: TList<String>);
  end;

  TMigrateService = class(TInterfacedObject, IMigrateService)
  private

    FPackage: TPackage;

    function getListFilesMigrate: TList<String>;

  public
    constructor Create(const package: TPackage);
    function getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<TMigrate>;
    procedure removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<TMigrate>; const listMigratesExecuted: TList<String>);
  end;

implementation

constructor TMigrateService.Create(const package: TPackage);
begin
  inherited Create();
  FPackage := package;
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

function TMigrateService.getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<TMigrate>;
var
  listFiles: TList<String>;
  search: TSearchRec;
  index: Integer;
  listMigrates: TList<TMigrate>;
  Migrate: TMigrate;
  fileName, fileValue: String;
  teste: TArray<String>;
  teste2: String;
  json: ISuperObject;
  wrapper: migrateWrapper;
begin

  listFiles := getListFilesMigrate();

  try

    listFiles.Sort();

    if listFiles.Count = 0 then
      exit(nil);

    listMigrates := TList<TMigrate>.Create();

    for fileName in listFiles do
    begin

      fileValue := TFile.ReadAllText(Format('%s%s%s', [FPackage.BaseDir, FPackage.MigrationDir, fileName]));

      wrapper := TJson.Parse<migrateWrapper>(fileValue);

      Migrate := TJson.Parse<TMigrate>(fileValue);

      teste := Migrate.Up;

      listMigrates.Add(Migrate);

    end;

    Result := listMigrates;

  finally

    if Assigned(listFiles) then
      listFiles.Free();

  end;

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
      canRemove := listMigratesExecuted.Contains(Migrate.UnixIdentifier)
    else
      canRemove := not listMigratesExecuted.Contains(Migrate.UnixIdentifier);

    if canRemove then
      migrates.Remove(Migrate)
    else
      Inc(index);

  end;

end;

end.
