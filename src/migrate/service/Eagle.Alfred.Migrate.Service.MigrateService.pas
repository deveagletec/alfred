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

  TExecuteMode = (TByVersion, TByMigrate, TAll);

  IMigrateService = interface
    ['{A31CC6DC-9FE4-4FC1-9610-E476806C6D33}']
    function getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<IMigrate>;
    procedure removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<IMigrate>);
  end;

  TMigrateService = class(TInterfacedObject, IMigrateService)
  private

    FPackage: TPackage;

    function getListFilesMigrate: TList<String>;

  public
    constructor Create(const package: TPackage);
    function getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<IMigrate>;
    procedure removeMigratesUnusableList(const executionMode:
        TExecutionModeMigrate; var migrates: TList<IMigrate>; const
        lastScriptExecuted: String = '');
  end;

implementation

constructor TMigrateService.Create(const package: TPackage);
begin
  inherited;
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

function TMigrateService.getMigratesByMigrationDir(const executionMode: TExecutionModeMigrate): TList<IMigrate>;
var
  listFiles: TList<String>;
  search: TSearchRec;
  index: Integer;
  listMigrates: TList<IMigrate>;
  Migrate: IMigrate;
  fileName, fileValue: String;
begin

  listFiles := getListFilesMigrate();

  listFiles.Sort();

  if listFiles.Count = 0 then
    exit(nil);

  listMigrates := TList<IMigrate>.Create();

  for fileName in listFiles do
  begin

    fileValue := TFile.ReadAllText(Format('%s%s%s', [FPackage.BaseDir, FPackage.MigrationDir, fileName]));

    Migrate := TJson.Parse<TMigrate>(fileValue);

    listMigrates.Add(Migrate);

  end;

  Result := listMigrates;

end;

procedure TMigrateService.removeMigratesUnusableList(const executionMode: TExecutionModeMigrate; var migrates: TList<IMigrate>; const lastScriptExecuted: String = '');
var
  migrateName: String;
  indexLasTScriptExecuted, limitCount: Integer;
  migrate: IMigrate;
begin

  if lastScriptExecuted.Trim().IsEmpty then
    exit;

  for migrate in migrates do
  begin

    migrateName := Format('%%%', [migrate.UnixIdentifier, '_', migrate.IssueIdentifier]);

    if migrateName.StartsWith(lastScriptExecuted) then
      indexLasTScriptExecuted := migrates.IndexOf(migrate);

  end;

  if executionMode = TExecutionModeMigrate.TUp then
  begin

    limitCount := migrates.Count - (indexLasTScriptExecuted + 1);

    while migrates.Count > limitCount do
      migrates.Remove(migrates.First);

  end
  else
  begin

    while migrates.Count > (indexLasTScriptExecuted + 1) do
      migrates.Remove(migrates[indexLasTScriptExecuted + 1]);

  end;

end;

end.
