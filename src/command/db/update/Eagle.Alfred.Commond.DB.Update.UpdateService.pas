unit Eagle.Alfred.Commond.DB.Update.UpdateService;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,

  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.DB.Common.Driver;

type

  IUpdateService = interface
    function getConflictMigrates(const migrates: TList<TMigrate>): TDictionary<String, TList<String>>;
    function indexIsIgnored(const indexCurrent: Integer; const Migrate: TMigrate): Boolean;
  end;

  TUpdateService = class(TInterfacedObject, IUpdateService)
  private
    FDriver: TDriver;
    function getObjectNameBySQL(const SQL: String): String;
    procedure removeRegisterInvalid(var conflictMigrates: TDictionary < String, TList < String >> );
  public
    constructor Create(const DriverName: String);
    function indexIsIgnored(const indexCurrent: Integer; const Migrate: TMigrate): Boolean;
    function getConflictMigrates(const migrates: TList<TMigrate>): TDictionary<String, TList<String>>;
  end;

const
  REGULAR_EXPRESSION_FIND_CONFLICTS_MIGRATES_FB = 'CREATE( OR ALTER)?\s+((TRIGGER|PROCEDURE|TABLE|VIEW|GENERATOR|INDEX)\s+(\w+))';
  REGULAR_EXPRESSION_FIND_CONFLICTS_MIGRATES_PG = 'CREATE( OR REPLACE)?\s+(\w+\.)?((TRIGGER|PROCEDURE|FUNCTION|TABLE|VIEW|GENERATOR|INDEX)( IF NOT EXISTS)?\s+(\w+))';

implementation

constructor TUpdateService.Create(const DriverName: String);
begin
  FDriver := TDriver.ConstruirDriver(DriverName);
end;

function TUpdateService.getConflictMigrates(const migrates: TList<TMigrate>): TDictionary<String, TList<String>>;
var
  conflictsMigrate: TDictionary<String, TList<String>>;
  SQL, objectName: String;
  indexMigrate, indexSQL: Integer;
  listConflicts: TList<String>;
begin

  conflictsMigrate := TDictionary < String, TList < String >>.Create();

  for indexMigrate := 0 to migrates.Count - 1 do
  begin

    indexSQL := 0;

    for SQL in Migrates[indexMigrate].up do
    begin

      objectName := EmptyStr;

      if indexIsIgnored(indexSQL, Migrates[indexMigrate]) then
      begin
        Inc(indexSQL);
        continue;
      end;

      objectName := getObjectNameBySQL(SQL);

      if (objectName.Trim().IsEmpty) then
      begin
        Inc(indexSQL);
        continue;
      end;

      conflictsMigrate.TryGetValue(objectName, listConflicts);

      if not Assigned(listConflicts) then
        listConflicts := TList<String>.Create();

      listConflicts.Add(Migrates[indexMigrate].Id);

      conflictsMigrate.AddOrSetValue(objectName, listConflicts);

      Inc(indexSQL);

    end;

  end;

  removeRegisterInvalid(conflictsMigrate);

  Result := conflictsMigrate;

end;

function TUpdateService.getObjectNameBySQL(const SQL: String): String;
var
  validator: TRegEx;
  objectName: String;
  match: TMatch;
begin
  case FDriver of
    FIREBIRD: validator := TRegEx.Create(REGULAR_EXPRESSION_FIND_CONFLICTS_MIGRATES_FB);
    POSTGRES: validator := TRegEx.Create(REGULAR_EXPRESSION_FIND_CONFLICTS_MIGRATES_PG);
  end;

  match := validator.match(SQL);

  if not match.Success then
    exit;

  objectName := match.Groups[3].Value + ':' + match.Groups[4].Value;

  Result := objectName;

end;

function TUpdateService.indexIsIgnored(const indexCurrent: Integer; const Migrate: TMigrate): Boolean;
var
  Value: Integer;
begin

  Result := False;

  for Value in Migrate.ignoredScripts do
    if Value = indexCurrent then
      exit(True);

end;

procedure TUpdateService.removeRegisterInvalid(var conflictMigrates: TDictionary < String, TList < String >> );
var
  key: String;
  conflictMigratesList: TList<String>;
  Keys: TArray<string>;
begin

  Keys := conflictMigrates.Keys.ToArray;

  for key in Keys do
  begin

    conflictMigrates.TryGetValue(key, conflictMigratesList);

    if conflictMigratesList.Count > 1 then
      continue;

    conflictMigrates.Items[key].Free();
    conflictMigrates.Remove(key);

  end;

end;

end.
