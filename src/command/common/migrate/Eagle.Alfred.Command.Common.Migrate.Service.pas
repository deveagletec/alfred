unit Eagle.Alfred.Command.Common.Migrate.Service;

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

  XSuperObject,

  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Core.Enums,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Command.Common.Service.ScaperJSON,
  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.Common.Migrate.Repository;

type

  TExecuteMode = (TByVersion, TByMigrate, TAll);

  IMigrateService = interface
    ['{A31CC6DC-9FE4-4FC1-9610-E476806C6D33}']
    function GetMigratesByMigrationDir(): TList<TMigrate>;
    function GetMigratesByVersion(const Version: string): TList<TMigrate>;
    function GetMigrateByIdentifier(const Identifier: string): TMigrate;
    procedure RemoveMigratesUnusableList(const ExecutionMode: TExecutionModeMigrate; var Migrates: TList<TMigrate>; const ListMigratesExecuted: TList<string>);
    procedure CreateNewMigrate(const Migrate: TMigrate);
    procedure CreateMigrateDirectory();
    function MigrateDirectoryExists: Boolean;
  end;

  TMigrateService = class(TInterfacedObject, IMigrateService)
  private

    FPackage: TPackage;
    FMigrateRepository: IMigrateRepository;

    procedure FilterMigratesByVersion(var Migrates: TList<TMigrate>; const Version: string);
    function GetListFilesMigrate: TList<string>;
    function LoadMigrate(FileName: string; ExecutedMigrates: TList<string>): TMigrate;

  public
    constructor Create(const Package: TPackage);
    procedure CreateMigrateDirectory;
    procedure CreateNewMigrate(const Migrate: TMigrate);
    function GetMigratesByMigrationDir: TList<TMigrate>;
    function GetMigratesByVersion(const Version: string): TList<TMigrate>;
    function GetMigrateByIdentifier(const Identifier: string): TMigrate;
    function MigrateDirectoryExists: Boolean;
    procedure RemoveMigratesUnusableList(const ExecutionMode: TExecutionModeMigrate; var Migrates: TList<TMigrate>; const ListMigratesExecuted: TList<string>);
  end;

implementation

constructor TMigrateService.Create(const Package: TPackage);
begin
  inherited Create();
  FPackage := package;

  FMigrateRepository := TMigrateRepository.Create(FPackage);
end;

procedure TMigrateService.CreateMigrateDirectory;
begin

  try
    CreateDiretories([FPackage.MigrationDir + '\']);
  except

    on E: Exception do
      raise EAlfredCreateDirException.CreateFmt('Not is possible create the directory %s! | Error: %s', [FPackage.MigrationDir, E.Message]);

  end;

end;

procedure TMigrateService.CreateNewMigrate(const Migrate: TMigrate);
var
  FileValue, FileName: String;
begin

  CreateMigrateDirectory();

  FileValue := TJSON.Stringify(Migrate, True);

  FileName := FPackage.MigrationDir + Migrate.Name + '.json';

  TFile.WriteAllText(FileName, FileValue);

end;

procedure TMigrateService.FilterMigratesByVersion(var Migrates: TList<TMigrate>; const Version: string);
var
  Index: Integer;
  CanRemove: Boolean;
begin

  Index := 0;

  while index < Migrates.Count do
  begin

    CanRemove := not Migrates[Index].Version.Equals(Version);

    if CanRemove then
      Migrates.Delete(index)
    else
      Inc(Index);

  end;

end;

function TMigrateService.GetListFilesMigrate: TList<string>;
var
  ListFiles: TList<String>;
  Search: TSearchRec;
  Index: Integer;
begin

  ListFiles := TList<String>.Create;

  Index := FindFirst(Format('%s%s*.json', [FPackage.BaseDir, FPackage.MigrationDir]), faAnyFile, Search);

  while Index = 0 do
  begin

    ListFiles.Add(Search.Name);
    index := FindNext(Search);

  end;

  Result := ListFiles;

end;

function TMigrateService.GetMigrateByIdentifier(const Identifier: string): TMigrate;
var
  Migrates: TList<TMigrate>;
  Migrate, MigrateResult: TMigrate;
begin

  Migrates := GetMigratesByMigrationDir();

  if (not Assigned(Migrates)) or (Migrates.Count <= 0) then
    Exit(MigrateResult);

  try

    for Migrate in Migrates do
    begin

      if not(Migrate.Id.Equals(Identifier) or Migrate.Name.Replace('.json', '').ToUpper.Equals(Identifier.ToUpper)) then
        continue;

      MigrateResult := Migrates.Extract(Migrate);
      Exit(MigrateResult);

    end;

  finally
    Migrates.Free;
  end;

  Result := MigrateResult;

end;

function TMigrateService.GetMigratesByMigrationDir: TList<TMigrate>;
var
  ListFiles: TList<string>;
  ListMigrates: TList<TMigrate>;
  FileName: string;
  ExecutedMigrates: TList<string>;
begin

  ListFiles := GetListFilesMigrate();

  try

    ListFiles.Sort();

    if ListFiles.Count = 0 then
      Exit(nil);

    ExecutedMigrates := FMigrateRepository.GetListMigratesExecuted();

    ListMigrates := TList<TMigrate>.Create();

    for FileName in ListFiles do
      ListMigrates.Add(LoadMigrate(FileName, ExecutedMigrates));

    Result := ListMigrates;

  finally

    if Assigned(ListFiles) then
      ListFiles.Free();

    if Assigned(ExecutedMigrates) then
      ExecutedMigrates.Free();

  end;

end;

function TMigrateService.GetMigratesByVersion(const Version: string): TList<TMigrate>;
var
  Migrates: TList<TMigrate>;
begin

  Migrates := GetMigratesByMigrationDir();

  if Assigned(Migrates) then
    FilterMigratesByVersion(Migrates, Version);

  Result := Migrates;

end;

function TMigrateService.LoadMigrate(FileName: string; ExecutedMigrates: TList<string>): TMigrate;
var
  Data: string;
begin

  Data := TFile.ReadAllText(FPackage.MigrationDir + FileName);

  Data := Data.Replace(#13#10, #9);

  Data := TScaperJSON.Scape(Data);

  try
    Result := TJSON.Parse<TMigrate>(Data);
    Result.Id := FileName.Split(['_'])[0];
    Result.Name := FileName;

    if Assigned(ExecutedMigrates) and (ExecutedMigrates.Count > 0) then
      Result.WasExecuted := ExecutedMigrates.Contains(Result.Id);

  except
    on E: Exception do
      raise EJSONReadException.CreateFmt('Not is possible create the files JSON! Error: %s', [E.Message]);
  end;
end;

function TMigrateService.MigrateDirectoryExists: Boolean;
begin
  Result := DirectoryExists(FPackage.MigrationDir + '\');
end;

procedure TMigrateService.RemoveMigratesUnusableList(const ExecutionMode: TExecutionModeMigrate; var Migrates: TList<TMigrate>; const ListMigratesExecuted: TList<string>);
var
  Migrate: TMigrate;
  CanRemove: Boolean;
  Index: Integer;
begin

  if ListMigratesExecuted.Count <= 0 then
    Exit;

  Index := 0;

  while Index < Migrates.Count do
  begin

    Migrate := Migrates[Index];

    CanRemove := False;

    if ExecutionMode = TExecutionModeMigrate.TUp then
      CanRemove := ListMigratesExecuted.Contains(Migrate.Id)
    else
      CanRemove := not ListMigratesExecuted.Contains(Migrate.Id);

    if CanRemove then
      Migrates.Delete(Index)
    else
      Inc(Index);

  end;

end;

end.
