unit Eagle.Alfred.Core.Types;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions,

  XSuperObject,

  Eagle.Alfred.Core.Exceptions;

type

  TDependency = record [Alias('groupId')]
    GroupId: string;
    [Alias('artifactId')]
    ArtifactId: string;
    [Alias('version')]
    Version: string;
    [Alias('packageFile')]
    PackageFile: string;
    [Alias('repository')]
    Repository: string;
    [DISABLE]
    Host: string;
    [DISABLE]
    AuthUser: string;
    [DISABLE]
    AuthPass: string;
    [DISABLE]
    Repo: string;
    [DISABLE]
    CachePath: string;
    [DISABLE]
    VendorPath: string;
    [DISABLE]
    VendorPathFull: string;
    [DISABLE]
    Cached: Boolean;
    [DISABLE]
    Installed: Boolean;

    procedure Prepare;
    function Identifier(): string;
  end;

  TInstalledDependency = record [Alias('name')]
    Name: string;
    [Alias('version')]
    Version: string;
    [Alias('paths')]
    Paths: TArray<string>;
  end;

  TDataBase = class[Alias('host')]
    Host: string;
    [Alias('file')]
    &File: string;
    [Alias('user')]
    User: string;
    [Alias('pass')]
    Pass: string;
    [Alias('port')]
    Port: Integer;
    [Alias('character-set')]
    CharacterSet: string;
  end;

  TPackage = class
  public
    [Alias('name')]
    Name: string;

    [Alias('namespace')]
    Namespace: string;

    [Alias('description')]
    Description: string;

    [Alias('version')]
    Version: string;

    [Alias('modular')]
    Modular: Boolean;

    [Alias('multisolutions')]
    MultiSolutions: Boolean;

    [Alias('base-dir')]
    BaseDir: string;

    [Alias('package-dir')]
    PackagesDir: string;

    [Alias('source-dir')]
    SourceDir: string;

    [Alias('tests-dir')]
    TestsDir: string;

    [Alias('migration-dir')]
    MigrationDir: string;

    [Alias('update-script-dir')]
    UpdateScriptDir: string;

    [Alias('vendor-dir')]
    VendorDir: string;

    [Alias('config-dir')]
    ConfigDir: string;

    [Alias('coverage-config-dir')]
    CoverageConfigDir: string;

    [Alias('db')]
    DataBase: TDataBase;

    [Alias('dependencies')]
    Dependencies: TArray<TDependency>;

    [Alias('dev-dependencies')]
    DevDependencies: TArray<TDependency>;

    destructor Destroy; override;
    procedure Validate;
  end;

  TPackagelocked = class
  public
    [Alias('dependencies')]
    Dependencies: TArray<TDependency>;

    [Alias('dev-dependencies')]
    DevDependencies: TArray<TDependency>;
  end;

  TConfiguration = class
  public
    [Alias('namespace')]
    Namespace: string;

    [Alias('version')]
    Version: string;

    [Alias('modular')]
    Modular: Boolean;

    [Alias('base-dir')]
    BaseDir: string;

    [Alias('package-dir')]
    PackagesDir: string;

    [Alias('source-dir')]
    SourceDir: string;

    [Alias('tests-dir')]
    TestsDir: string;

    [Alias('migration-dir')]
    MigrationDir: string;

    [Alias('update-script-dir')]
    UpdateScriptDir: string;

    [Alias('config_dir')]
    ConfigDir: string;

    [Alias('vendor-dir')]
    VendorDir: string;

    [Alias('db-host')]
    DBHost: string;
    [Alias('db-user')]
    DBUser: string;
    [Alias('db-pass')]
    DBPass: string;
    [Alias('db-port')]
    DBPort: Integer;
    [Alias('character-set')]
    CharacterSet: string;

    [Alias('default-editor')]
    DefaultEditor: string;

    [Alias('author')]
    Author: string;

    [Alias('auto-open')]
    AutoOpen: Boolean;

    [Alias('storage-dir')]
    StorageDir: string;

    constructor Create;
  end;

implementation

{ TPackage }

destructor TPackage.Destroy;
begin

  if Assigned(DataBase) then
    DataBase.Free;

  inherited;
end;

procedure TPackage.Validate;
begin

  if Name.IsEmpty then
    raise EPackageInvalidException.Create('Package configuration Invalid! Please enter application name');

end;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  BaseDir := '.\';
  PackagesDir := 'packages\';
  SourceDir := 'src\';
  TestsDir := 'tests\';
  MigrationDir := 'migrations\';
  VendorDir := 'vendor\';
  Namespace := '';
  Version := '1.0.0';
  Modular := False;
  UpdateScriptDir := 'migrations\updates';

  DBHost := 'localhost';
  DBUser := 'sysdba';
  DBPass := 'masterkey';
  DBPort := 3050;
  CharacterSet := 'WIN1252';
end;

{ TDependency }

procedure TDependency.Prepare;
const
  REGEX = '^(\w+)((\+(\w+\:\/\/[^:]+:\d+))?:(([^;]+:[^:]+)@))?$';
  GROUP_REPO = 1;
  GROUP_HOST = 3;
  GROUP_AUTH = 5;
  GROUP_USER = 6;
  GROUP_PROJECT = 7;
  GROUP_VERSION = 8;
var
  Match: TMatch;
  Group: TGroup;
  Auth: TArray<string>;
begin
  AuthUser := EmptyStr;
  AuthPass := EmptyStr;

  Match := TRegEx.Match(Repository, REGEX);

  if not Match.Success then
    raise EDependencyDefinitionException.Create('Invalid dependency definition pattern');

  Group := Match.Groups.Item[GROUP_REPO];
  Repo := Group.Value;

  if Match.Groups.Count < 3 then
    Exit;

  Group := Match.Groups.Item[GROUP_HOST];
  Host := Group.Value;

  Group := Match.Groups.Item[GROUP_AUTH];
  Auth := Group.Value.Split([':']);

  if Length(Auth) < 2 then
    Exit;

  AuthUser := Auth[0];
  AuthPass := Auth[1];
end;

function TDependency.Identifier: string;
begin
  Result := GroupId + '/' + ArtifactId;
end;

end.
