unit Eagle.Alfred.Core.Types;

interface
uses
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions,

  XSuperObject,

  Eagle.Alfred.Core.Exceptions;

type

  TDependency = record
    Host: string;
    AuthUser: string;
    AuthPass: string;
    Repo: string;
    User: string;
    Project: string;
    Version: string;
    Name: string;
    Full : string;

    constructor Create(const Value: string);
  end;
  
  TInstalledDependency = record
    [Alias('name')]
    Name: string;
    [Alias('version')]
    Version: string;
    [Alias('paths')]
    Paths: TArray<string>;
  end;

  TDataBase = class
    [Alias('host')]
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

    [Alias('vendor-dir')]
    VendorDir: string;

    [Alias('coverage-config-dir')]
    CoverageConfigDir: string;

    [Alias('db')]
    DataBase: TDataBase;

    [Alias('dependencies')]
    Dependencies: TArray<string>;

    [Alias('dev-dependencies')]
    DevDependencies: TArray<string>;

    destructor Destroy; override;
    procedure Validate;
  end;

  TPackagelocked = class
  public
    [Alias('dependencies')]
    Dependencies: TArray<string>;

    [Alias('dev-dependencies')]
    DevDependencies: TArray<string>;
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

  DBHost := 'localhost';
  DBUser := 'sysdba';
  DBPass := 'masterkey';
  DBPort := 3050;
  CharacterSet := 'WIN1252';
end;

{ TDependency }

constructor TDependency.Create(const Value: string);
const
  REGEX = '^(\w+)(\+(\w+\:\/\/[^:]+:\d+))?:(([^;]+:[^:]+)@)?([\w-]+)\/([\w-]+)#([a-f0-9]+|latest)$';
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
  Full := Value;

  Match := TRegEx.Match(Value, REGEX);

  if not Match.Success then
    raise EDependencyDefinitionException.Create('Invalid dependency definition pattern');

  Group := Match.Groups.Item[GROUP_REPO];
  Repo := Group.Value;

  Group := Match.Groups.Item[GROUP_HOST];
  Host := Group.Value;

  Group := Match.Groups.Item[GROUP_AUTH];
  Auth := Group.Value.Split([':']);

  Group := Match.Groups.Item[GROUP_USER];
  User := Group.Value;

  Group := Match.Groups.Item[GROUP_PROJECT];
  Project := Group.Value;

  if Match.Groups.Count < 8 then
  begin
    Repo := 'latest';
    Exit;
  end;

  Group := Match.Groups.Item[GROUP_VERSION];
  Version := IfThen(Group.Success, Group.Value, 'latest');

  Name := User + '/' + Project;

  if Length(Auth) < 2 then
    Exit;

  AuthUser := Auth[0];
  AuthPass := Auth[1];
end;

end.
