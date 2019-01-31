unit Eagle.Alfred.Core.Types;

interface
uses
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions,

  XSuperObject,

  Eagle.Alfred.Core.Exceptions;

type

  TDependency = class
    Name: string;
    Version: string;
    Repo : string;
    SrcDir : string;

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

    [Alias('db')]
    DataBase: TDataBase;

    [Alias('dependencies')]
    Dependencies: TArray<string>;

    [Alias('dev-dependencies')]
    DevDependencies: TArray<string>;

    destructor Destroy; override;
    procedure Validate;
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
var
  Dependency: TDependency;
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
end;

{ TDependency }

constructor TDependency.Create(const Value: string);
var
  Match: TMatch;
  Group: TGroup;
begin
  Match := TRegEx.Match(Value, '^((\w+):)?([\w\/-]+)(#([a-f0-9]+))?$');

  if not Match.Success then
    raise Exception.Create('Error Message');

  Group := Match.Groups.Item[2];
  Repo := IfThen(Group.Success, Group.Value, 'github');
  Group := Match.Groups.Item[3];
  Name := Group.Value;
  if Match.Groups.Count < 5 then
  begin
    Repo := 'lasted';
    Exit;
  end;

  Group := Match.Groups.Item[5];
  Repo := IfThen(Group.Success, Group.Value, 'lasted');
end;

end.
