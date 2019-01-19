unit Eagle.Alfred.Core.Types;

interface
uses
  System.SysUtils,

  XSuperObject,

  Eagle.Alfred.Core.Exceptions;

type

  TDependency = class
    Name: string;
    Version: string;
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

    [Alias('db')]
    DataBase: TDataBase;

    [Alias('dependencies')]
    Dependencies: TArray<TDependency>;

    [Alias('dependencies-dev')]
    DependenciesDev: TArray<TDependency>;

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

  for Dependency in Dependencies do
  begin
    if Assigned(Dependency) then
      Dependency.Free;
  end;

  for Dependency in DependenciesDev do
  begin
    if Assigned(Dependency) then
      Dependency.Free;
  end;

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
  Namespace := '';
  Version := '1.0.0';
  Modular := False;

  DBHost := 'localhost';
  DBUser := 'sysdba';
  DBPass := 'masterkey';
  DBPort := 3050;
end;

end.
