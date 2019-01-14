unit Eagle.Alfred.Data;

interface
uses
  System.SysUtils,

  Eagle.Alfred.Core.Exceptions;

type

  TDatabase = class
  public
    DataBase: string;
    HostName: string;
    UserName: string;
    Password:string;
    Port: string;
  end;

  TDependency = class
    Name: string;
    Version: string;
  end;

  TPackage = class
  public
    Id: string;
    Version: string;
    DataBase: TDatabase;
    BaseDir: string;
    MigrationDir: string;
    PackagesDir: string;
    SourceDir: string;
    TestsDir: string;
    AppNamespace: string;
    Modular: Boolean;
    Dependencies: TArray<TDependency>;

    destructor Destroy; override;
    procedure Validate;
  end;

implementation

{ TPackage }

destructor TPackage.Destroy;
var
  Dependency: TDependency;
begin

  if Assigned(DataBase) then
    DataBase.Free();

  for Dependency in Dependencies do
  begin
    if Assigned(Dependency) then
      Dependency.Free;
  end;

  inherited;
end;

procedure TPackage.Validate;
begin

  if Id.IsEmpty then
    raise EPackageInvalidException.Create('Package configuration Invalid! Please enter application ID');

end;

end.
