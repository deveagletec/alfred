unit Eagle.Alfred.Data;

interface

type

  TDependency = class
    Name: string;
    Version: string;
  end;

  TPackage = class
  public
    Id: string;
    Version: string;
    DataBase: string;
    BaseDir: string;
    MigrationDir: string;
    PackagesDir: string;
    SourceDir: string;
    TestsDir: string;
    AppNamespace: string;
    Modular: Boolean;
    Dependencies: TArray<TDependency>;

    destructor Destroy; override;
  end;

implementation

{ TPackage }

destructor TPackage.Destroy;
var
  Dependency: TDependency;
begin

  for Dependency in Dependencies do
  begin
    if Assigned(Dependency) then
      Dependency.Free;
  end;

  inherited;
end;

end.
