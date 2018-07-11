unit Eagle.Alfred.Data;

interface

type

  TDependency = record
    Name: string;
    Version: string;
  end;

  TPackage = record
    Version: string;
    DataBase: string;
    MigrationDir: string;
    PackagesDir: string;
    SourceDir: string;
    TestsDir: string;
    Dependencies: TArray<TDependency>;
  end;

implementation

end.
