unit Eagle.Alfred.Migrate.Model.Migrate;

interface

type
  TMigrate = record

    issueIdentifier: String;
    unixIdentifier: String;
    version: String;
    description: String;
    responsible: String;
    ignoredScripts: TArray<Integer>;
    up: TArray<String>;
    down: TArray<String>;

  end;

implementation

end.
