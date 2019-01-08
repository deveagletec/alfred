unit Eagle.Alfred.Migrate.Model.Migrate;

interface

type
  TMigrate = record
  public

    IssueIdentifier: String;
    UnixIdentifier: String;
    Version: String;
    Description: String;
    Responsible: String;
    IgnoredScripts: TArray<Integer>;
    Up: TArray<String>;
    Down: TArray<String>;

  end;

implementation

end.
