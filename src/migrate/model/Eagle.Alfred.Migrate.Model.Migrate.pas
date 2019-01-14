unit Eagle.Alfred.Migrate.Model.Migrate;

interface

uses
  XSuperObject;

type
  TMigrate = record

    [Alias('issueIdentifier')]
    IssueIdentifier: string;

    [Alias('unixIdentifier')]
    UnixIdentifier: string;

    [Alias('version')]
    Version: string;

    [Alias('description')]
    description: string;

    [Alias('responsible')]
    Responsible: string;

    [Alias('ignoredScripts')]
    IgnoredScripts: TArray<integer>;

    [Alias('up')]
    Up: TArray<string>;

    [Alias('down')]
    Down: TArray<string>;

  end;

implementation

end.
