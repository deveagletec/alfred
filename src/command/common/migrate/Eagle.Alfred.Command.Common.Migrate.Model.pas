unit Eagle.Alfred.Command.Common.Migrate.Model;

interface

uses
  XSuperObject;

type
  TMigrate = record

    [DISABLE]
    Id: string;

    [DISABLE]
    Name: string;

    [DISABLE]
    WasExecuted: Boolean;

    [Alias('issue')]
    Issue: string;

    [Alias('version')]
    Version: string;

    [Alias('description')]
    Description: string;

    [Alias('author')]
    Author: string;

    [Alias('ignore')]
    IgnoredScripts: TArray<Integer>;

    [Alias('up')]
    Up: TArray<string>;

    [Alias('down')]
    Down: TArray<string>;

  end;

implementation

end.
