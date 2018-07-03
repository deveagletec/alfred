unit Eagle.Alfred.MigrateCommand;

interface
uses
  Eagle.ConsoleIO,
  Eagle.Alfred.Command;

type
  TMigrateCommand = class(TInterfacedObject, ICommand)
  public
    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO);
    procedure Execute;
    procedure Help;
  end;

implementation

{ TMigrateCommand }

constructor TMigrateCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO);
begin

end;

procedure TMigrateCommand.Execute;
begin

end;

procedure TMigrateCommand.Help;
begin

end;

end.
