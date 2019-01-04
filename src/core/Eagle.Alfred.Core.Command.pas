unit Eagle.Alfred.Core.Command;

interface
uses
  Eagle.ConsoleIO,
  Eagle.Alfred.Data,
  Eagle.Alfred.Exceptions;

type
  ICommand = interface
    ['{CD87FF03-12AE-4AB2-9D53-BEE9CB353811}']
    procedure Execute;
    procedure Help;
  end;

  TCommandAbstract = class abstract (TInterfacedObject, ICommand)
  protected
    FAppPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;
  public
    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
    procedure Execute; virtual; abstract;
    procedure Help; virtual; abstract;
  end;

implementation

{ TCommandAbstract }

constructor TCommandAbstract.Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  FAppPath := AppPath;
  FConsoleIO := ConsoleIO;
  FPackage := APackage;
end;

end.
