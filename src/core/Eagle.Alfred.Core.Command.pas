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
  end;

  TCommandAbstract = class abstract (TInterfacedObject, ICommand)
  protected
    FCurrentPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;

    procedure Init; virtual;
  public
    constructor Create(const ACurrentPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
    procedure Execute; virtual; abstract;
  end;

implementation

{ TCommandAbstract }

constructor TCommandAbstract.Create(const ACurrentPath: string; APackage:
    TPackage; ConsoleIO: IConsoleIO);
begin
  FCurrentPath := ACurrentPath + '\';
  FConsoleIO := ConsoleIO;
  FPackage := APackage;

  Init;
end;

procedure TCommandAbstract.Init;
begin

end;

end.
