unit Eagle.Alfred.Core.Command;

interface
uses
  Eagle.ConsoleIO,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Exceptions;

type
  ICommand = interface
    ['{CD87FF03-12AE-4AB2-9D53-BEE9CB353811}']
    procedure Execute;
  end;

  TCommandAbstract = class abstract (TInterfacedObject, ICommand)
  protected
    FCurrentPath: string;
    FConsoleIO: IConsoleIO;
    FConfiguration: TConfiguration;
    FPackage: TPackage;
    FVerbose: Boolean;

    procedure Init; virtual;
  public
    constructor Create(const ACurrentPath: string; AConfig: TConfiguration; APackage: TPackage; ConsoleIO: IConsoleIO);
    procedure Execute; virtual; abstract;

    [OptionAttribute('verbose', 'v', 'Adds more details to output logging.')]
    procedure Verbose;
  end;

implementation

{ TCommandAbstract }

constructor TCommandAbstract.Create(const ACurrentPath: string; AConfig:
    TConfiguration; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  FCurrentPath := ACurrentPath + '\';
  FConsoleIO := ConsoleIO;
  FPackage := APackage;
  FConfiguration := AConfig;

  Init;
end;

procedure TCommandAbstract.Init;
begin

end;

procedure TCommandAbstract.Verbose;
begin
  FVerbose := True;
end;

end.
