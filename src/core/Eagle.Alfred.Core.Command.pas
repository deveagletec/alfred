unit Eagle.Alfred.Core.Command;

interface

uses
  Eagle.Alfred.Core.ConsoleIO,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Exceptions;

type
  ICommand = interface
    ['{CD87FF03-12AE-4AB2-9D53-BEE9CB353811}']
    procedure Execute;
  end;

  TCommandAbstract = class abstract(TInterfacedObject, ICommand)
  protected
    FAppPath: string;
    FCurrentPath: string;
    FConsoleIO: IConsoleIO;
    FConfiguration: TConfiguration;
    FPackage: TPackage;
    FVerbose: Boolean;

    procedure Init; virtual;
    procedure DoShowMessageSuccessful(const Msg: string);
  public
    constructor Create(const AAppPath, ACurrentPath: string; AConfig: TConfiguration; APackage: TPackage; ConsoleIO: IConsoleIO);
    procedure Execute; virtual; abstract;

    [OptionAttribute('verbose', 'v', 'Adds more details to output logging')]
    procedure Verbose;
  end;

implementation

{ TCommandAbstract }

constructor TCommandAbstract.Create(const AAppPath, ACurrentPath: string; AConfig: TConfiguration; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  FAppPath := AAppPath;
  FCurrentPath := ACurrentPath + '\';
  FConsoleIO := ConsoleIO;
  FPackage := APackage;
  FConfiguration := AConfig;

  Init;
end;

procedure TCommandAbstract.DoShowMessageSuccessful(const Msg: string);
begin
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteSuccess('* ------- ');
  FConsoleIO.WriteSuccessFmt('| %s ;) ', [Msg]);
  FConsoleIO.WriteSuccess('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo('');
end;

procedure TCommandAbstract.Init;
begin

end;

procedure TCommandAbstract.Verbose;
begin
  FVerbose := True;
end;

end.
