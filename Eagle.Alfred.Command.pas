unit Eagle.Alfred.Command;

interface
uses
  System.SysUtils,
  Eagle.ConsoleIO,
  Eagle.Alfred.Data,
  Eagle.Alfred.Exceptions;

type

  TCommand = class
  protected
    FAppPath: string;
    FConsoleIO: IConsoleIO;
    FPackage: TPackage;

    procedure CheckProjectConfiguration;

  public
    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
  end;

implementation

{ TCommand }

procedure TCommand.CheckProjectConfiguration;
begin

  if not Assigned(FPackage) then
    raise EAlfredException.Create('Projeto não configurado! Arquivo package.json não encontrado.');

  FPackage.Validate;

end;

constructor TCommand.Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  FAppPath := AppPath;
  FConsoleIO := ConsoleIO;
  FPackage := APackage;
end;

end.
