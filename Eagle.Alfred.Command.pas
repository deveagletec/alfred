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

    function Capitalize(const Str: string): string;
    procedure CreateDiretories(const Paths: array of string);

    procedure CheckProjectConfiguration;
  public
    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
  end;

implementation

{ TCommand }

function TCommand.Capitalize(const Str: string): string;
var
  flag: Boolean;
  i: Byte;
  s: string;
begin

  flag := True;
  s := Str.ToLower;
  Result := EmptyStr;

  for i := 1 to Length(s) do
  begin

    if flag then
      Result := Result + AnsiUpperCase(s[i])
    else
      Result := Result + s[i];

    flag := CharInSet(s[i], [' ', '[',']', '(', ')']);
  end;

end;

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

procedure TCommand.CreateDiretories(const Paths: array of string);
var
  Path: string;
begin

  for Path in Paths do
  begin
    if not DirectoryExists(Path) then
      ForceDirectories(Path);
  end;

end;

end.
