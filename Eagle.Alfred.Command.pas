unit Eagle.Alfred.Command;

interface
uses
  System.SysUtils,
  Eagle.ConsoleIO,
  Eagle.Alfred.DprojParser;

type

  TCommand = class
  protected
    FAppPath: string;
    FDprojParser: TDprojParser;
    FConsoleIO: IConsoleIO;

    function Capitalize(const Str: string): string;
    procedure CreateDiretories(const Paths: array of string);
  public
    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
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

constructor TCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
begin
  FDprojParser := DprojParser;
  FAppPath := AppPath;
  FConsoleIO := ConsoleIO;
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
