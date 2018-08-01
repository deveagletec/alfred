unit Eagle.Alfred.Utils;

interface
uses
  System.SysUtils;

function Capitalize(const Str: string): string;
procedure CreateDiretories(const Paths: array of string);
function GuidCreate: string;

implementation

function Capitalize(const Str: string): string;
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

procedure CreateDiretories(const Paths: array of string);
var
  Path: string;
begin

  for Path in Paths do
  begin
    if not DirectoryExists(Path) then
      ForceDirectories(Path);
  end;

end;

function GuidCreate: string;
var
  ID: TGUID;
begin

  ID := TGUID.NewGuid;

  Result := GUIDToString(ID);

end;

end.
