unit Eagle.Alfred.Utils;

interface

uses
  Windows,
  System.SysUtils,
  System.Classes;

function Capitalize(const Str: string): string;
procedure CreateDiretories(const Paths: array of string);
function GuidCreate: string;
function GetResource(const ResourceName: string): TStringList;

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

    flag := CharInSet(s[i], [' ', '[', ']', '(', ')']);
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

function GetResource(const ResourceName: string): TStringList;
var
  ResourceStream: TResourceStream;
  StringList: TStringList;
begin

  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    StringList := TStringList.Create;
    try
      StringList.LoadFromStream(ResourceStream);
    except
      on E: Exception do
      begin
        StringList.Free;
        raise;
      end;
    end;
  finally
    ResourceStream.Free;
  end;

  Result := StringList;
end;

end.
