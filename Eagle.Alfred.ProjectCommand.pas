unit Eagle.Alfred.ProjectCommand;

interface
uses
  Classes,
  SysUtils,
  Eagle.ConsoleIO,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Command;

type
  TProjectCommand = class(TInterfacedObject, ICommand)
  private
    FAppPath: string;
    FDprojParser: TDprojParser;
    FConsoleIO: IConsoleIO;
    procedure GetSubDirs(const sRootDir: string; slt: TStrings);
    function AddDirSeparator(const dir: string): string;
  public
    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
    destructor Destroy; override;
    procedure Execute;
    procedure Help;
  end;

implementation

{ TProjectCommand }

function TProjectCommand.AddDirSeparator(const dir: string): string;
var h:string;
begin

  Result:= dir;

  h := dir;

  if h[Length(h)] <> '\' then h := h + '\';
    Result:=h;

end;

constructor TProjectCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
begin
  FAppPath := AppPath;
  FConsoleIO := ConsoleIO;
  FDprojParser := DprojParser;
end;

destructor TProjectCommand.Destroy;
begin

  inherited;
end;

procedure TProjectCommand.Execute;
var
  list: TStringList;
begin

  list := TStringList.Create;

  try

    GetSubDirs('src', list);

    list.SaveToFile('search_library.txt');

  finally
    list.Free;
  end;

end;

procedure TProjectCommand.GetSubDirs(const sRootDir: string; slt: TStrings);
var
  srSearch: TSearchRec;
  sSearchPath: string;
  sltSub: TStrings;
  i: Integer;
begin

  sltSub := TStringList.Create;
  slt.BeginUpdate;

  try
    sSearchPath := AddDirSeparator(sRootDir);
    if FindFirst(sSearchPath + '*', faDirectory, srSearch) = 0 then
      repeat
        if ((srSearch.Attr and faDirectory) = faDirectory) and
          (srSearch.Name <> '.') and
          (srSearch.Name <> '..') then
        begin
          slt.Add(sSearchPath + srSearch.Name);
          sltSub.Add(sSearchPath + srSearch.Name);
        end;
      until (FindNext(srSearch) <> 0);

    FindClose(srSearch);

    for i := 0 to sltSub.Count - 1 do
      GetSubDirs(sltSub.Strings[i], slt);

  finally
    slt.EndUpdate;
    FreeAndNil(sltSub);
  end;

end;

procedure TProjectCommand.Help;
begin

end;

end.
