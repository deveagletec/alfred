unit Eagle.Alfred.Command.Project;

interface
uses
  Classes,
  SysUtils,
  System.IOUtils,

  XSuperJSON, XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Data;

type

  [Command('new', 'PROJECT', 'Gerencia os dados do projeto')]
  TProjectCommand = class(TCommand)
  private
    procedure GetSubDirs(const sRootDir: string; slt: TStrings);
    function AddDirSeparator(const dir: string): string;
  public
    [Action('CREATE', '')]
    procedure Execute;

    [Action('SETUP', '')]
    procedure Setup;

    [Action('VERSION', '')]
    procedure Version;

    [Action('HELP', '')]
    procedure Help();
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

procedure TProjectCommand.Execute;
var
  list: TStringList;
begin

  CheckProjectConfiguration;

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

procedure TProjectCommand.Setup;
var
  Package: TPackage;
  Data: string;
begin

  Package := TPackage.Create;

  Package.Id := '';
  Package.Version := String.Parse(CurrentYear) + '.00#';
  Package.BaseDir := '.\';
  Package.MigrationDir := 'migrations\';
  Package.PackagesDir := 'packages\';
  Package.SourceDir := 'src\';
  Package.TestsDir := 'tests\';
  Package.AppNamespace := 'Eagle';
  Package.Modular := False;

  try

    Data := TJSON.Stringify(Package, True);

    TFile.WriteAllText('package.json', Data);

    FConsoleIO.WriteInfo('Projeto configurado com sucesso!');

  finally
    Package.Free;
  end;

end;

procedure TProjectCommand.Version;
begin

  CheckProjectConfiguration;

  FConsoleIO.WriteInfo(FPackage.Version);

end;

initialization
  TAlfred.GetInstance.Register(TProjectCommand);

end.
