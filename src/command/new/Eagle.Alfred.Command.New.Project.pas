unit Eagle.Alfred.Command.New.Project;

interface
uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Windows,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type
  [Command('new', 'project', 'Default applications are created in a directory of the same name')]
  TNewProjectCommand = class(TCommandAbstract)
  private
    FProjectDir: string;
    FName: string;
    FForce: Boolean;
    FSkipTests: Boolean;
    FSkipPackageJson: Boolean;
    FVerbose: Boolean;

    procedure CreateDirs;
    procedure CreateFiles;
    procedure CreateGitIgnore;
    procedure CreatePackageJson;
    procedure CreateProjectDpr;
    procedure CreateProjectDproj;
    procedure CreateProjectGroup;
    procedure CreateProjectTestDpr;
    procedure CreateProjectTestDproj;
    procedure CreateReadMe;
    function GetResource(ResourceName: string): TStringList;
  public
    procedure Execute; override;

    [ParamAttribute(1, 'Nome do projeto')]
    procedure SetName(const Name: string);

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [OptionAttribute('skip-tests', '-S', 'Skip creating tests files.')]
    procedure SkipTests;

    [OptionAttribute('skip-package-json', '', 'Do not add dependencies to package.json.')]
    procedure SkipPackageJson;

    [OptionAttribute('verbose', 'v', 'Adds more details to output logging.')]
    procedure Verbose;

  end;

implementation

{ TNewProjectCommand }

procedure TNewProjectCommand.CreateDirs;
var
  Value, Path: string;
  Paths: TArray<string>;
begin

  Paths := ['\build','\ci\','\libs','\packages','\resources','\src','\tests'];

  for Value in Paths do
  begin

    Path := FProjectDir + Value;

    TDirectory.CreateDirectory(Path);

    if FVerbose then
      FConsoleIO.WriteInfo('Create '+ Value);

  end;
end;

procedure TNewProjectCommand.CreateFiles;
begin

  CreateReadMe;
  CreateGitIgnore;
  CreateProjectDpr;
  CreateProjectDproj;

  if FSkipTests then
    Exit;

  CreateProjectTestDpr;
  CreateProjectTestDproj;
  CreateProjectGroup;

end;

procedure TNewProjectCommand.CreateGitIgnore;
var
  StringList: TStringList;
begin

  StringList := GetResource('git_ignore');

  try
    StringList.SaveToFile(FProjectDir + '\.gitignore');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create .gitignore');
end;

procedure TNewProjectCommand.CreatePackageJson;
var
  Package: TPackage;
  Data: string;
begin

  Package := TPackage.Create;

  Package.Id := FName;
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

    TFile.WriteAllText(FProjectDir + '\package.json', Data);

  finally
    Package.Free;
  end;

end;

procedure TNewProjectCommand.CreateProjectDpr;
var
  StringList: TStringList;
begin

  StringList := GetResource('project_base_dpr');

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\packages\' + FName + '.dpr');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create ' + FName + '.dpr');

end;

procedure TNewProjectCommand.CreateProjectDproj;
var
  StringList: TStringList;
begin

  StringList := GetResource('project_base_dproj');

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\packages\' + FName + '.dproj');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create ' + FName + '.dproj');

end;

procedure TNewProjectCommand.CreateProjectGroup;
var
  StringList: TStringList;
begin

  StringList := GetResource('project_group');

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\packages\' + FName + 'Group.groupproj');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create ' + FName + 'Group.groupproj');
end;

procedure TNewProjectCommand.CreateProjectTestDpr;
var
  StringList: TStringList;
begin

  StringList := GetResource('project_test_dpr');

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\packages\' + FName + 'Test.dpr');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create ' + FName + 'Test.dpr');

end;

procedure TNewProjectCommand.CreateProjectTestDproj;
var
  StringList: TStringList;
begin

  StringList := GetResource('project_test_dproj');

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\packages\' + FName + 'Test.dproj');
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Create ' + FName + 'Test.dproj');

end;

procedure TNewProjectCommand.CreateReadMe;
begin
  TFile.WriteAllText(FProjectDir + '\README.md', '#' + FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Create README.md');
end;

procedure TNewProjectCommand.Execute;
begin

  FConsoleIO.WriteInfo('Creating ' + FName);

  TDirectory.CreateDirectory(FCurrentPath + FName);

  CreatePackageJson;

  CreateDirs;

  CreateFiles;

  FConsoleIO.WriteInfo('Done');

end;

procedure TNewProjectCommand.Force;
begin
  FForce := True;
end;

function TNewProjectCommand.GetResource(ResourceName: string): TStringList;
var
  ResourceStream: TResourceStream;
  StringList: TStringList;
begin

  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    StringList := TStringList.Create;
    try
      StringList.LoadFromStream(ResourceStream);
    except on E: Exception do
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

procedure TNewProjectCommand.SetName(const Name: string);
begin
  FName := Name;
  FProjectDir := FCurrentPath + FName;
end;

procedure TNewProjectCommand.SkipPackageJson;
begin
  FSkipPackageJson := True;
end;

procedure TNewProjectCommand.SkipTests;
begin
  FSkipTests := True;
end;

procedure TNewProjectCommand.Verbose;
begin
  FVerbose := True;
end;

initialization
  TAlfred.GetInstance.Register(TNewProjectCommand);
end.
