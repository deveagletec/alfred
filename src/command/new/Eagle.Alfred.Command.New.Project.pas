unit Eagle.Alfred.Command.New.Project;

interface
uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Windows,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.IOUtils,
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

    procedure CreateDirs;
    procedure CreateFiles;
    procedure CreateGitIgnore;
    procedure CreatePackageJson;

    procedure DoCreateProjectFile(const ResourceName, FileName: string);
    procedure CreateFile(const Path, Contents: string);
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

  end;

implementation

procedure TNewProjectCommand.CreateDirs;
var
  Value, Path: string;
  Paths: TArray<string>;
begin
  Paths := ['\build','\ci\','\libs','\packages','\resources','\src','\tests'];

  for Value in Paths do
  begin

    Path := FProjectDir + Value;

    TIOUtils.CreateDir(Path, FForce);

    if FVerbose then
      FConsoleIO.WriteInfo('Created .'+ Value);

  end;
end;

procedure TNewProjectCommand.CreateFile(const Path, Contents: string);
begin

  TIOUtils.CreateFile(FProjectDir + Path, Contents, FForce);

  if FVerbose then
    FConsoleIO.WriteInfo('Created .' + Path);

end;

procedure TNewProjectCommand.CreateFiles;
begin

  CreateFile('\README.md', '#' + FName);
  CreateGitIgnore;

  DoCreateProjectFile('project_base_dpr', FName + '.dpr');
  DoCreateProjectFile('project_base_dproj', FName + '.dproj');

  if FSkipTests then
    Exit;

  DoCreateProjectFile('project_test_dpr', FName + 'Test.dpr');
  DoCreateProjectFile('project_test_dproj', FName + 'Test.dproj');
  DoCreateProjectFile('project_group', FName + 'Group.groupproj');

  CreateFile('\ci\dcov_paths.lst', '');
  CreateFile('\ci\dcov_units.lst', '');

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
    FConsoleIO.WriteInfo('Created .\.gitignore');
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

procedure TNewProjectCommand.DoCreateProjectFile(const ResourceName, FileName: string);
var
  StringList: TStringList;
begin

  StringList := GetResource(ResourceName);

  try

    StringList.Text := StringList.Text.Replace('{Project-Name}', FName, [rfReplaceAll]);

    StringList.SaveToFile(FProjectDir + '\' + FPackage.PackagesDir + FileName);
  finally
    StringList.Free;
  end;

  if FVerbose then
    FConsoleIO.WriteInfo('Created .\' + FileName);

end;

procedure TNewProjectCommand.Execute;
begin

  if FVerbose then
    FConsoleIO.WriteInfo('Creating ' + FName + '... ')
  else
    FConsoleIO.WriteProcess('Creating ' + FName + '... ');

  TIOUtils.CreateDir(FCurrentPath + FName, FForce);

  CreatePackageJson;

  CreateDirs;

  CreateFiles;

  if FVerbose then
    FConsoleIO.WriteInfo('Done')
  else
    FConsoleIO.WriteProcess('Creating ' + FName + '... Done');

end;

procedure TNewProjectCommand.Force;
begin
  FForce := True;
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

initialization
  TAlfred.GetInstance.Register(TNewProjectCommand);
end.
