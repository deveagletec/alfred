unit Eagle.Alfred.Command.Show;

interface
uses
  System.SysUtils,
  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Command;

type
  [Command('show', '', 'To list all of the available packages')]
  TShowCommand = class(TCommandAbstract)
  private
    FPackageName: string;
    FVersionOnly: Boolean;
    FNameOnly: Boolean;
    FRootOnly: Boolean;
    FInstalledOnly: Boolean;

    procedure ClearOptions;
    procedure ListAll;
    procedure ListDetails;
    procedure RootDetails;
    procedure Detail(Dependency: TDependency);
    function SelectDependency: TDependency;
  public
    procedure Execute; override;

    [Param(1, 'Package name', False)]
    procedure SetName(const Name: string);

    [Option('version', '-v', 'List package versions')]
    procedure Version;

    [Option('name-only', '-n', 'List package names only')]
    procedure NameOnly;

    [Option('self', '-s', 'List the root package info')]
    procedure Root;

    [Option('installed', '-i', 'List the packages that are installed')]
    procedure Installed;
  end;

implementation

{ TShowCommand }

procedure TShowCommand.ClearOptions;
begin
  FVersionOnly := False;
  FNameOnly := False;
  FRootOnly := False;
end;

procedure TShowCommand.Detail(Dependency: TDependency);
begin
  FConsoleIO.WriteInfo('Name       : ' + Dependency.Identifier);
  FConsoleIO.WriteInfo('Version    : ' + Dependency.Version);
  FConsoleIO.WriteInfo('PackageFile: ' + Dependency.PackageFile);
  FConsoleIO.WriteInfo('Repository : ' + Dependency.Repository);
  FConsoleIO.WriteInfo('Installed  : ' + Boolean.ToString(Dependency.Installed, TUseBoolStrs.True));
  FConsoleIO.WriteInfo('VendorPath : ' + Dependency.VendorPath);
  FConsoleIO.WriteInfo('Cached     : ' + Boolean.ToString(Dependency.Cached, TUseBoolStrs.True));
  FConsoleIO.WriteInfo('CachePath  : ' + Dependency.CachePath);
end;

procedure TShowCommand.Execute;
begin
  if FRootOnly then
  begin
    RootDetails;
    Exit;
  end;

  if FPackageName.IsEmpty then
    ListAll
  else
    ListDetails;
end;

procedure TShowCommand.Installed;
begin
  FInstalledOnly := True;
end;

procedure TShowCommand.ListAll;
var
  Dependency: TDependency;
begin
  for Dependency in FPackage.Dependencies do
  begin
    if FInstalledOnly and not Dependency.Installed then
      Continue;

    Detail(Dependency);
    FConsoleIO.WriteInfo(StringOfChar('-', 60));
  end;
end;

procedure TShowCommand.ListDetails;
var
  Dependency: TDependency;
begin
  Dependency := SelectDependency;

  if FVersionOnly then
  begin
    FConsoleIO.WriteInfo(Dependency.Version);
    Exit;
  end;

  if FNameOnly then
  begin
    FConsoleIO.WriteInfo(Dependency.Identifier);
    Exit;
  end;

  Detail(Dependency);
end;

procedure TShowCommand.NameOnly;
begin
  ClearOptions;
  FNameOnly := True;
end;

procedure TShowCommand.Root;
begin
  ClearOptions;
  FRootOnly := True;
end;

procedure TShowCommand.RootDetails;
begin
  FConsoleIO.WriteInfo('Name          : ' + FPackage.Name);
  FConsoleIO.WriteInfo('Namespace     : ' + FPackage.Namespace);
  FConsoleIO.WriteInfo('Description   : ' + FPackage.Description);
  FConsoleIO.WriteInfo('Version       : ' + FPackage.Version);
  FConsoleIO.WriteInfo('Modular       : ' + Boolean.ToString(FPackage.Modular, TUseBoolStrs.True));
  FConsoleIO.WriteInfo('MultiSolutions: ' + Boolean.ToString(FPackage.MultiSolutions, TUseBoolStrs.True));
  FConsoleIO.WriteInfo('BaseDir       : ' + FPackage.BaseDir);
  FConsoleIO.WriteInfo('PackagesDir   : ' + FPackage.PackagesDir);
  FConsoleIO.WriteInfo('SourceDir     : ' + FPackage.SourceDir);
  FConsoleIO.WriteInfo('TestsDir      : ' + FPackage.TestsDir);
  FConsoleIO.WriteInfo('MigrationDir  : ' + FPackage.MigrationDir);
  FConsoleIO.WriteInfo('VendorDir     : ' + FPackage.VendorDir);
end;

function TShowCommand.SelectDependency: TDependency;
var
  Dependency: TDependency;
  LPackageName: string;
begin
  LPackageName := FPackageName.ToLower;
  for Dependency in FPackage.Dependencies do
  begin
    if Dependency.Identifier.ToLower.Equals(LPackageName) then
      Exit(Dependency);
  end;

  raise EAlfredException.CreateFmt('Package "%s" not found!', [FPackageName]);
end;

procedure TShowCommand.SetName(const Name: string);
begin
  FPackageName := Name.Trim;
end;

procedure TShowCommand.Version;
begin
  ClearOptions;
  FVersionOnly := True;
end;

initialization
  TAlfred.GetInstance.Register(TShowCommand);
end.
