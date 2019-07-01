unit Eagle.Alfred.Command.Show;

interface
uses
  System.SysUtils,
  System.RegularExpressions,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Command;

type
  [Command('show', '', 'To list all of the available packages')]
  TShowCommand = class(TCommandAbstract)
  private
    FDetailAll: Boolean;
    FPackageName: string;
    FVersionOnly: Boolean;
    FNameOnly: Boolean;
    FRootOnly: Boolean;
    FInstalledOnly: Boolean;

    procedure ListAll;
    procedure ListDetails;
    procedure RootDetails;
    procedure Detail(Dependency: TDependency);

    procedure Init; override;
  public
    procedure Execute; override;

    [Param(1, 'Package name', False)]
    procedure SetName(const Name: string);

    [Option('version', '-v', 'List package versions')]
    procedure Version;

    [Option('name', '-n', 'List package names only')]
    procedure NameOnly;

    [Option('self', '-s', 'List the root package info')]
    procedure Root;

    [Option('installed', '-i', 'List the packages that are installed')]
    procedure Installed;
  end;

implementation

{ TShowCommand }

procedure TShowCommand.Detail(Dependency: TDependency);
begin
  if FDetailAll or FNameOnly then
    FConsoleIO.WriteInfo('Name       : ' + Dependency.Identifier);

  if FDetailAll or FVersionOnly then
    FConsoleIO.WriteInfo('Version    : ' + Dependency.Version);

  if not FDetailAll then
    Exit;

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

procedure TShowCommand.Init;
begin
  inherited;
  FDetailAll := True;
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
    FConsoleIO.WriteLine;
  end;
end;

procedure TShowCommand.ListDetails;
var
  Dependency: TDependency;
  LPackageName: string;
  Detailed: Boolean;
begin
  Detailed := False;
  LPackageName := FPackageName.ToLower.Replace('*', '.*').Replace('/', '\/');
  for Dependency in FPackage.Dependencies do
  begin
    if not TRegEx.IsMatch(Dependency.Identifier.ToLower, '^' + LPackageName + '$') then
      Continue;

    if Detailed then
      FConsoleIO.WriteLine;

    Detail(Dependency);
    Detailed := True;
  end;

  if not Detailed then
    raise EAlfredException.CreateFmt('Package "%s" not found!', [FPackageName]);
end;

procedure TShowCommand.NameOnly;
begin
  FNameOnly := True;
  FRootOnly := False;
  FDetailAll := False;
end;

procedure TShowCommand.Root;
begin
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

procedure TShowCommand.SetName(const Name: string);
begin
  FPackageName := Name.Trim;
end;

procedure TShowCommand.Version;
begin
  FVersionOnly := True;
  FRootOnly := False;
  FDetailAll := False;
end;

initialization
  TAlfred.GetInstance.Register(TShowCommand);
end.
