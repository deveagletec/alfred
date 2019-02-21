unit Eagle.Alfred.DependencyResolver;

interface
uses
   TypInfo,
   System.SysUtils,
   System.Classes,
   System.Generics.Collections,
   System.IOUtils,
   system.RegularExpressions,

   XSuperObject,

   Eagle.Alfred.Core.Types,
   Eagle.Alfred.Core.ConsoleIO,
   Eagle.Alfred.Core.IOUtils,
   Eagle.Alfred.DprojParser,
   Eagle.Alfred.Core.DownloaderFactory,
   Eagle.Alfred.Command.Common.Downloaders.Downloader;

type

   IDependencyResolver = interface
      ['{BBA5E2B9-98CC-4C23-9772-3F009020E746}']
      procedure ResolverAll;
      procedure UpdateAll;
      procedure Install(const Dependency: string);
      procedure Uninstall(Dependency: TDependency);
      procedure SetForce(const Value: Boolean);
   end;

   TDependencyResolver = class(TInterfacedObject ,IDependencyResolver)
   private
      FPackage: TPackage;
      FPackageLocked: TPackagelocked;
      FVendorDir : string;
      FRelativeVendorPath: string;
      FForce: Boolean;
      FConsoleIO : IConsoleIO;
      FPaths : TList<string>;
      FDependencies: TDictionary<string, TDependency>;
      FInstalledDependencies: TDictionary<string, TDependency>;
      FRemovedDependencies: TList<TDependency>;
      FMainProject: IDprojParser;
      FTestProject: IDprojParser;
      FDownloaderFactory: IDownloaderFactory;

      procedure DoResolver(Dependency: TDependency);
      procedure LoadFileLock;
      function ResolverSourceDiretory(const Path : string): string;
      procedure SaveFileLock;
      procedure ScanSourceDirectory(Dependency: TDependency);
      procedure DoScanSourceDirectory(const Path : string);
      procedure DownloadDependency(Dependency: TDependency);
      function IsInstalled(Dependency: TDependency): Boolean;
      procedure LoadDependencies;
      procedure RegisterDependency(Dependency: TDependency);
      procedure UpdatePackage;
      procedure UpdateProject;
   public
      constructor Create(APackage: TPackage; aIO : IConsoleIO);
      destructor Destroy(); override;
      procedure ResolverAll;
      procedure UpdateAll;
      procedure Install(const Value: string);
      procedure Uninstall(Dependency: TDependency);
      procedure SetForce(const Value: Boolean);
   end;

implementation

{ TDependencyResolver }

constructor TDependencyResolver.Create(APackage: TPackage; aIO : IConsoleIO);
begin
  FForce := False;

  if APackage.VendorDir.EndsWith('\') then
    FVendorDir := APackage.VendorDir
  else
    FVendorDir := APackage.VendorDir + '\';

  FPackage := APackage;
  FConsoleIO := aIO;

  FRelativeVendorPath := TRegEx.Replace(FPackage.PackagesDir, '[^\\]+', '..');

  FPaths := TList<string>.Create;
  FDependencies := TDictionary<string,TDependency>.Create;
  FInstalledDependencies := TDictionary<string,TDependency>.Create;
  FRemovedDependencies := TList<TDependency>.Create;

  FMainProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name);
  FTestProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name + 'Test');

  FDownloaderFactory := TDownloaderFactory.Create(FConsoleIO, FVendorDir);

  LoadDependencies;
  LoadFileLock;
end;

destructor TDependencyResolver.Destroy;
begin

  if Assigned(FPackageLocked) then
    FPackageLocked.Free;

  if Assigned(FPaths) then
    FPaths.Free;

  if Assigned(FDependencies) then
    FDependencies.Free;

  if Assigned(FInstalledDependencies) then
    FInstalledDependencies.Free;

  if Assigned(FRemovedDependencies) then
    FRemovedDependencies.Free;

  inherited;
end;

procedure TDependencyResolver.DoResolver(Dependency: TDependency);
begin
  FConsoleIO.WriteInfo('Resolving dependency '+ Dependency.Name.QuotedString);

  DownloadDependency(Dependency);

  ScanSourceDirectory(Dependency);

  RegisterDependency(Dependency);
end;

procedure TDependencyResolver.Install(const Value: string);
var
  Dependency: TDependency;
begin

  Dependency := TDependency.Create(Value);

  if IsInstalled(Dependency) and not FForce then
  begin
    FConsoleIO.WriteInfoFmt('Dependency %s already installed!', [Dependency.Name.QuotedString]);
    Exit;
  end;

  DoResolver(Dependency);

  UpdateProject;

  UpdatePackage;

  SaveFileLock;
end;

procedure TDependencyResolver.LoadFileLock;
var
  Data, Dep: string;
  Dependencies: TArray<string>;
  Dependency: TDependency;
begin

  try
    Data := TFile.ReadAllText('package.lock');

    FPackageLocked := TJSON.Parse<TPackageLocked>(Data);

    Dependencies := FPackageLocked.Dependencies;

    for Dep in Dependencies do
    begin
      Dependency := TDependency.Create(Dep);

      FInstalledDependencies.Add(Dependency.Name, Dependency);
    end;

  except on E: Exception do
    raise Exception.Create('Error load package.lock + ' + E.Message);
  end;

end;

procedure TDependencyResolver.ResolverAll;
var
  Dependency : TDependency;
  Value: string;
  Updated: Boolean;
begin
  Updated := False;

  for Value in FPackage.Dependencies do
  begin
    Dependency := TDependency.Create(Value);

    if IsInstalled(Dependency) and not FForce then
    begin
      FConsoleIO.WriteInfoFmt('Dependency %s already installed!', [Dependency.Name.QuotedString]);
      Continue;
    end;

    DoResolver(Dependency);
    Updated := True;
  end;

  if not Updated then
    Exit;

  UpdateProject;

  UpdatePackage;

  SaveFileLock;
end;

function TDependencyResolver.ResolverSourceDiretory(const Path : string): string;
var
  Dirs: TArray<string>;
  Dir: string;
begin
  Dirs := ['src', 'source'];
  Result := Path;

  for Dir in Dirs do
  begin
    if TDirectory.Exists(Path + '\' + Dir) then
    begin
      Result := Path + '\' + Dir;
      Break;
    end;
  end;
end;

procedure TDependencyResolver.SaveFileLock;
var
  Dependencies: TArray<TDependency>;
  Dependency: TDependency;
  List: TList<string>;
  Data: string;
begin
  Dependencies := FInstalledDependencies.Values.ToArray;

  List := TList<string>.Create;

  try
    for Dependency in Dependencies do
      List.Add(Dependency.Full);

    FPackageLocked.Dependencies := List.ToArray;

    TIOUtils.Save<TPackageLocked>(FPackageLocked, 'package.lock');
  finally
    List.Free;
  end;
end;

procedure TDependencyResolver.ScanSourceDirectory(Dependency: TDependency);
var
  DependencyDiretory, SourceDiretory: string;
begin
  FConsoleIO.WriteInfo('Scanning source diretory...');

  DependencyDiretory := FVendorDir + Dependency.Project;

  SourceDiretory := ResolverSourceDiretory(DependencyDiretory);

  if SourceDiretory.Equals(DependencyDiretory) then
    FPaths.Add(FRelativeVendorPath + SourceDiretory)
  else
    DoScanSourceDirectory(SourceDiretory);
end;

procedure TDependencyResolver.SetForce(const Value: Boolean);
begin
  FForce := Value;
end;

procedure TDependencyResolver.DoScanSourceDirectory(const Path : string);
var
  SearchResult: TSearchRec;
  Dir: string;
begin

  if FPaths.Contains(Path) then
    Exit;

  FPaths.Add(FRelativeVendorPath + Path);

  if FindFirst(Path + '\*', faDirectory, SearchResult) <> 0 then
    Exit;

  repeat
    Dir := SearchResult.Name;

    if Dir.Equals('.') or Dir.Equals('..') then
      Continue;

    if (SearchResult.Attr and faDirectory) = faDirectory then
      DoScanSourceDirectory(Path + '\' + Dir);

  until FindNext(searchResult) <> 0;

  FindClose(searchResult);
end;

procedure TDependencyResolver.DownloadDependency(Dependency: TDependency);
var
  RepoName: string;
  Repo: TRepositoryType;
  Downloader: IDownloader;
begin
  RepoName := Dependency.Repo.Trim;

  Repo := TRepositoryType(GetEnumValue(TypeInfo(TRepositoryType), RepoName));

  Downloader := FDownloaderFactory.GetDownloader(Repo);
  Downloader.DownloadDependency(Dependency);
end;

function TDependencyResolver.IsInstalled(Dependency: TDependency): Boolean;
begin
  if not FInstalledDependencies.ContainsKey(Dependency.Name) then
    Exit(False);
  Result := FInstalledDependencies.Items[Dependency.Name].Version.Equals(Dependency.Version);
end;

procedure TDependencyResolver.LoadDependencies;
var
  Dep: string;
  Dependency: TDependency;
begin
  for Dep in FPackage.Dependencies do
  begin
    Dependency := TDependency.Create(Dep);
    FDependencies.Add(Dependency.Name, Dependency);
  end;
end;

procedure TDependencyResolver.RegisterDependency(Dependency: TDependency);
var
  Dep: TDependency;
begin
  if FInstalledDependencies.ContainsKey(Dependency.Name) then
  begin
    Dep := FInstalledDependencies.Items[Dependency.Name];

    if not Dependency.Version.Equals(Dep.Version) or FForce then
      FRemovedDependencies.Add(Dep);
  end;

  FDependencies.AddOrSetValue(Dependency.Name, Dependency);
  FInstalledDependencies.AddOrSetValue(Dependency.Name, Dependency);
end;

procedure TDependencyResolver.Uninstall(Dependency: TDependency);
begin

end;

procedure TDependencyResolver.UpdateAll;
begin

end;

procedure TDependencyResolver.UpdatePackage;
var
  Dependencies: TArray<TDependency>;
  Dependency: TDependency;
  List: TList<string>;
begin
  Dependencies := FDependencies.Values.ToArray;

  List := TList<string>.Create;

  try
    for Dependency in Dependencies do
      List.Add(Dependency.Full);

    FPackage.Dependencies := List.ToArray;
  finally
    List.Free;
  end;

  TIOUtils.Save<TPackage>(FPackage, 'package.json');
end;

procedure TDependencyResolver.UpdateProject;
var
  Dependency: TDependency;
  Path: string;
begin

  for Dependency in FRemovedDependencies.ToArray do
    FMainProject.RemoveLibInSearchPath(FVendorDir + Dependency.Project);

  for Path in FPaths do
    FMainProject.AddPathInUnitSearchPath(Path);

  FMainProject.Save;
end;

end.
