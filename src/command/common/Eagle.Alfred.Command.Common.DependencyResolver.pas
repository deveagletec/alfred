unit Eagle.Alfred.Command.Common.DependencyResolver;

interface
uses
   TypInfo,
   System.Zip,
   System.SysUtils,
   System.Classes,
   System.Generics.Collections,
   System.IOUtils,
   system.RegularExpressions,

   XSuperObject,

   Eagle.Alfred.Core.Types,
   Eagle.Alfred.Core.ConsoleIO,
   Eagle.Alfred.Core.IOUtils,
   Eagle.Alfred.Core.Exceptions,
   Eagle.Alfred.Command.Common.DprojParser,
   Eagle.Alfred.Command.Common.DownloaderFactory,
   Eagle.Alfred.Command.Common.Downloaders.Downloader,
   Eagle.Alfred.Command.Common.Builder;

type

   IDependencyResolver = interface
      ['{BBA5E2B9-98CC-4C23-9772-3F009020E746}']
      procedure ResolverAll;
      procedure UpdateAll;
      procedure Install(const Dependency: string);
      procedure Uninstall(const Dependency: string);
      procedure SetForce(const Value: Boolean);
      procedure SetSaveDev(const Value: Boolean);
   end;

   TDependencyResolver = class(TInterfacedObject ,IDependencyResolver)
   private
      FConfiguration: TConfiguration;
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
      FSaveDev: Boolean;

      procedure BuildDependency(Dependency: TDependency);
      procedure DoResolver(Dependency: TDependency);
      procedure LoadFileLock;
      function ResolverSourceDiretory(const Path : string): string;
      procedure SaveFileLock;
      procedure ScanSourceDirectory(Dependency: TDependency);
      procedure DoScanSourceDirectory(const Path : string);
      procedure DownloadDependency(Dependency: TDependency);
      procedure DoDownloadDependency(Dependency: TDependency);
      procedure UnZipDependency(const FileName : string);
      function GetSourceDirName(const FileName : string) : string;
      procedure CopyDependency(Dependency : TDependency);
      procedure DeleteDownloadedFiles(const FileName : string);
      procedure DoCopyDependencyFromStorage(Dependency: TDependency);
      function IsInstalled(Dependency: TDependency): Boolean;
      procedure LoadDependencies;
      procedure Prepare;
      procedure RegisterDependency(Dependency: TDependency);
      procedure RemoveDependencyDir(Dependency: TDependency);
      procedure SaveCache(Dependency: TDependency; const Mode: string);
      procedure TrimDependencies;
      procedure UpdateMainProject;
      procedure UpdatePackage;
      procedure UpdateProject;
      procedure UpdateTestProject;
   public
      constructor Create(AConfiguration: TConfiguration; APackage: TPackage; aIO : IConsoleIO);
      destructor Destroy(); override;
      procedure ResolverAll;
      procedure UpdateAll;
      procedure Install(const Value: string);
      procedure Uninstall(const DependencyName: string);
      procedure SetForce(const Value: Boolean);
      procedure SetSaveDev(const Value: Boolean);
   end;

implementation

{ TDependencyResolver }

procedure TDependencyResolver.CopyDependency(Dependency: TDependency);
var
  SourceDirName, DestDirName: string;
begin
  SourceDirName := GetSourceDirName(Dependency.ArtifactId);

  DestDirName := FVendorDir + Dependency.ArtifactId;

  if TDirectory.Exists(DestDirName) then
    TDirectory.Delete(DestDirName, True);

  TDirectory.Copy(SourceDirName, Dependency.CachePath + '\source');
  TDirectory.Move(SourceDirName, DestDirName + '\source');
end;

constructor TDependencyResolver.Create(AConfiguration: TConfiguration; APackage: TPackage; aIO : IConsoleIO);
begin
  FForce := False;

  if APackage.VendorDir.EndsWith('\') then
    FVendorDir := APackage.VendorDir
  else
    FVendorDir := APackage.VendorDir + '\';

  FConfiguration := AConfiguration;
  FPackage := APackage;
  FConsoleIO := aIO;

  FRelativeVendorPath := TRegEx.Replace(FPackage.PackagesDir, '[^\\]+', '..');

  FPaths := TList<string>.Create;
  FDependencies := TDictionary<string,TDependency>.Create;
  FInstalledDependencies := TDictionary<string,TDependency>.Create;
  FRemovedDependencies := TList<TDependency>.Create;

  FMainProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name);
  FTestProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name + 'Test');

  FDownloaderFactory := TDownloaderFactory.Create(FVendorDir);
end;

procedure TDependencyResolver.DeleteDownloadedFiles(const FileName: string);
begin
  TDirectory.Delete(FileName, True);
  TFile.Delete(FileName + '.zip');
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

procedure TDependencyResolver.BuildDependency(Dependency: TDependency);
var
  Builder: IBuilder;
begin
  Builder := TBuilder.Create;

  FConsoleIO.WriteInfo('Buiding dependency...');
  Builder.Build(Dependency, 'Debug');
  SaveCache(Dependency, 'Debug');
  FConsoleIO.WriteInfo('Buiding dependency... Done');
end;

procedure TDependencyResolver.DoCopyDependencyFromStorage(Dependency: TDependency);
var
  DestDirName: string;
begin
  DestDirName := FVendorDir + Dependency.ArtifactId;

  if TDirectory.Exists(DestDirName) then
    TDirectory.Delete(DestDirName, True);

  FConsoleIO.WriteProcess('Copying from Cache...');

  try
    TDirectory.Copy(Dependency.CachePath, DestDirName);

    FConsoleIO.WriteProcess('Copying from Cache... Done');
  finally
    FConsoleIO.NewEmptyLine;
  end;
end;

procedure TDependencyResolver.DoResolver(Dependency: TDependency);
begin
  Dependency.Prepare;

  FConsoleIO.WriteInfo('Resolving dependency '+ Dependency.Identifier.QuotedString);

  Dependency.VendorPath := FVendorDir + Dependency.ArtifactId;
  Dependency.VendorPathFull := GetCurrentDir + '\' + FVendorDir + Dependency.ArtifactId;

  Dependency.CachePath := string.Format('%s\%s\%s\%s', [
    FConfiguration.StorageDir,
    Dependency.GroupId,
    Dependency.ArtifactId,
    Dependency.Version]);

  Dependency.Cached := TDirectory.Exists(Dependency.CachePath);

  DownloadDependency(Dependency);

  if not Dependency.Cached then
    BuildDependency(Dependency);

  ScanSourceDirectory(Dependency);

  RegisterDependency(Dependency);
end;

procedure TDependencyResolver.Install(const Value: string);
var
  Dependency: TDependency;
begin
  Prepare;

 // Dependency := TDependency.Create(Value);

  if IsInstalled(Dependency) and not FForce then
  begin
    FConsoleIO.WriteInfoFmt('Dependency %s already installed!', [Dependency.Identifier.QuotedString]);
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
  Dependencies: TArray<TDependency>;
  Dependency: TDependency;
begin
  if not TFile.Exists('package.lock') then
    Exit;

  try
    Data := TFile.ReadAllText('package.lock');

    FPackageLocked := TJSON.Parse<TPackageLocked>(Data);

    if FSaveDev then
      Dependencies := FPackageLocked.DevDependencies
    else
      Dependencies := FPackageLocked.Dependencies;

    for Dependency in Dependencies do
      FInstalledDependencies.Add(Dependency.Identifier, Dependency);

  except on E: Exception do
    raise Exception.Create('Error load package.lock + ' + E.Message);
  end;
end;

procedure TDependencyResolver.ResolverAll;
var
  Dependency : TDependency;
  Updated: Boolean;
  Dependencies: TArray<TDependency>;
begin
  Updated := False;

  Prepare;

  if FSaveDev then
    Dependencies := FPackage.DevDependencies
  else
    Dependencies := FPackage.Dependencies;

  for Dependency in Dependencies do
  begin
    if IsInstalled(Dependency) and not FForce then
    begin
      FConsoleIO.WriteAlertFmt('Dependency %s already installed!', [Dependency.Identifier.QuotedString]);
      Continue;
    end;

    DoResolver(Dependency);
    Updated := True;
  end;

  if not Updated then
    Exit;

  TrimDependencies;

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
begin
  if not Assigned(FPackageLocked) then
    FPackageLocked := TPackagelocked.Create;

  Dependencies := FInstalledDependencies.Values.ToArray;

  if FSaveDev then
    FPackageLocked.DevDependencies := Dependencies
  else
    FPackageLocked.Dependencies := Dependencies;

  TIOUtils.Save<TPackageLocked>(FPackageLocked, 'package.lock');
end;

procedure TDependencyResolver.ScanSourceDirectory(Dependency: TDependency);
var
  DependencyDiretory, SourceDiretory: string;
begin
  FConsoleIO.WriteProcess('Scanning source diretory...');
  try
    DependencyDiretory := FVendorDir + Dependency.ArtifactId + '\source';

    SourceDiretory := ResolverSourceDiretory(DependencyDiretory);

    if SourceDiretory.Equals(DependencyDiretory) then
      FPaths.Add(FRelativeVendorPath + SourceDiretory)
    else
      DoScanSourceDirectory(SourceDiretory);

    FConsoleIO.WriteProcess('Scanning source diretory... Done');
  finally
    FConsoleIO.NewEmptyLine;
  end;
end;

procedure TDependencyResolver.SetForce(const Value: Boolean);
begin
  FForce := Value;
end;

procedure TDependencyResolver.SetSaveDev(const Value: Boolean);
begin
  FSaveDev := Value;
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
begin
  if Dependency.Cached then
    DoCopyDependencyFromStorage(Dependency)
  else
    DoDownloadDependency(Dependency);
end;

procedure TDependencyResolver.DoDownloadDependency(Dependency: TDependency);
var
  RepoName: string;
  Repo: TRepositoryType;
  Downloader: IDownloader;
begin
  RepoName := Dependency.Repo.Trim;

  Repo := TRepositoryType(GetEnumValue(TypeInfo(TRepositoryType), RepoName));

  Downloader := FDownloaderFactory.GetDownloader(Repo);

  try
    FConsoleIO.WriteProcess('Downloading => 0 Mb');
    Downloader.DownloadDependency(Dependency, procedure(const Value: Double)
    begin
      FConsoleIO.WriteProcess('Downloading => ' + String.Parse(Value) + ' Mb');
    end);

    FConsoleIO.NewEmptyLine;
    FConsoleIO.WriteProcess('Unzipping ...');
    UnZipDependency(Dependency.ArtifactId);
    FConsoleIO.WriteProcess('Unzipping... Done');

    FConsoleIO.NewEmptyLine;
    FConsoleIO.WriteProcess('Copying...');
    CopyDependency(Dependency);
    FConsoleIO.WriteProcess('Copying... Done');

    FConsoleIO.NewEmptyLine;
    FConsoleIO.WriteProcess('Cleaning swap...');
    DeleteDownloadedFiles(Dependency.ArtifactId);
    FConsoleIO.WriteProcess('Cleaning swap... Done');
  finally
    FConsoleIO.NewEmptyLine;
  end;
end;

function TDependencyResolver.GetSourceDirName(const FileName: string): string;
var
  Dir : string;
  searchResult : TSearchRec;
begin
  if FindFirst(FileName + '\*', faDirectory, searchResult) = 0 then
  begin

    repeat
      Dir := searchResult.Name;

      if Dir.Equals('.') or Dir.Equals('..') then
         continue;

      Result := FileName + '\' + Dir;

    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;
end;

function TDependencyResolver.IsInstalled(Dependency: TDependency): Boolean;
begin
  if not FInstalledDependencies.ContainsKey(Dependency.Identifier) then
    Exit(False);
  Result := FInstalledDependencies.Items[Dependency.Identifier].Version.Equals(Dependency.Version);
end;

procedure TDependencyResolver.LoadDependencies;
var
  Dependency: TDependency;
  Dependencies: TArray<TDependency>;
begin
  if FSaveDev then
    Dependencies := FPackage.DevDependencies
  else
    Dependencies := FPackage.Dependencies;

  for Dependency in Dependencies do
    FDependencies.Add(Dependency.Identifier, Dependency);
end;

procedure TDependencyResolver.Prepare;
begin
  LoadDependencies;
  LoadFileLock;
end;

procedure TDependencyResolver.RegisterDependency(Dependency: TDependency);
var
  Dep: TDependency;
begin
  if FInstalledDependencies.ContainsKey(Dependency.Identifier) then
  begin
    Dep := FInstalledDependencies.Items[Dependency.Identifier];

    if not Dependency.Version.Equals(Dep.Version) or FForce then
      FRemovedDependencies.Add(Dep);
  end;

  FDependencies.AddOrSetValue(Dependency.Identifier, Dependency);
  FInstalledDependencies.AddOrSetValue(Dependency.Identifier, Dependency);
end;

procedure TDependencyResolver.RemoveDependencyDir(Dependency: TDependency);
var
  Path: string;
begin
  Path := FVendorDir + Dependency.ArtifactId;

  if TDirectory.Exists(Path) then
    TDirectory.Delete(Path, True);
end;

procedure TDependencyResolver.SaveCache(Dependency: TDependency; const Mode: string);
var
  SourceDir, TargetDir: string;
begin
  SourceDir := Dependency.VendorPath + '\bin\' + Mode;
  TargetDir := Dependency.CachePath + '\bin\' + Mode;

  TDirectory.Copy(SourceDir, TargetDir);
end;

procedure TDependencyResolver.TrimDependencies;
var
  Dependency: TDependency;
  Dependencies: TArray<TDependency>;
begin
  Dependencies := FInstalledDependencies.Values.ToArray;

  for Dependency in Dependencies do
  begin
    if FDependencies.ContainsKey(Dependency.Identifier) then
      Continue;

    FRemovedDependencies.Add(Dependency);

    FInstalledDependencies.Remove(Dependency.Identifier);

    RemoveDependencyDir(Dependency);
  end;
end;

procedure TDependencyResolver.Uninstall(const DependencyName: string);
begin
  Prepare;

  if not FDependencies.ContainsKey(DependencyName) then
    raise EUninstallException.Create('Dependency Not Found');

  FDependencies.Remove(DependencyName);

  TrimDependencies;

  UpdateProject;

  UpdatePackage;

  SaveFileLock;
end;

procedure TDependencyResolver.UnZipDependency(const FileName: string);
var
  Zipper: TZipFile;
begin
  Zipper := TZipFile.Create();

  if TDirectory.Exists(FileName) then
    TDirectory.Delete(FileName, True);

  try
    Zipper.Open(FileName + '.zip', zmRead);
    Zipper.ExtractAll(FileName);
    Zipper.Close;
  finally
    Zipper.Free;
  end;
end;

procedure TDependencyResolver.UpdateAll;
begin
  Prepare;
end;

procedure TDependencyResolver.UpdateMainProject;
var
  Dependency: TDependency;
  Path: string;
begin
  for Dependency in FRemovedDependencies.ToArray do
    FMainProject.RemoveLibInSearchPath(FVendorDir + Dependency.ArtifactId);

  for Path in FPaths do
    FMainProject.AddPathInUnitSearchPath(Path);

  //FInstalledDependencies

  FMainProject.Save;
end;

procedure TDependencyResolver.UpdatePackage;
var
  Dependencies: TArray<TDependency>;
begin
  Dependencies := FDependencies.Values.ToArray;

  if FSaveDev then
    FPackage.DevDependencies := Dependencies
  else
    FPackage.Dependencies := Dependencies;

  TIOUtils.Save<TPackage>(FPackage, 'package.json');
end;

procedure TDependencyResolver.UpdateProject;
begin
  FConsoleIO.WriteProcess('Updating Search Path...');
  try
    UpdateTestProject;

    if not FSaveDev then
      UpdateMainProject;

    FConsoleIO.WriteProcess('Updating Search Path... Done');
  finally
    FConsoleIO.NewEmptyLine;
  end;
end;

procedure TDependencyResolver.UpdateTestProject;
var
  Dependency: TDependency;
  Path: string;
begin
  for Dependency in FRemovedDependencies.ToArray do
    FTestProject.RemoveLibInSearchPath(FVendorDir + Dependency.ArtifactId);

  for Path in FPaths do
    FTestProject.AddPathInUnitSearchPath(Path);

  FTestProject.Save;
end;

end.
