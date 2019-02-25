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
   Eagle.Alfred.Command.Common.Downloaders.Downloader;

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

      procedure DoResolver(Dependency: TDependency);
      procedure LoadFileLock;
      function ResolverSourceDiretory(const Path : string): string;
      procedure SaveFileLock;
      procedure ScanSourceDirectory(Dependency: TDependency);
      procedure DoScanSourceDirectory(const Path : string);
      procedure DownloadDependency(Dependency: TDependency);
      procedure UnZipDependency(const FileName : string);
      function GetSourceDirName(const FileName : string) : string;
      procedure CopyDependency(Dependency : TDependency);
      procedure DeleteDownloadedFiles(const FileName : string);
      function IsInstalled(Dependency: TDependency): Boolean;
      procedure LoadDependencies;
      procedure Prepare;
      procedure RegisterDependency(Dependency: TDependency);
      procedure RemoveDependencyDir(Dependency: TDependency);
      procedure TrimDependencies;
      procedure UpdateMainProject;
      procedure UpdatePackage;
      procedure UpdateProject;
      procedure UpdateTestProject;
   public
      constructor Create(APackage: TPackage; aIO : IConsoleIO);
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
  SourceDirName := GetSourceDirName(Dependency.Project);

  DestDirName := FVendorDir + Dependency.Project;

  if TDirectory.Exists(DestDirName) then
    TDirectory.Delete(DestDirName, True);

  TDirectory.Move(SourceDirName, DestDirName);
end;

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
  Prepare;

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

    if FSaveDev then
      Dependencies := FPackageLocked.DevDependencies
    else
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
  Dependencies: TArray<string>;
begin
  Updated := False;

  Prepare;

  if FSaveDev then
    Dependencies := FPackage.DevDependencies
  else
    Dependencies := FPackage.Dependencies;

  for Value in Dependencies do
  begin
    Dependency := TDependency.Create(Value);

    if IsInstalled(Dependency) and not FForce then
    begin
      FConsoleIO.WriteAlertFmt('Dependency %s already installed!', [Dependency.Name.QuotedString]);
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
  Dependency: TDependency;
  List: TList<string>;
begin
  Dependencies := FInstalledDependencies.Values.ToArray;

  List := TList<string>.Create;

  try
    for Dependency in Dependencies do
      List.Add(Dependency.Full);

    if FSaveDev then
      FPackageLocked.DevDependencies := List.ToArray
    else
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
  FConsoleIO.WriteProcess('Scanning source diretory...');
  try
    DependencyDiretory := FVendorDir + Dependency.Project;

    SourceDiretory := ResolverSourceDiretory(DependencyDiretory);

    if SourceDiretory.Equals(DependencyDiretory) then
      FPaths.Add(FRelativeVendorPath + SourceDiretory)
    else
      DoScanSourceDirectory(SourceDiretory);

    FConsoleIO.WriteProcess('Scanning source diretory... Done');
  finally
    FConsoleIO.WriteInfo('');
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

    FConsoleIO.WriteInfo('');
    FConsoleIO.WriteProcess('Unzipping ...');
    UnZipDependency(Dependency.Project);
    FConsoleIO.WriteProcess('Unzipping... Done');

    FConsoleIO.WriteInfo('');
    FConsoleIO.WriteProcess('Copying...');
    CopyDependency(Dependency);
    FConsoleIO.WriteProcess('Copying... Done');

    FConsoleIO.WriteInfo('');
    FConsoleIO.WriteProcess('Cleaning swap...');
    DeleteDownloadedFiles(Dependency.Project);
    FConsoleIO.WriteProcess('Cleaning swap... Done');
  finally
    FConsoleIO.WriteInfo('');
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
  if not FInstalledDependencies.ContainsKey(Dependency.Name) then
    Exit(False);
  Result := FInstalledDependencies.Items[Dependency.Name].Version.Equals(Dependency.Version);
end;

procedure TDependencyResolver.LoadDependencies;
var
  Dep: string;
  Dependency: TDependency;
  Dependencies: TArray<string>;
begin
  if FSaveDev then
    Dependencies := FPackage.DevDependencies
  else
    Dependencies := FPackage.Dependencies;

  for Dep in Dependencies do
  begin
    Dependency := TDependency.Create(Dep);
    FDependencies.Add(Dependency.Name, Dependency);
  end;
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
  if FInstalledDependencies.ContainsKey(Dependency.Name) then
  begin
    Dep := FInstalledDependencies.Items[Dependency.Name];

    if not Dependency.Version.Equals(Dep.Version) or FForce then
      FRemovedDependencies.Add(Dep);
  end;

  FDependencies.AddOrSetValue(Dependency.Name, Dependency);
  FInstalledDependencies.AddOrSetValue(Dependency.Name, Dependency);
end;

procedure TDependencyResolver.RemoveDependencyDir(Dependency: TDependency);
var
  Path: string;
begin
  Path := FVendorDir + Dependency.Project;

  if TDirectory.Exists(Path) then
    TDirectory.Delete(Path, True);
end;

procedure TDependencyResolver.TrimDependencies;
var
  Dependency: TDependency;
  Dependencies: TArray<TDependency>;
begin
  Dependencies := FInstalledDependencies.Values.ToArray;

  for Dependency in Dependencies do
  begin
    if FDependencies.ContainsKey(Dependency.Name) then
      Continue;

    FRemovedDependencies.Add(Dependency);

    FInstalledDependencies.Remove(Dependency.Name);

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
    FMainProject.RemoveLibInSearchPath(FVendorDir + Dependency.Project);

  for Path in FPaths do
    FMainProject.AddPathInUnitSearchPath(Path);

  FMainProject.Save;
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

    if FSaveDev then
      FPackage.DevDependencies := List.ToArray
    else
      FPackage.Dependencies := List.ToArray;
  finally
    List.Free;
  end;

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
    FConsoleIO.WriteInfo('');
  end;
end;

procedure TDependencyResolver.UpdateTestProject;
var
  Dependency: TDependency;
  Path: string;
begin
  for Dependency in FRemovedDependencies.ToArray do
    FTestProject.RemoveLibInSearchPath(FVendorDir + Dependency.Project);

  for Path in FPaths do
    FTestProject.AddPathInUnitSearchPath(Path);

  FTestProject.Save;
end;

end.
