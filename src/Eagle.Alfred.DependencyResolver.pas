unit Eagle.Alfred.DependencyResolver;

interface
uses
   System.SysUtils,
   System.Classes,
   System.Generics.Collections,
   System.IOUtils,

   XSuperObject,

   Eagle.Alfred.Core.Types,
   Eagle.Alfred.Core.ConsoleIO,
   Eagle.Alfred.Core.IOUtils,
   Eagle.Alfred.DprojParser,
   Eagle.Alfred.Command.Common.Downloaders.Downloader,
   Eagle.Alfred.Command.Common.Downloaders.GithubDownloader,
   Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader,
   Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader;

type

   IDependencyResolver = interface
      ['{BBA5E2B9-98CC-4C23-9772-3F009020E746}']
      procedure Resolver(Dependency: TDependency);
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
      FForce: Boolean;
      FConsoleIO : IConsoleIO;
      FPaths : TList<string>;
      FDependencies: TDictionary<string, TDependency>;
      FInstalledDependencies: TDictionary<string, TDependency>;
      FRemovedDependencies: TList<TDependency>;
      FMainProject: IDprojParser;
      FTestProject: IDprojParser;

      procedure DoResolver(Dependency: TDependency);
      procedure LoadFileLock;
      function ResolverSourceDiretory(const Path : string): string;
      procedure SaveFileLock;
      procedure ScanSourceDirectory(Dependency: TDependency);
      procedure DoScanSourceDirectory(const Path : string);
      procedure DownloadBitbucket(Dependency: TDependency);
      procedure DownloadDependency(Dependency: TDependency);
      procedure DownloadGithub(Dependency: TDependency);
      procedure DownloadGitlab(Dependency: TDependency);
      function IsInstalled(Dependency: TDependency): Boolean;
      procedure RegisterDependency(Dependency: TDependency);
      procedure UpdateLibraryPath();
      procedure UpdatePackage;
      procedure UpdateProject;
   public
      constructor Create(APackage: TPackage; aIO : IConsoleIO);
      destructor Destroy(); override;
      procedure Resolver(Dependency: TDependency);
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

  FPaths := TList<string>.Create;
  FDependencies := TDictionary<string,TDependency>.Create;
  FInstalledDependencies := TDictionary<string,TDependency>.Create;
  FRemovedDependencies := TList<TDependency>.Create;

  FMainProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name);
  FTestProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name + 'Test');

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
  Len: Integer;
begin

  try

    Dependency := TDependency.Create(Value);

    if IsInstalled(Dependency) and not FForce then
    begin
      FConsoleIO.WriteInfo('Dependency already installed!');
      Exit;
    end;

    DoResolver(Dependency);

    UpdateProject;

    UpdatePackage;

    SaveFileLock;
  finally
    Dependency.Free;
  end;

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

procedure TDependencyResolver.Resolver(Dependency: TDependency);
begin

end;

procedure TDependencyResolver.ResolverAll;
var
  Dependency : TDependency;
begin

 { for Dependency in FPackage.Dependencies do
    DoResolver(Dependency);  }

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
    FPaths.Add(SourceDiretory)
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

  FPaths.Add(Path);

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

procedure TDependencyResolver.DownloadBitbucket(Dependency: TDependency);
var
  Downloader: IDownloader;
begin
  Downloader := TBitbucketDownloader.Create(FConsoleIO, FVendorDir);
  Downloader.DownloadDependency(Dependency);
end;

procedure TDependencyResolver.DownloadDependency(Dependency: TDependency);
var
  RepoName: string;
begin
  RepoName := Dependency.Repo.ToUpper;

  if 'GITHUB'.Equals(RepoName) then
    DownloadGithub(Dependency)
  else if 'BITBUCKET'.Equals(RepoName) then
    DownloadBitbucket(Dependency)
  else if 'GITLAB'.Equals(RepoName) then
    DownloadGitlab(Dependency)
  else
    raise Exception.Create('Invalid repository ' + RepoName);
end;

procedure TDependencyResolver.DownloadGithub(Dependency: TDependency);
var
  Downloader: IDownloader;
begin
  Downloader := TGithubDownloader.Create(FConsoleIO, FVendorDir);
  Downloader.DownloadDependency(Dependency);
end;

procedure TDependencyResolver.DownloadGitlab(Dependency: TDependency);
var
  Downloader: IDownloader;
begin
  Downloader := TGitlabDownloader.Create(FConsoleIO, FVendorDir);
  Downloader.DownloadDependency(Dependency);
end;

function TDependencyResolver.IsInstalled(Dependency: TDependency): Boolean;
begin
  if not FInstalledDependencies.ContainsKey(Dependency.Name) then
    Exit(False);
  Result := FInstalledDependencies.Items[Dependency.Name].Version.Equals(Dependency.Version);
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

  FDependencies.Add(Dependency.Name, Dependency);
  FInstalledDependencies.Add(Dependency.Name, Dependency);

end;

procedure TDependencyResolver.Uninstall(Dependency: TDependency);
begin

end;

procedure TDependencyResolver.UpdateAll;
begin

end;

procedure TDependencyResolver.UpdateLibraryPath;
var
  Path: string;
begin

  for Path in FPaths do
  begin
    FMainProject.AddPathInUnitSearchPath(Path);
  end;

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
begin
  // Remover dependencias depreciadas
  // Adicionar as novas dependencias

  //UpdateLibraryPath;

  TFile.WriteAllText('lib.txt', string.Join(#13, FPaths.ToArray));
end;

end.
