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
   end;

   TDependencyResolver = class(TInterfacedObject ,IDependencyResolver)
   private
      FPackage: TPackage;
      FVendorDir : string;
      FConsoleIO : IConsoleIO;
      FGithubDownloader : IDownloader;
      FBitbucketDownloader : IDownloader;
      FGitlabDownloader : IDownloader;
      FPaths : TList<string>;
      FInstalledDependencies: TList<TInstalledDependency>;
      FRemovedDependencies: TList<TInstalledDependency>;
      FMainProject: IDprojParser;
      FTestProject: IDprojParser;

      procedure DoResolver(Dependency: TDependency);
      procedure LoadFileLock;
      procedure SaveFileLock;
      procedure ScanSourceDirectory(const Path : string);
      procedure SeparateRemovedDependencies;
      procedure UpdateLibraryPath();
   public
      constructor Create(APackage: TPackage; aIO : IConsoleIO);
      destructor Destroy(); override;
      procedure Resolver(Dependency: TDependency);
      procedure ResolverAll;
      procedure UpdateAll;
      procedure Install(const Value: string);
      procedure Uninstall(Dependency: TDependency);
   end;

implementation

{ TDependencyResolver }

constructor TDependencyResolver.Create(APackage: TPackage; aIO : IConsoleIO);
begin

   if APackage.VendorDir.EndsWith('\') then
      FVendorDir := APackage.VendorDir
   else
      FVendorDir := APackage.VendorDir + '\';

   FPackage := APackage;
   FConsoleIO := aIO;

   FGithubDownloader := TGithubDownloader.Create(aIO, FVendorDir);
   FBitbucketDownloader := TBitbucketDownloader.Create(aIO, FVendorDir);
   FGitlabDownloader := TGitlabDownloader.Create(aIO, FVendorDir);

   FPaths := TList<string>.Create;
   FInstalledDependencies := TList<TInstalledDependency>.Create;
   FRemovedDependencies := TList<TInstalledDependency>.Create;

   FMainProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name);

   FTestProject := TDprojParser.Create(APackage.PackagesDir, APackage.Name + 'Test');

   LoadFileLock;

end;

destructor TDependencyResolver.Destroy;
begin

  if Assigned(FPaths) then
    FPaths.Free;

  if Assigned(FInstalledDependencies) then
    FInstalledDependencies.Free;

  if Assigned(FRemovedDependencies) then
    FRemovedDependencies.Free;

  inherited;
end;

procedure TDependencyResolver.DoResolver(Dependency: TDependency);
var
   RepoName, RootSourcePath : string;
begin

   RepoName := Dependency.Repo.ToUpper;

   FConsoleIO.WriteInfo('Resolving dependency '+ Dependency.Name.QuotedString);

   if 'GITHUB'.Equals(RepoName) then
      FGithubDownloader.DownloadDependency(Dependency)
   else if 'BITBUCKET'.Equals(RepoName) then
      FBitbucketDownloader.DownloadDependency(Dependency)
   else if 'GITLAB'.Equals(RepoName) then
      FGitlabDownloader.DownloadDependency(Dependency)
   else
      raise Exception.Create('Invalid repository ' + RepoName);

   RootSourcePath := FVendorDir + Dependency.Name + '\' + Dependency.SrcDir;

   ScanSourceDirectory(RootSourcePath);


end;

procedure TDependencyResolver.Install(const Value: string);
var
  Dependency: TDependency;
  Len: Integer;
begin

  try
    Dependency := TDependency.Create(Value);

   // DoResolver(Dependency);

    Len := Length(FPackage.Dependencies);

    SetLength(FPackage.Dependencies, Len + 1);

    FPackage.Dependencies[Len] := Value;

    TIOUtils.Save<TPackage>(FPackage, 'package.json');

  finally
    Dependency.Free;
  end;

end;

procedure TDependencyResolver.LoadFileLock;
var
  Data: string;
begin

  try
    Data := TFile.ReadAllText('package.lock');

    FInstalledDependencies.AddRange(TJSON.Parse<TArray<TInstalledDependency>>(Data));
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

  SeparateRemovedDependencies;

 { for Dependency in FPackage.Dependencies do
    DoResolver(Dependency);

  for Dependency in FPackage.DevDependencies do
    DoResolver(Dependency);  }

  SaveFileLock;

  UpdateLibraryPath;

end;

procedure TDependencyResolver.SaveFileLock;
var
  Dependencies: TArray<TInstalledDependency>;
  Data: string;
begin
  Dependencies := FInstalledDependencies.ToArray;

  Data := TJSON.Stringify<TArray<TInstalledDependency>>(Dependencies);

  TFile.WriteAllText('package.lock', Data);
end;

procedure TDependencyResolver.ScanSourceDirectory(const Path: string);
var
  searchResult: TSearchRec;
  Dir: string;
begin

   if FPaths.Contains(Path) then
      Exit;

   FPaths.Add(Path);

   if FindFirst(Path + '\*', faDirectory, searchResult) = 0 then
   begin

      repeat
         Dir := searchResult.Name;

         if Dir.Equals('.') or Dir.Equals('..') then
            continue;

         if (searchResult.attr and faDirectory) = faDirectory then
            ScanSourceDirectory(Path + '\' + Dir);

      until FindNext(searchResult) <> 0;

      FindClose(searchResult);
   end;

end;

procedure TDependencyResolver.SeparateRemovedDependencies;
var
  Dependency: TInstalledDependency;
  Aux: TDependency;
  Dependencies: TArray<TDependency>;
  Exists: Boolean;
begin
 // Dependencies := Concat(FPackage.Dependencies, FPackage.DevDependencies);

  Exists := False;

  for Dependency in FInstalledDependencies do
  begin

    for Aux in Dependencies do
    begin
      if Aux.Name.Equals(Dependency.Name) then
      begin
        Exists := True;
        Break;
      end;
    end;

    if not Exists then
      FRemovedDependencies.Add(Dependency);
  end;
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

end.
