unit Eagle.Alfred.Command.Common.DownloaderFactory;

interface
uses
  System.SysUtils,

  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Command.Common.Downloaders.Downloader,
  Eagle.Alfred.Command.Common.Downloaders.GithubDownloader,
  Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader,
  Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader,
  Eagle.Alfred.Command.Common.Downloaders.SourceForgeDownloader;

type

  TRepositoryType = (Github, Bitbucket, Gitlab, SourceForge, Unknown);

  IDownloaderFactory = interface
    ['{E7BC9E60-670B-4CD0-8C43-AE044CFFE076}']
    function GetDownloader(const RepoType: TRepositoryType): IDownloader;
  end;

  TDownloaderFactory = class(TInterfacedObject, IDownloaderFactory)
  private
    FVendorDir: string;

    FGithubDownloader: IDownloader;
    FGitlabDownloader: IDownloader;
    FBitbucketDownloader: IDownloader;
    FSourceForgeDownloader: IDownloader;
    function CreateBitbucketDownloader: IDownloader;
    function CreateGithubDownloader: IDownloader;
    function CreateGitlabDownloader: IDownloader;
    function CreateSourceForgeDownloader: IDownloader;
  public
    constructor Create(const VendorDir: string);
    function GetDownloader(const RepoType: TRepositoryType): IDownloader;
  end;

implementation

constructor TDownloaderFactory.Create(const VendorDir: string);
begin
  FVendorDir := VendorDir;
end;

function TDownloaderFactory.CreateBitbucketDownloader: IDownloader;
begin
  if not Assigned(FBitbucketDownloader) then
    FBitbucketDownloader := TBitbucketDownloader.Create(FVendorDir);

  Result := FBitbucketDownloader;
end;

function TDownloaderFactory.CreateGithubDownloader: IDownloader;
begin
  if not Assigned(FGithubDownloader) then
    FGithubDownloader := TGithubDownloader.Create(FVendorDir);

  Result := FGithubDownloader
end;

function TDownloaderFactory.CreateGitlabDownloader: IDownloader;
begin
  if not Assigned(FGitlabDownloader) then
    FGitlabDownloader := TGitlabDownloader.Create(FVendorDir);

  Result := FGitlabDownloader;
end;

function TDownloaderFactory.CreateSourceForgeDownloader: IDownloader;
begin
  if not Assigned(FSourceForgeDownloader) then
    FSourceForgeDownloader := TSourceForgeDownloader.Create(FVendorDir);

  Result := FSourceForgeDownloader;
end;

function TDownloaderFactory.GetDownloader(const RepoType: TRepositoryType): IDownloader;
begin
  case RepoType of
    Github:
      Result := CreateGithubDownloader;
    Gitlab:
      Result := CreateGitlabDownloader;
    Bitbucket:
      Result := CreateBitbucketDownloader;
    SourceForge:
      Result := CreateSourceForgeDownloader;
  else
    raise ERepositoryTypeInvalidException.Create('Repository type invalid');
  end;
end;

end.
