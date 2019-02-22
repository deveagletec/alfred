unit Eagle.Alfred.Core.DownloaderFactory;

interface
uses
  System.SysUtils,

  Eagle.Alfred.Core.ConsoleIO,

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
    FConsoleIO: IConsoleIO;
    FVendorDir: string;
  public
    constructor Create(ConsoleIO: IConsoleIO; const VendorDir: string);
    function GetDownloader(const RepoType: TRepositoryType): IDownloader;
  end;

implementation

constructor TDownloaderFactory.Create(ConsoleIO: IConsoleIO; const VendorDir: string);
begin
  FConsoleIO := ConsoleIO;
  FVendorDir := VendorDir;
end;

function TDownloaderFactory.GetDownloader(const RepoType: TRepositoryType): IDownloader;
begin
  case RepoType of
    Github:
      Result := TGithubDownloader.Create(FConsoleIO, FVendorDir);
    Gitlab:
      Result := TGitlabDownloader.Create(FConsoleIO, FVendorDir);
    Bitbucket:
      Result := TBitbucketDownloader.Create(FConsoleIO, FVendorDir);
    SourceForge:
      Result := TSourceForgeDownloader.Create(FConsoleIO, FVendorDir);
  else
    raise Exception.Create('Invalid repository ');
  end;
end;

end.
