unit Eagle.Alfred.Downloaders.GithubDownloader;

interface
uses Eagle.Alfred.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TGithubDownloader = class(TDownloader)
   public
      function GetUrlDependency(Dependency : TDependency) : string; override;
   end;

implementation

{ TGithubDownloader }

function TGithubDownloader.GetUrlDependency(Dependency: TDependency): string;
begin
   Result := 'https://api.github.com/repos/' + Dependency.Name + '/zipball/' + Dependency.Version;
end;

end.
