unit Eagle.Alfred.Command.Common.Downloaders.GithubDownloader;

interface
uses Eagle.Alfred.Command.Common.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TGithubDownloader = class(TDownloader)
   public
      function GetUrlDependency(Dependency : TDependency) : string; override;
   end;

implementation

{ TGithubDownloader }

function TGithubDownloader.GetUrlDependency(Dependency: TDependency): string;
begin
   Result := 'https://api.github.com/repos/' + Dependency.User + '/' + Dependency.Project + '/zipball/' + Dependency.Version;
end;

end.
