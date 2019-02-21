unit Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader;

interface
uses Eagle.Alfred.Command.Common.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TGitlabDownloader = class(TDownloader)
   public
      function GetUrlDependency(Dependency : TDependency) : string; override;
   end;

implementation

{ TGitlabDownloader }

function TGitlabDownloader.GetUrlDependency(Dependency: TDependency): string;
begin
   Result := 'https://api.github.com/repos/' + Dependency.Name + '/zipball/' + Dependency.Version;
end;

end.