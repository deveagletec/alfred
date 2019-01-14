unit Eagle.Alfred.Downloaders.SourceForgeDownloader;

interface
uses Eagle.Alfred.Downloaders.Downloader, Eagle.Alfred.Data;

type

   TSourceForgeDownloader = class(TDownloader)
   public
      function GetUrlDependency(Dependency : TDependency) : string; override;
   end;

implementation

{ TSourceForgeDownloader }

function TSourceForgeDownloader.GetUrlDependency(Dependency: TDependency): string;
begin
  //https://sourceforge.net/code-snapshots/svn/a/ac/acbr/code/acbr-code-13690.zip
   Result := 'https://api.github.com/repos/' + Dependency.Name + '/zipball/' + Dependency.Version;
end;

end.
