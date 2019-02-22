unit Eagle.Alfred.Command.Common.Downloaders.SourceForgeDownloader;

interface
uses Eagle.Alfred.Command.Common.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TSourceForgeDownloader = class(TDownloader)
   protected
      function MountUrl(Dependency : TDependency): string; override;
      procedure SetAuthentication(Dependency: TDependency); override;
   end;

implementation

{ TSourceForgeDownloader }

function TSourceForgeDownloader.MountUrl(Dependency: TDependency): string;
begin
  //https://sourceforge.net/code-snapshots/svn/a/ac/acbr/code/acbr-code-13690.zip
   Result := 'https://api.github.com/repos/' + Dependency.Name + '/zipball/' + Dependency.Version;
end;

procedure TSourceForgeDownloader.SetAuthentication(Dependency: TDependency);
begin
  inherited;

end;

end.
