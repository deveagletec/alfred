unit Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader;

interface
uses Eagle.Alfred.Command.Common.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TBitbucketDownloader = class(TDownloader)
   protected
      function MountUrl(Dependency : TDependency): string; override;
      procedure SetAuthentication(Dependency: TDependency); override;
   end;

implementation

{ TBitbucketDownloader }

function TBitbucketDownloader.MountUrl(Dependency: TDependency): string;
begin
 //https://bitbucket.org/sglienke/spring4d/get/4cf6393bf1ae.zip
end;

procedure TBitbucketDownloader.SetAuthentication(Dependency: TDependency);
begin
  inherited;

end;

end.
