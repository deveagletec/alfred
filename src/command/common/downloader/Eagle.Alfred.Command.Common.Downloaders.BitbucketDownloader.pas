unit Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader;

interface
uses Eagle.Alfred.Command.Common.Downloaders.Downloader, Eagle.Alfred.Core.Types;

type

   TBitbucketDownloader = class(TDownloader)
   public
      function GetUrlDependency(Dependency : TDependency) : string; override;
   end;

implementation

{ TBitbucketDownloader }

function TBitbucketDownloader.GetUrlDependency(Dependency: TDependency): string;
begin
 //https://bitbucket.org/sglienke/spring4d/get/4cf6393bf1ae.zip
end;

end.
