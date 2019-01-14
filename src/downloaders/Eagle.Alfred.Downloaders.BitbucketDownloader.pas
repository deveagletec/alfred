unit Eagle.Alfred.Downloaders.BitbucketDownloader;

interface
uses Eagle.Alfred.Downloaders.Downloader, Eagle.Alfred.Data;

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
