unit Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader;

interface
uses
  System.SysUtils,

  Eagle.Alfred.Command.Common.Downloaders.Downloader,
  Eagle.Alfred.Core.Types;

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
  Result := string.Format('https://bitbucket.org/%s/%s/get/%s.zip', [
    Dependency.GroupId,
    Dependency.ArtifactId,
    Dependency.Version
  ]);
end;

procedure TBitbucketDownloader.SetAuthentication(Dependency: TDependency);
begin
  inherited;

end;

end.
