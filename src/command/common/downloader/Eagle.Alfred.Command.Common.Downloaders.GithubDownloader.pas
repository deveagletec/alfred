unit Eagle.Alfred.Command.Common.Downloaders.GithubDownloader;

interface
uses
  System.SysUtils,
  Eagle.Alfred.Command.Common.Downloaders.Downloader,
  Eagle.Alfred.Core.Types;

type

  TGithubDownloader = class(TDownloader)
  protected
    function MountUrl(Dependency : TDependency): string; override;
    procedure SetAuthentication(Dependency: TDependency); override;
  end;

implementation

{ TGithubDownloader }

function TGithubDownloader.MountUrl(Dependency: TDependency): string;
begin
  Result := 'https://api.github.com/repos/' + Dependency.GroupId + '/' + Dependency.ArtifactId + '/zipball/' + Dependency.Version;
end;

procedure TGithubDownloader.SetAuthentication(Dependency: TDependency);
begin
  if Dependency.AuthUser.IsEmpty then
    Exit;

  FIdHTTP.Request.BasicAuthentication := True;
  FIdHTTP.Request.Username := Dependency.AuthUser;
  FIdHTTP.Request.Password := Dependency.AuthPass;
end;

end.
