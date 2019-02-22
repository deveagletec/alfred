unit Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader;

interface
uses
  System.SysUtils,
  Eagle.Alfred.Command.Common.Downloaders.Downloader,
  Eagle.Alfred.Core.Types;

type

  TGitlabDownloader = class(TDownloader)
  protected
    function MountUrl(Dependency : TDependency): string; override;
    procedure SetAuthentication(Dependency: TDependency); override;
  end;

implementation

{ TGitlabDownloader }

function TGitlabDownloader.MountUrl(Dependency: TDependency): string;
var
  Host, ProjectId: string;
begin
  ProjectId := Dependency.User + '%2F' + Dependency.Project;

  if Dependency.Host.IsEmpty then
    Host := 'https://gitlab.com'
  else
    Host := Dependency.Host;

  Result := Host + '/api/v4/projects/'+ ProjectId +'/repository/archive.zip?sha=' + Dependency.Version;
end;

procedure TGitlabDownloader.SetAuthentication(Dependency: TDependency);
begin
  if Dependency.AuthUser.IsEmpty then
    Exit;

  FIdHTTP.Request.CustomHeaders.AddValue('Private-Token', Dependency.AuthPass);
end;

end.
