unit Eagle.Alfred.Command.Config.Project.Edit;

interface

uses
  SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.IOUtils,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command;

type
  [Command('config:project', 'edit', 'Opens the config project file in an editor')]
  TConfigProjectEdit = class(TCommandAbstract)
  public
    procedure Execute; override;
  end;

implementation

procedure TConfigProjectEdit.Execute;
var
  FileName: string;
begin
  inherited;
  FileName := 'package.json';

  TIOUtils.OpenFile(FileName, FConfiguration.DefaultEditor);
end;

initialization
  TAlfred.GetInstance.Register(TConfigProjectEdit);

end.
