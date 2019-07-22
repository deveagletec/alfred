unit Eagle.Alfred.Command.Config.Global.Edit;

interface
uses
  SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.IOUtils,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('config:global', 'edit', 'Opens the config global file in an editor')]
  TConfigEditCommand = class(TCommandAbstract)
  public
    procedure Execute; override;
  end;

implementation

procedure TConfigEditCommand.Execute;
var
  FileName: string;
begin
  inherited;
  FileName := ExtractFilePath(ParamStr(0)) + 'alfred.conf';

  TIOUtils.OpenFile(FileName, FConfiguration.DefaultEditor);
end;

initialization
  TAlfred.GetInstance.Register(TConfigEditCommand);
end.
