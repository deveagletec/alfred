unit Eagle.Alfred.Command.Config.Edit;

interface
uses
  ShellApi,
  Windows,
  SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('config', 'edit', 'Opens the config file in an editor')]
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

  ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWNORMAL)
end;

initialization
  TAlfred.GetInstance.Register(TConfigEditCommand);
end.
