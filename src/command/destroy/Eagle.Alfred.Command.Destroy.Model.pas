unit Eagle.Alfred.Command.Destroy.Model;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Destroy;

type

  [Command('destroy', 'model', 'Generates package.json')]
  TDestroyModelCommand = class(TDestroyCommand)
  public
     procedure Execute; override;
  end;

implementation

{ TDestroyModelCommand }

procedure TDestroyModelCommand.Execute;
begin
  inherited;
  DoDestroyFile('Model');

  FConsoleIO.WriteInfo('Removed Model');
end;

initialization
  TAlfred.GetInstance.Register(TDestroyModelCommand);
end.
