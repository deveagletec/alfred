unit Eagle.Alfred.Command.Delete.Model;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Delete.DeleteCrudFile;

type
  [Command('generate', 'test', 'Generates a Test')]
  TDeleteModelCommand = class(TDeleteCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TDeleteModelCommand }

procedure TDeleteModelCommand.Execute;
begin
  inherited;

end;

initialization
 // TAlfred.GetInstance.Register(TDeleteModelCommand);

end.
