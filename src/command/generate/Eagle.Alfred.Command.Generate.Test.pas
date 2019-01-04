unit Eagle.Alfred.Command.Generate.Test;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'test', 'Generates a Test')]
  TGenerateTestCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateTestCommand }

procedure TGenerateTestCommand.Execute;
begin
  inherited;

end;

initialization
  TAlfred.GetInstance.Register(TGenerateTestCommand);
end.
