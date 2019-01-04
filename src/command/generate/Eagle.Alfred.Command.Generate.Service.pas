unit Eagle.Alfred.Command.Generate.Service;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'service', 'Generates a Service')]
  TGenerateServiceCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateViewCommand }

procedure TGenerateServiceCommand.Execute;
begin
  inherited;

end;

initialization
  TAlfred.GetInstance.Register(TGenerateServiceCommand);
end.
