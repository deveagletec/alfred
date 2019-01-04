unit Eagle.Alfred.Command.Generate.ViewModel;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'view-model', 'Generates a ViewModel')]
  TGenerateViewModelCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateViewModelCommand }

procedure TGenerateViewModelCommand.Execute;
begin
  inherited;

end;

initialization
  TAlfred.GetInstance.Register(TGenerateViewModelCommand);
end.
