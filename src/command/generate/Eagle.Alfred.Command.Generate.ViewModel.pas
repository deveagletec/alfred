unit Eagle.Alfred.Command.Generate.ViewModel;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
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

  FCodeGenerator.GenerateViewModel(FModuleName, FName);

  FConsoleIO.WriteInfo('Created ViewModel');

  if FSkipTests then
    Exit;

  FCodeGenerator.GenerateTest(FModuleName, 'ViewModel', FName);

  FConsoleIO.WriteInfo('Created ViewModel Test');

end;

initialization
  TAlfred.GetInstance.Register(TGenerateViewModelCommand);
end.
