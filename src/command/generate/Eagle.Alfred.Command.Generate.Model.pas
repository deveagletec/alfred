unit Eagle.Alfred.Command.Generate.Model;

interface
uses
  System.IOUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'model', 'Generates a Model')]
  TGenerateModelCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateModelCommand }

procedure TGenerateModelCommand.Execute;
begin

  FCodeGenerator.GenerateModel(FModuleName, FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created Model');

  if FSkipTests then
    Exit;

  FCodeGenerator.GenerateTest(FModuleName, 'Entity', FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created Model Test');

end;

initialization
  TAlfred.GetInstance.Register(TGenerateModelCommand);
end.
