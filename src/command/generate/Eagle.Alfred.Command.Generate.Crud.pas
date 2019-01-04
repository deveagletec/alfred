unit Eagle.Alfred.Command.Generate.Crud;

interface
uses
  System.IOUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'crud', 'Generates a Crud')]
  TGenerateCrudCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateCrudCommand }

procedure TGenerateCrudCommand.Execute;
begin
  CheckProjectConfiguration;

  FCodeGenerator.GenerateView(FModuleName, FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created View');

  FCodeGenerator.GenerateViewModel(FModuleName, FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created ViewModel');

  FCodeGenerator.GenerateModel(FModuleName, FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created Model');

  FCodeGenerator.GenerateRepository(FModuleName, FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created Repository');

  if FSkipTests then
    Exit;

  FCodeGenerator.GenerateTest(FModuleName, 'Entity', FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created Model Test');

  FCodeGenerator.GenerateTest(FModuleName, 'ViewModel', FName);

  if FVerbose then
    FConsoleIO.WriteInfo('Created ViewModel Test');

end;

initialization
  TAlfred.GetInstance.Register(TGenerateCrudCommand);
end.
