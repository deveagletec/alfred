unit Eagle.Alfred.Command.Generate.Service;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
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

  FCodeGenerator.GenerateService(FModuleName, FName);

  FConsoleIO.WriteInfo('Created Service');

  if FSkipTests then
    Exit;

  FCodeGenerator.GenerateTest(FModuleName, 'Service', FName);

  FConsoleIO.WriteInfo('Created Service Test');
end;

initialization
  TAlfred.GetInstance.Register(TGenerateServiceCommand);
end.
