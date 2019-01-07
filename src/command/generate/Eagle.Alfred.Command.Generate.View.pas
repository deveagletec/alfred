unit Eagle.Alfred.Command.Generate.View;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'view', 'Generates a View')]
  TGenerateViewCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateViewCommand }

procedure TGenerateViewCommand.Execute;
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateView(FModuleName, FName);

  FConsoleIO.WriteInfo('Created View');
end;

initialization
  TAlfred.GetInstance.Register(TGenerateViewCommand);
end.