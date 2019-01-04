unit Eagle.Alfred.Command.Generate.Repository;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'repository', 'Generates a Repository')]
  TGenerateRepositoryCommand = class(TGenerateCrudFileCommand)
  public
    procedure Execute; override;
  end;

implementation

{ TGenerateViewCommand }

procedure TGenerateRepositoryCommand.Execute;
begin

  FCodeGenerator.GenerateRepository(FModuleName, FName);

  FConsoleIO.WriteInfo('Created Repository');

end;

initialization
  TAlfred.GetInstance.Register(TGenerateRepositoryCommand);
end.
