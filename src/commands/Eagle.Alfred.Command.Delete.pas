unit Eagle.Alfred.Command.Delete;

interface
uses
  Eagle.ConsoleIO,
  Eagle.Alfred,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Data;

type
  [Command('remove', 'DELETE', 'Exclui units do projeto')]
  TDeleteCommand = class(TCommand)
  public

    [Action('VIEW', '')]
    procedure DeleteView(const ModelName, ModuleName: string);

    [Action('VIEWMODEL', '')]
    procedure DeleteViewModel(const ModelName, ModuleName: string);

    [Action('MODEL', '')]
    procedure DeleteModel(const ModelName, ModuleName: string);

    [Action('SERVICE', '')]
    procedure DeleteService(const ModelName, ModuleName: string);

    [Action('REPOSITORY', '')]
    procedure DeleteRepository(const ModelName, ModuleName: string);

    [Action('TEST', '')]
    procedure DeleteTest(const ModelName, ModuleName: string);

    [Action('CRUD', '')]
    procedure DeleteCRUD(const ModelName, ModuleName: string);

    [Action('FILE', '')]
    procedure DeleteFile(const FileName);

  end;

implementation

{ TDeleteCommand }

procedure TDeleteCommand.DeleteCRUD(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteFile(const FileName);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteModel(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteRepository(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteService(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteTest(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteView(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

procedure TDeleteCommand.DeleteViewModel(const ModelName, ModuleName: string);
begin
  FConsoleIO.WriteError('Commando não implementado!');
end;

initialization

  TAlfred.GetInstance.Register(TDeleteCommand);

end.
