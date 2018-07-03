unit Eagle.Alfred.MigrateCommand;

interface
uses
  Eagle.ConsoleIO,
  Eagle.Alfred.Command;

type
  TMigrateCommand = class(TInterfacedObject, ICommand)
  private
    FConsoleIO: IConsoleIO;
  public
    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO);
    procedure Execute;
    procedure Help;
  end;

implementation

{ TMigrateCommand }

constructor TMigrateCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO);
begin
  FConsoleIO := ConsoleIO;
end;

procedure TMigrateCommand.Execute;
begin
  Help;
end;

procedure TMigrateCommand.Help;
begin

  FConsoleIO.WriteInfo('Cria arquivos de migração de versão do banco de dados');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Para executar alguma ação de migração use:');
  FConsoleIO.WriteInfo('MIGRATE [opção] [versão] [nome_da_migração]');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Opções:');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('CREATE         Cria uma nova migrate');
  FConsoleIO.WriteInfo('EXECUTE        Executa as migrates');
  FConsoleIO.WriteInfo('DELETE         Apaga a migrate informada');
  FConsoleIO.WriteInfo('');

end;

end.
