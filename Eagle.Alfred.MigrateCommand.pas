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

  FConsoleIO.WriteInfo('Cria arquivos de migra��o de vers�o do banco de dados');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Para executar alguma a��o de migra��o use:');
  FConsoleIO.WriteInfo('MIGRATE [op��o] [vers�o] [nome_da_migra��o]');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Op��es:');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('CREATE         Cria uma nova migrate');
  FConsoleIO.WriteInfo('EXECUTE        Executa as migrates');
  FConsoleIO.WriteInfo('DELETE         Apaga a migrate informada');
  FConsoleIO.WriteInfo('');

end;

end.
