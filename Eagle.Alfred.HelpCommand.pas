unit Eagle.Alfred.HelpCommand;

interface
uses
  System.SysUtils,
  Console,
  Eagle.ConsoleIO,
  Eagle.Alfred.Command,
  Eagle.Alfred.CreateCommand;

type
  THelpCommand = class(TInterfacedObject, ICommand)
  private
    FConsoleIO: IConsoleIO;
  public
    constructor Create(ConsoleIO: IConsoleIO);
    procedure Execute;
    procedure Help;
  end;

implementation

{ THelpCommand }

constructor THelpCommand.Create(ConsoleIO: IConsoleIO);
begin
  FConsoleIO := ConsoleIO;
end;

procedure THelpCommand.Execute;
begin

  Help;

end;

procedure THelpCommand.Help;
begin

  FConsoleIO.WriteInfo('         Alfred - Code Generate for Delphi');
  FConsoleIO.WriteInfo('-----------------------------------------------------');

  FConsoleIO.WriteInfo('Para obter mais informa��es sobre um comando espec�fico,');
  FConsoleIO.WriteInfo('digite nome_do_comando HELP');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('CREATE         Cria arquivos relacionados �s oper��es de CRUD');
  FConsoleIO.WriteInfo('PROJECT        Gerencia informa��es relacionadas ao projeto');
  FConsoleIO.WriteInfo('MIGRATE        Gerencia a cria��o e execu��o dos arquivos de migra��o do DB');
  FConsoleIO.WriteInfo('');

end;

end.
