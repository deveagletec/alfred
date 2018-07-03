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

  FConsoleIO.WriteInfo('Para obter mais informações sobre um comando específico,');
  FConsoleIO.WriteInfo('digite nome_do_comando HELP');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('CREATE         Cria arquivos relacionados às operções de CRUD');
  FConsoleIO.WriteInfo('PROJECT        Gerencia informações relacionadas ao projeto');
  FConsoleIO.WriteInfo('MIGRATE        Gerencia a criação e execução dos arquivos de migração do DB');
  FConsoleIO.WriteInfo('');

end;

end.
