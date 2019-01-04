unit Eagle.Alfred.Command.Migrate;

interface
uses
  System.DateUtils,
  System.IOUtils,
  System.SysUtils,
  Eagle.Alfred,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes;

type
  [Command('db', 'MIGRATE', 'Cria arquivos de migração de versão do banco de dados')]
  TMigrateCommand = class(TCommand)
  public
    [Action('CREATE', 'Cria um novo arquivo de migração')]
    procedure CreateNew(const Name: string);

    [Action('EXECUTE', '')]
    procedure Execute;

    [Action('GENERATE', '')]
    procedure Generate;

    [Action('HELP', '')]
    procedure Help;
  end;

implementation

{ TMigrateCommand }

procedure TMigrateCommand.CreateNew(const Name: string);
var
  FileName, TimeStamp: string;
begin

  CheckProjectConfiguration;

  TimeStamp := DateTimeToUnix(Now).ToString;

  FileName := Name + '_' + TimeStamp + '.sql';

  Tfile.WriteAllText(FileName, 'INSERT INTO MIGRATIONS (ID) VALUES (' + TimeStamp + '); ');

end;

procedure TMigrateCommand.Execute;
begin

  CheckProjectConfiguration;

end;

procedure TMigrateCommand.Generate;
begin

  CheckProjectConfiguration;

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

initialization
  TAlfred.GetInstance.Register(TMigrateCommand);

end.
