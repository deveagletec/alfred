unit Eagle.Alfred.Command.Generate;

interface
uses
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  Eagle.Alfred,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes,
  Eagle.ConsoleIO,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Data,
  Eagle.Alfred.MigrateService,
  Eagle.Alfred.CodeGenerator,
  Eagle.Alfred.Generate.UnitGenerate,
  Eagle.Alfred.Generate.View,
  Eagle.Alfred.Generate.ViewModel,
  Eagle.Alfred.Generate.Entity,
  Eagle.Alfred.Generate.Service,
  Eagle.Alfred.Generate.Repository,
  Eagle.Alfred.Generate.Test;

type
  [Command('GENERATE', 'Gerador de arquivos relacionados às operações de CRUD')]
  TGenerateCommand = class(TCommand)
  private
    FCodeGenerator: ICodeGenerator;
  public

    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);

    [Action('VIEW', '')]
    procedure CreateView(const ModelName, ModuleName: string; const Simple: Boolean);

    [Action('VIEWMODEL', '')]
    procedure CreateViewModel(const ModelName, ModuleName: string; const IgnoreTest, Simple: Boolean);

    [Action('MODEL', '')]
    procedure CreateModel(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('SERVICE', '')]
    procedure CreateService(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('REPOSITORY', '')]
    procedure CreateRepository(const ModelName, ModuleName: string; const Simple: Boolean);

    [Action('CRUD', '')]
    procedure CreateCRUD(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('TEST', '')]
    procedure CreateTEST(const LayerName, ModelName, ModuleName: string);

    [Action('MIGRATE', '')]
    procedure CreateMigrate(const Name: string);

    [Action('HELP', '')]
    procedure Help();
  end;

implementation

{ TCrudCommand }

constructor TGenerateCommand.Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
begin
  inherited Create(AppPath, APackage, ConsoleIO);

  FCodeGenerator := TCodeGenerator.Create(AppPath, APackage);

end;

procedure TGenerateCommand.CreateCRUD(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateView(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created View');

  FCodeGenerator.GenerateViewModel(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created ViewModel');

  FCodeGenerator.GenerateModel(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created Model');

  if IgnoreTest then
    Exit;

  FCodeGenerator.GenerateTest(ModuleName, 'Entity', ModelName);

  FConsoleIO.WriteInfo('Created Model Test');

  FCodeGenerator.GenerateTest(ModuleName, 'ViewModel', ModelName);

  FConsoleIO.WriteInfo('Created ViewModel Test');

end;

procedure TGenerateCommand.CreateMigrate(const Name: string);
var
  MigrateService: IMigrateService;
begin

  MigrateService := TMigrateService.Create(FPackage);

  MigrateService.Generate(Name);

  FConsoleIO.WriteInfo('Create migrate');

end;

procedure TGenerateCommand.CreateModel(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateModel(ModuleName, ModelName);

  if IgnoreTest then
    Exit;

  FCodeGenerator.GenerateTest(ModuleName, 'Entity', ModelName);

end;

procedure TGenerateCommand.CreateRepository(const ModelName, ModuleName: string; const Simple: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateRepository(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created Repository');

end;

procedure TGenerateCommand.CreateService(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateService(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created Service');

  if IgnoreTest then
    Exit;

  FCodeGenerator.GenerateTest(ModuleName, 'Service', ModelName);

  FConsoleIO.WriteInfo('Created Service Test');

end;

procedure TGenerateCommand.CreateTEST(const LayerName, ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateTest(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created Test');

end;

procedure TGenerateCommand.CreateView(const ModelName, ModuleName: string; const Simple: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateView(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created View');

end;

procedure TGenerateCommand.CreateViewModel(const ModelName, ModuleName: string; const IgnoreTest, Simple: Boolean);
begin

  CheckProjectConfiguration;

  FCodeGenerator.GenerateViewModel(ModuleName, ModelName);

  FConsoleIO.WriteInfo('Created ViewModel');

  if IgnoreTest then
    Exit;

  FCodeGenerator.GenerateTest(ModuleName, 'ViewModel', ModelName);

  FConsoleIO.WriteInfo('Created ViewModel Test');

end;

procedure TGenerateCommand.Help;
begin

  FConsoleIO.WriteInfo('Cria arquivos relacionados às operações de CRUD');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Para criar arquivos use:');
  FConsoleIO.WriteInfo('GENERATE [tipo] [nome_do_modelo] [nome_do_modulo]');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Tipos:');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('View           Cria um novo Form');
  FConsoleIO.WriteInfo('Model          Cria uma nova entidade');
  FConsoleIO.WriteInfo('ViewModel      Cria um novo ViewModel');
  FConsoleIO.WriteInfo('Repository     Cria um novo repositório');
  FConsoleIO.WriteInfo('Service        Cria um novo serviço');
  FConsoleIO.WriteInfo('CRUD           Cria os arquivos no padrão MVVM, caso não deseje gerar algum arquivo basta não informa a letra correspondente');
  FConsoleIO.WriteInfo('Test           Cria um novo teste');
  FConsoleIO.WriteInfo('Migrate        Cria um novo migrate');
  FConsoleIO.WriteInfo('');

end;

initialization
  TAlfred.GetInstance.Register(TGenerateCommand);

end.
