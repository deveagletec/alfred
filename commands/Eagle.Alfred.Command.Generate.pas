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
  Eagle.Alfred.Generate.UnitGenerate,
  Eagle.Alfred.Generate.View,
  Eagle.Alfred.Generate.ViewModel,
  Eagle.Alfred.Generate.Entity,
  Eagle.Alfred.Generate.Service,
  Eagle.Alfred.Generate.Repository,
  Eagle.Alfred.Generate.Test;

type
  [Command('GENERATE', 'Cria arquivos relacionados às operações de CRUD')]
  TGenerateCommand = class(TCommand)
  private
    procedure DoCreateView(const ModuleName, LayerName, ModelName: string);
    procedure DoCreateViewModel(const ModuleName, LayerName, ModelName: string);
    procedure DoCreateService(const ModuleName, LayerName, ModelName: string);
    procedure DoCreateRepository(const ModuleName, LayerName, ModelName: string);
    procedure DoCreateTest(const ModuleName, LayerName, ModelName: string);
    procedure DoCreateModel(const ModuleName, LayerName, ModelName: string);

  public

    [Action('VIEW', '')]
    procedure CreateView(const ModelName, ModuleName: string);

    [Action('VIEWMODEL', '')]
    procedure CreateViewModel(const ModelName, ModuleName: string);

    [Action('MODEL', '')]
    procedure CreateModel(const ModelName, ModuleName: string);

    [Action('SERVICE', '')]
    procedure CreateService(const ModelName, ModuleName: string);

    [Action('REPOSITORY', '')]
    procedure CreateRepository(const ModelName, ModuleName: string);

    [Action('CRUD', '')]
    procedure CreateCRUD(const ModelName, ModuleName: string);

    [Action('TEST', '')]
    procedure CreateTEST(const LayerName, ModelName, ModuleName: string);

    [Action('MIGRATE', '')]
    procedure CreateMigrate(const Name: string);

    [Action('HELP', '')]
    procedure Help();
  end;

implementation

{ TCrudCommand }

procedure TGenerateCommand.CreateCRUD(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateView(ModuleName, 'View', ModelName);

  DoCreateViewModel(ModuleName, 'ViewModel', ModelName);

  DoCreateModel(ModuleName, 'Entity', ModelName);

end;

procedure TGenerateCommand.CreateMigrate(const Name: string);
var
  MigrateService: IMigrateService;
begin

  MigrateService := TMigrateService.Create(FPackage);

  MigrateService.Generate(Name);

  FConsoleIO.WriteInfo('Create migrate');

end;

procedure TGenerateCommand.CreateModel(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateModel(ModuleName, 'Entity', ModelName);

//  if IgnoreTest then
 //   Exit;

 // DoCreateTest(ModuleName, 'Entity', ModelName);

end;

procedure TGenerateCommand.CreateRepository(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateRepository(ModuleName, 'Repository', ModelName);

end;

procedure TGenerateCommand.CreateService(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateService(ModuleName, 'Service', ModelName);

  //if IgnoreTest then
    Exit;

  DoCreateTest(ModuleName, 'Service', ModelName);

end;

procedure TGenerateCommand.CreateTEST(const LayerName, ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateTest(ModuleName, LayerName, ModelName);

end;

procedure TGenerateCommand.CreateView(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateView(ModuleName, 'View', ModelName);

end;

procedure TGenerateCommand.CreateViewModel(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  DoCreateViewModel(ModuleName, 'ViewModel', ModelName);

  //if IgnoreTest then
    Exit;

  DoCreateTest(ModuleName, 'ViewModel', ModelName);

end;

procedure TGenerateCommand.DoCreateRepository(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TRepositoryGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created Repository');

end;

procedure TGenerateCommand.DoCreateService(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TServiceGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created Service');

end;

procedure TGenerateCommand.DoCreateTest(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TTestGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

end;

procedure TGenerateCommand.DoCreateView(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TViewGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created View');

end;

procedure TGenerateCommand.DoCreateViewModel(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TViewModelGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created ViewModel');

end;

procedure TGenerateCommand.DoCreateModel(const ModuleName, LayerName, ModelName: string);
var
  Generate: IUnitGenerate;
begin

  Generate := TEntityGenerate.Create(FAppPath, FPackage);

  Generate.Execute(ModuleName, LayerName, ModelName);

  FConsoleIO.WriteInfo('Created Model');

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
