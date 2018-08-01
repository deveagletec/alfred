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
  Eagle.Alfred.MigrateService;

type
  [Command('GENERATE', 'Cria arquivos relacionados às operações de CRUD')]
  TGenerateCommand = class(TCommand)
  private
    FModelName: string;
    FModuleName: string;
    FLayerName: string;
    FStringList: TStringList;
    FDprojParser: IDprojParser;

    procedure DoCreateView;
    procedure DoCreateViewModel;
    procedure DoCreateService;
    procedure DoCreateRepository;
    procedure DoCreateTest;

    procedure DoCreateModel(const SubLayer, Sufix, TemplateInterface, TemplateClass: string);
    procedure GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean = False);
    procedure RegisterForm(const UnitName, FormName, Path: string);
    procedure RegisterUnit(const Name, Path: string);
    function GuidCreate: string;
    procedure LogProgress(const Value: string; const NewLine: Boolean = False);
  public

    constructor Create(const AppPath: string; APackage: TPackage; ConsoleIO: IConsoleIO);
    destructor Destroy; override;

    [Action('VIEW', '')]
    procedure CreateView(const ModelName, ModuleName: string);

    [Action('VIEWMODEL', '')]
    procedure CreateViewModel(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('MODEL', '')]
    procedure CreateModel(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('SERVICE', '')]
    procedure CreateService(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('REPOSITORY', '')]
    procedure CreateRepository(const ModelName, ModuleName: string);

    [Action('CRUD', '')]
    procedure CreateCRUD(const ModelName, ModuleName: string; const IgnoreTest: Boolean);

    [Action('TEST', '')]
    procedure CreateTEST(const LayerName, ClassName, ModuleName: string);

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

  FDprojParser := TDprojParser.Create(FPackage.DataBase + FPackage.PackagesDir, FPackage.Id);

  FStringList := TStringList.Create;

end;

procedure TGenerateCommand.CreateCRUD(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateView;

  DoCreateViewModel;

  DoCreateModel('Entity', '', 'IModel.pas', 'TModel.pas');

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

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateModel('Entity', '', 'IModel.pas', 'TModel.pas');

  if IgnoreTest then
    Exit;

  FLayerName := 'Entity';

  DoCreateTest;

end;

procedure TGenerateCommand.CreateRepository(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateRepository;

end;

procedure TGenerateCommand.CreateService(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateService;

  if IgnoreTest then
    Exit;

  FLayerName := 'Service';

  DoCreateTest;

end;

procedure TGenerateCommand.CreateTEST(const LayerName, ClassName, ModuleName: string);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ClassName);
  FModuleName := Capitalize(ModuleName);
  FLayerName := Capitalize(LayerName);

  DoCreateTest;

end;

procedure TGenerateCommand.CreateView(const ModelName, ModuleName: string);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateView;

end;

procedure TGenerateCommand.CreateViewModel(const ModelName, ModuleName: string; const IgnoreTest: Boolean);
begin

  CheckProjectConfiguration;

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  DoCreateViewModel;

  if IgnoreTest then
    Exit;

  FLayerName := 'ViewModel';

  DoCreateTest;

end;

procedure TGenerateCommand.DoCreateRepository;
begin
  DoCreateModel('Repository', 'Repository', 'IModelRepository.pas', 'TModelRepository.pas');
end;

procedure TGenerateCommand.DoCreateService;
begin
  DoCreateModel('Service', 'Service', 'IModelService.pas', 'TModelService.pas');
end;

procedure TGenerateCommand.DoCreateTest;
var
  Namespace, BaseDir, Dir, ModelName, ModuleName, ModulesDir: string;
begin

  if FPackage.Modular then
  begin
    ModuleName := FModuleName + '.';
    ModulesDir := 'modulos\' + FModuleName.ToLower + '\';
  end
  else
  begin
    ModuleName := EmptyStr;
    ModulesDir := EmptyStr;
  end;

  if FLayerName.ToLower.Equals('entity') then
    ModelName := FModelName
  else
    if FLayerName.ToLower.Equals('viewmodel') then
      ModelName := FModelName + FLayerName
    else
      ModelName := FModelName + '.Model.' + FLayerName;

  Namespace := FPackage.AppNamespace + ModuleName + FLayerName + '.' + ModelName;

  Dir := FPackage.TestsDir + ModulesDir + FLayerName.ToLower + '\';
  BaseDir := FPackage.BaseDir + Dir;

  CreateDiretories([BaseDir]);

  GenerateFile('T' + FLayerName + 'Test.pas', Namespace + 'Test.pas', BaseDir + Namespace + 'Test.pas');


end;

procedure TGenerateCommand.DoCreateView;
var
  Namespace, BaseDir, Dir, ModuleName, ModulesDir: string;
begin

  if FPackage.Modular then
  begin
    ModuleName := FModuleName + '.';
    ModulesDir := 'modulos\' + FModuleName.ToLower + '\';
  end
  else
  begin
    ModuleName := EmptyStr;
    ModulesDir := EmptyStr;
  end;

  Namespace := FPackage.AppNamespace + ModuleName + 'View.' + fModelName;

  Dir := FPackage.SourceDir + ModulesDir + 'view\';
  BaseDir := FPackage.BaseDir + Dir;

  CreateDiretories([BaseDir]);

  GenerateFile('View.dfm', Namespace + 'View.dfm', BaseDir + Namespace + 'View.dfm');
  GenerateFile('View.pas', Namespace + 'View.pas', BaseDir + Namespace + 'View.pas');

  RegisterForm(Namespace + 'View.pas', FModelName + 'View', '..\..\' + Dir + Namespace + 'View.pas');

end;

procedure TGenerateCommand.DoCreateViewModel;
var
  Namespace, BaseDir, Dir, InterfacePath, ClassPath: string;
  NamespaceInterface, NamespaceClass: string;
begin

  Namespace := FPackage.AppNamespace + FModuleName + '.ViewModel.';

  Dir := 'src\modulos\' + FModuleName.ToLower + '\viewmodel\';

  BaseDir := FPackage.BaseDir + Dir;

  InterfacePath := Dir + Namespace + FModelName + 'ViewModel.pas';

  ClassPath := Dir + 'impl\' + Namespace + 'Impl.' + FModelName + 'ViewModel.pas';

  NamespaceInterface := Namespace + FModelName + 'ViewModel.pas';
  NamespaceClass := Namespace + 'impl.' + FModelName + 'ViewModel.pas';

  CreateDiretories([BaseDir, BaseDir + '\impl\']);

  GenerateFile('IViewModel.pas', NamespaceInterface, FPackage.BaseDir + InterfacePath, True);
  GenerateFile('TViewModel.pas', NamespaceClass, FPackage.BaseDir + ClassPath);

  RegisterUnit(NamespaceInterface, '..\..\' + InterfacePath);
  RegisterUnit(NamespaceClass, '..\..\' + ClassPath);

end;

destructor TGenerateCommand.Destroy;
begin
  FStringList.Free;
  inherited;
end;

procedure TGenerateCommand.DoCreateModel(const SubLayer, Sufix, TemplateInterface, TemplateClass: string);
var
  Namespace, BaseDir, Dir, InterfacePath, ClassPath, ModelNameFull: string;
  NamespaceInterface, NamespaceClass: string;
begin

  ModelNameFull := FModelName + Sufix;

  Namespace := FPackage.AppNamespace + FModuleName + '.Model.' + SubLayer + '.';

  Dir := 'src\modulos\' + FModuleName.ToLower + '\model\' + SubLayer.ToLower + '\';

  BaseDir := FPackage.BaseDir + Dir;

  InterfacePath := Dir + Namespace + ModelNameFull + '.pas';

  ClassPath := Dir + 'impl\' + Namespace + 'Impl.' + ModelNameFull + '.pas';

  NamespaceInterface := Namespace + ModelNameFull + '.pas';

  NamespaceClass := Namespace + 'Impl.' + ModelNameFull + '.pas';

  CreateDiretories([BaseDir, BaseDir + '\impl\']);

  GenerateFile(TemplateInterface, NamespaceInterface, FPackage.BaseDir + InterfacePath, True);
  GenerateFile(TemplateClass, NamespaceClass, FPackage.BaseDir + ClassPath);

  RegisterUnit(NamespaceInterface, '..\..\' + InterfacePath);
  RegisterUnit(NamespaceClass, '..\..\' + ClassPath);

end;

procedure TGenerateCommand.GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean);
begin

  LogProgress('Gerando: ' + UnitName + '...');

  FStringList.LoadFromFile(FAppPath + 'templates\' + Template);

  FStringList.Text := FStringList.Text.Replace('{ModelName}', FModelName, [rfReplaceAll]);
  FStringList.Text := FStringList.Text.Replace('{ModuleName}', FModuleName, [rfReplaceAll]);
  FStringList.Text := FStringList.Text.Replace('{LayerName}', FLayerName, [rfReplaceAll]);

  if HasGUID then
    FStringList.Text := FStringList.Text.Replace('{GUID}', GuidCreate, [rfReplaceAll]);

  FStringList.SaveToFile(FileName);

  LogProgress('Gerando: ' + UnitName + '... Done', True);

end;

function TGenerateCommand.GuidCreate: string;
var
  ID: TGUID;
begin

  ID := TGUID.NewGuid;

  Result := GUIDToString(ID);

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

procedure TGenerateCommand.LogProgress(const Value: string; const NewLine: Boolean);
begin

  FConsoleIO.WriteProcess(Value);

  if NewLine then
    FConsoleIO.WriteInfo('');

end;

procedure TGenerateCommand.RegisterForm(const UnitName, FormName, Path: string);
begin

  LogProgress('Registrado o formulário: ' + UnitName + '...');

  FDprojParser.AddForm(UnitName, FormName, Path);

  LogProgress('Registrado o formulário: ' + UnitName + '... Done', True);

end;

procedure TGenerateCommand.RegisterUnit(const Name, Path: string);
begin

  LogProgress('Registrado a unit: ' + Name + '...');

  FDprojParser.AddUnit(Name, Path);

  LogProgress('Registrado a unit: ' + Name + '... Done', True);

end;

initialization
  TAlfred.GetInstance.Register(TGenerateCommand);

end.
