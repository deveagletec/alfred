unit Eagle.Alfred.CrudCommand;

interface
uses
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  Eagle.Alfred,
  Eagle.Alfred.Command,
  Eagle.Alfred.Attributes,
  Eagle.ConsoleIO,
  Eagle.Alfred.DprojParser;

type
  [Command('CRUD', 'Cria arquivos relacionados às operações de CRUD')]
  TCrudCommand = class(TCommand)
  private
    FModelName: string;
    FModuleName: string;
    FStringList: TStringList;

    procedure CreateView;
    procedure CreateViewModel;
    procedure CreateModel;
    procedure CreateService;
    procedure CreateRepository;

    procedure DoCreateModel(const SubLayer, Sufix, TemplateInterface, TemplateClass: string);
    procedure GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean = False);
    procedure RegisterForm(const UnitName, FormName, Path: string);
    procedure RegisterUnit(const Name, Path: string);
    function GuidCreate: string;
    procedure LogProgress(const Value: string; const NewLine: Boolean = False);
  public

    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
    destructor Destroy; override;

    [Action('CREATE', '')]
    procedure Execute(const ResourceType, ModelName, ModuleName: string);

    [Action('HELP', '')]
    procedure Help();
  end;

implementation

{ TCrudCommand }

constructor TCrudCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
begin
  inherited Create(AppPath, ConsoleIO, DprojParser);
  FStringList := TStringList.Create;
end;

procedure TCrudCommand.CreateModel;
begin
  DoCreateModel('Entity', '', 'IModel.pas', 'TModel.pas');
end;

procedure TCrudCommand.CreateRepository;
begin
  DoCreateModel('Repository', 'Repository', 'IModelRepository.pas', 'TModelRepository.pas');
end;

procedure TCrudCommand.CreateService;
begin
  DoCreateModel('Service', 'Service', 'IModelService.pas', 'TModelService.pas');
end;

procedure TCrudCommand.CreateView;
var
  Namespace, Dir: string;
begin

  Namespace := 'Eagle.ERP.' + FModuleName + '.View.' + fModelName;

  Dir := 'src\modulos\' + FModuleName.ToLower + '\view\';

  CreateDiretories(['.\' + Dir]);

  GenerateFile('View.dfm', Namespace + 'View.dfm', '.\' + Dir + Namespace + 'View.dfm');
  GenerateFile('View.pas', Namespace + 'View.pas', '.\' + Dir + Namespace + 'View.pas');

  RegisterForm(Namespace + 'View.pas', FModelName + 'View', '..\..\' + Dir + Namespace + 'View.pas');

end;

procedure TCrudCommand.CreateViewModel;
var
  Namespace, Dir, InterfacePath, ClassPath: string;
  NamespaceInterface, NamespaceClass: string;
begin

  Namespace := 'Eagle.ERP.' + FModuleName + '.ViewModel.';

  Dir := 'src\modulos\' + FModuleName.ToLower + '\viewmodel\';

  InterfacePath := Dir + Namespace + FModelName + 'ViewModel.pas';

  ClassPath := Dir + 'impl\' + Namespace + 'Impl.' + FModelName + 'ViewModel.pas';

  NamespaceInterface := Namespace + FModelName + 'ViewModel.pas';
  NamespaceClass := Namespace + 'impl.' + FModelName + 'ViewModel.pas';

  CreateDiretories(['.\' + Dir, '.\' + Dir + '\impl\']);

  GenerateFile('IViewModel.pas', NamespaceInterface, '.\' + InterfacePath, True);
  GenerateFile('TViewModel.pas', NamespaceClass, '.\' + ClassPath);

  RegisterUnit(NamespaceInterface, '..\..\' + InterfacePath);
  RegisterUnit(NamespaceClass, '..\..\' + ClassPath);

end;

destructor TCrudCommand.Destroy;
begin
  FStringList.Free;
  inherited;
end;

procedure TCrudCommand.DoCreateModel(const SubLayer, Sufix, TemplateInterface, TemplateClass: string);
var
  Namespace, Dir, InterfacePath, ClassPath, ModelNameFull: string;
  NamespaceInterface, NamespaceClass: string;
begin

  ModelNameFull := FModelName + Sufix;

  Namespace := 'Eagle.ERP.' + FModuleName + '.Model.' + SubLayer + '.';

  Dir := 'src\modulos\' + FModuleName.ToLower + '\model\' + SubLayer.ToLower + '\';

  InterfacePath := Dir + Namespace + ModelNameFull + '.pas';

  ClassPath := Dir + 'impl\' + Namespace + 'Impl.' + ModelNameFull + '.pas';

  NamespaceInterface := Namespace + ModelNameFull + '.pas';

  NamespaceClass := Namespace + 'Impl.' + ModelNameFull + '.pas';

  CreateDiretories(['.\' + Dir, '.\' + Dir + '\impl\']);

  GenerateFile(TemplateInterface, NamespaceInterface, '.\' + InterfacePath, True);
  GenerateFile(TemplateClass, NamespaceClass, '.\' + ClassPath);

  RegisterUnit(NamespaceInterface, '..\..\' + InterfacePath);
  RegisterUnit(NamespaceClass, '..\..\' + ClassPath);

end;

procedure TCrudCommand.Execute(const ResourceType, ModelName, ModuleName: string);
begin

  FModelName := Capitalize(ModelName);
  FModuleName := Capitalize(ModuleName);

  if TRegEx.IsMatch(ResourceType, '^(m|mv|mvm|mvvm|model)$') then
    CreateModel;

  if TRegEx.IsMatch(ResourceType, '^(v|mv|vvm|mvvm|view)$') then
    CreateView;

  if TRegEx.IsMatch(ResourceType, '^(v|mv|vvm|mvvm|view)$') then
    CreateViewModel;

  if TRegEx.IsMatch(ResourceType, '^(sev|service)$') then
  begin
    CreateService;
    Exit;
  end;

  if TRegEx.IsMatch(ResourceType, '^(rep|repository)$') then
  begin
    CreateRepository;
    Exit;
  end;

end;

procedure TCrudCommand.GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean);
begin

  LogProgress('Gerando: ' + UnitName + '...');

  FStringList.LoadFromFile(FAppPath + 'templates\' + Template);

  FStringList.Text := FStringList.Text.Replace('{ModelName}', FModelName, [rfReplaceAll]);
  FStringList.Text := FStringList.Text.Replace('{ModuleName}', FModuleName, [rfReplaceAll]);

  if HasGUID then
    FStringList.Text := FStringList.Text.Replace('{GUID}', GuidCreate, [rfReplaceAll]);

  FStringList.SaveToFile(FileName);

  LogProgress('Gerando: ' + UnitName + '... Done', True);

end;

function TCrudCommand.GuidCreate: string;
var
  ID: TGUID;
begin

  ID := TGUID.NewGuid;

  Result := GUIDToString(ID);

end;

procedure TCrudCommand.Help;
begin

  FConsoleIO.WriteInfo('Cria arquivos relacionados às operações de CRUD');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Para criar arquivos use:');
  FConsoleIO.WriteInfo('CREATE [tipo] [nome_do_modelo] [nome_do_modulo]');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('Tipos:');
  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('View           Cria um novo Form');
  FConsoleIO.WriteInfo('Model          Cria uma nova entidade');
  FConsoleIO.WriteInfo('ViewModel      Cria um novo ViewModel');
  FConsoleIO.WriteInfo('Repository     Cria um novo repositório');
  FConsoleIO.WriteInfo('Service        Cria um novo serviço');
  FConsoleIO.WriteInfo('MVVM           Cria os arquivos no padrão MVVM, caso não deseje gerar algum arquivo basta não informa a letra correspondente');
  FConsoleIO.WriteInfo('');

end;

procedure TCrudCommand.LogProgress(const Value: string; const NewLine: Boolean);
begin

  FConsoleIO.WriteProcess(Value);

  if NewLine then
    FConsoleIO.WriteInfo('');

end;

procedure TCrudCommand.RegisterForm(const UnitName, FormName, Path: string);
begin

  LogProgress('Registrado o formulário: ' + UnitName + '...');

  FDprojParser.AddForm(UnitName, FormName, Path);

  LogProgress('Registrado o formulário: ' + UnitName + '... Done', True);

end;

procedure TCrudCommand.RegisterUnit(const Name, Path: string);
begin

  LogProgress('Registrado a unit: ' + Name + '...');

  FDprojParser.AddUnit(Name, Path);

  LogProgress('Registrado a unit: ' + Name + '... Done', True);

end;

initialization
  TAlfred.GetInstance.Register(TCrudCommand);

end.
