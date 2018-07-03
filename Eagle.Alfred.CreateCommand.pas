unit Eagle.Alfred.CreateCommand;

interface
uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  System.RegularExpressions,
  Eagle.ConsoleIO,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Command;

type

  ECreateCommandException = class(Exception);

  TCreateCommand = class(TInterfacedObject, ICommand)
  private
    FAppPath: string;
    FCommand: string;
    FModelName: string;
    FModuleName: string;
    FStringList: TStringList;
    FDprojParser: TDprojParser;
    FConsoleIO: IConsoleIO;

    procedure CreateView;
    procedure CreateViewModel;
    procedure CreateModel;
    procedure CreateRepository;
    procedure CreateService;
    procedure CreateTest;
    procedure DoCreateModel(const SubLayer, Sufix, TemplateInterface,
        TemplateClass: string);
    procedure Parse;
    procedure GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean = False);
    procedure LogInfo(const Value: string);
    procedure LogError(const Value: string);
    procedure LogProgress(const Value: string; const NewLine: Boolean = False);
    procedure RegisterForm(const UnitName, FormName, Path: string);
    procedure RegisterUnit(const Name, Path: string);
    procedure CreateDiretories(const Paths: array of string);
    function Capitalize(const Str: string): string;
    function GuidCreate: string;
  public
    constructor Create(const AppPath: string; ConsoleIO: IConsoleIO; DprojParser: TDprojParser);
    destructor Destroy; override;
    procedure Execute;
    procedure Help;
  end;

implementation

{ TCreateCommand }

function TCreateCommand.Capitalize(const Str: string): string;
var
  flag: Boolean;
  i: Byte;
  s: string;
begin

  flag := True;
  s := Str.ToLower;
  Result := EmptyStr;

  for i := 1 to Length(s) do
  begin

    if flag then
      Result := Result + AnsiUpperCase(s[i])
    else
      Result := Result + s[i];

    flag := (s[i] in [' ', '[',']', '(', ')']);
  end;

end;

constructor TCreateCommand.Create(const AppPath: string; ConsoleIO: IConsoleIO;
    DprojParser: TDprojParser);
begin
  FStringList := TStringList.Create;
  FDprojParser := DprojParser;
  FAppPath := AppPath;
  FConsoleIO := ConsoleIO;
end;

procedure TCreateCommand.CreateDiretories(const Paths: array of string);
var
  Path: string;
begin

  for Path in Paths do
  begin
    if not DirectoryExists(Path) then
      ForceDirectories(Path);
  end;

end;

procedure TCreateCommand.CreateModel;
begin

  if not TRegEx.IsMatch(FCommand, '^(m|mv|mvm|mvvm|model)$') then
    Exit;

  DoCreateModel('Entity', '', 'IModel.pas', 'TModel.pas');

end;

procedure TCreateCommand.CreateRepository;
begin

  if not TRegEx.IsMatch(FCommand, '^(rep|repository)$') then
    Exit;

  DoCreateModel('Repository', 'Repository', 'IModelRepository.pas', 'TModelRepository.pas');

end;

procedure TCreateCommand.CreateService;
begin

  if not TRegEx.IsMatch(FCommand, '^(sev|service)$') then
    Exit;

  DoCreateModel('Service', 'Service', 'IModelService.pas', 'TModelService.pas');

end;

procedure TCreateCommand.CreateTest;
begin

end;

procedure TCreateCommand.CreateView;
var
  Namespace, Dir: string;
begin

  if not TRegEx.IsMatch(FCommand, '^(v|mv|vvm|mvvm|view)$') then
    Exit;

  Namespace := 'Eagle.ERP.' + FModuleName + '.View.' + FModelName;

  Dir := 'src\modulos\' + FModuleName.ToLower + '\view\';

  CreateDiretories(['.\' + Dir]);

  GenerateFile('View.dfm', Namespace + 'View.dfm', '.\' + Dir + Namespace + 'View.dfm');
  GenerateFile('View.pas', Namespace + 'View.pas', '.\' + Dir + Namespace + 'View.pas');

  RegisterForm(Namespace + 'View.pas', FModelName + 'View', '..\..\' + Dir + Namespace + 'View.pas');

end;

procedure TCreateCommand.CreateViewModel;
var
  Namespace, Dir, InterfacePath, ClassPath: string;
  NamespaceInterface, NamespaceClass: string;
begin

  if not TRegEx.IsMatch(FCommand, '^(vm|vvm|mvm|mvvm|viewmodel)$') then
    Exit;

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

destructor TCreateCommand.Destroy;
begin

  if Assigned(FStringList) then
    FStringList.Free;

  inherited;
end;

procedure TCreateCommand.DoCreateModel(const SubLayer, Sufix, TemplateInterface, TemplateClass: string);
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

procedure TCreateCommand.Execute;
begin

  try

    Parse;

    CreateModel;

    CreateView;

    CreateViewModel;

    CreateRepository;

    CreateService;

    CreateTest;

    FDprojParser.Save;

    Help;

  except
    on E: ECreateCommandException do
      FConsoleIO.WriteError(E.Message);
  end;

end;

procedure TCreateCommand.GenerateFile(const Template, UnitName, FileName: string; const HasGUID: Boolean = False);
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

function TCreateCommand.GuidCreate: string;
var
  ID: TGUID;
begin

  ID := TGUID.NewGuid;

  Result := GUIDToString(ID);

end;

procedure TCreateCommand.Help;
begin

   if not FCommand.Equals('help') then
    Exit;

  FConsoleIO.WriteInfo('Cria arquivos relacionados às operções de CRUD');
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

procedure TCreateCommand.LogError(const Value: string);
begin
  if Assigned(FConsoleIO) then
    FConsoleIO.WriteError(Value);
end;

procedure TCreateCommand.LogInfo(const Value: string);
begin
  if Assigned(FConsoleIO) then
    FConsoleIO.WriteInfo(Value);
end;

procedure TCreateCommand.LogProgress(const Value: string; const NewLine: Boolean = False);
begin
  if not Assigned(FConsoleIO) then
    Exit;

  FConsoleIO.WriteProcess(Value);

  if NewLine then
    FConsoleIO.WriteInfo('');

end;

procedure TCreateCommand.Parse;
begin

  FCommand := ParamStr(2).ToLower;

  FModelName := Capitalize(ParamStr(3));

  FModuleName := Capitalize(ParamStr(4));

end;

procedure TCreateCommand.RegisterForm(const UnitName, FormName, Path: string);
begin

  LogProgress('Registrado o formulário: ' + UnitName + '...');

  FDprojParser.AddForm(UnitName, FormName, Path);

  LogProgress('Registrado o formulário: ' + UnitName + '... Done', True);

end;

procedure TCreateCommand.RegisterUnit(const Name, Path: string);
begin

  LogProgress('Registrado a unit: ' + Name + '...');

  FDprojParser.AddUnit(Name, Path);

  LogProgress('Registrado a unit: ' + Name + '... Done', True);

end;

end.
