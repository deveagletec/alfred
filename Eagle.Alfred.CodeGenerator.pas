unit Eagle.Alfred.CodeGenerator;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Data,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Utils;

type

  ICodeGenerator = interface
    ['{A00AFE2B-290C-4515-A1AD-C923D25BB6D8}']
    procedure GenerateView(const ModuleName, ModelName: string);
    procedure GenerateViewModel(const ModuleName, ModelName: string);
    procedure GenerateModel(const ModuleName, ModelName: string);
    procedure GenerateService(const ModuleName, ModelName: string);
    procedure GenerateRepository(const ModuleName, ModelName: string);
    procedure GenerateTest(const ModuleName, LayerName, ModelName: string);
  end;

  TCodeGenerator = class(TInterfacedObject, ICodeGenerator)
  private
    FAppPath: string;
    FPackage: TPackage;
    FModuleName: string;
    FLayerName: string;
    FModelName: string;
    FClassName: string;
    FNamespace: string;
    FFilePath: string;
    FSourceDir: string;

    FDprojParser : IDprojParser;
    FDprojTestParser : IDprojParser;

    procedure MountNamespaceAndFilePath;
    procedure MountClassName;
    procedure Prepare(const ModuleName, LayerName, ModelName: string);
    procedure GenerateFile(const Template, UnitName, FileName: string);

    procedure DoGenerateModel(const InterfaceTemplate, ClassTemplate: string);
    procedure DoGenerateView;
    procedure DoGenerateTest;

  public
    constructor Create(const AppPath: string; APackage: TPackage);
    destructor Destroy; override;
    procedure GenerateView(const ModuleName, ModelName: string);
    procedure GenerateViewModel(const ModuleName, ModelName: string);
    procedure GenerateModel(const ModuleName, ModelName: string);
    procedure GenerateService(const ModuleName, ModelName: string);
    procedure GenerateRepository(const ModuleName, ModelName: string);
    procedure GenerateTest(const ModuleName, LayerName, ModelName: string);
  end;

implementation

{ TCodeGenerator }

constructor TCodeGenerator.Create(const AppPath: string; APackage: TPackage);
begin

  FAppPath := AppPath;
  FPackage := APackage;

  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id);

  FDprojTestParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id + 'Test');

end;

destructor TCodeGenerator.Destroy;
begin
  FDprojParser.Save;
  FDprojTestParser.Save;
  inherited;
end;

procedure TCodeGenerator.DoGenerateModel(const InterfaceTemplate, ClassTemplate: string);
var
  BaseDir, InterfaceName, ClassName: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir, BaseDir + 'impl\']);

  InterfaceName := FNamespace + FClassName + '.pas';
  ClassName := FNamespace + 'Impl.' + FClassName + '.pas';

  GenerateFile(InterfaceTemplate, InterfaceName, BaseDir + InterfaceName);
  GenerateFile(ClassTemplate, ClassName, BaseDir + 'impl\' + ClassName);

  FDprojParser.AddUnit(InterfaceName, '..\..\' + FFilePath + InterfaceName);
  FDprojParser.AddUnit(ClassName, '..\..\' + FFilePath + 'impl\' + ClassName);

  FDprojTestParser.AddPathInUnitSearchPath('..\..\' + FFilePath);
  FDprojTestParser.AddPathInUnitSearchPath('..\..\' + FFilePath + 'impl\');

end;

procedure TCodeGenerator.DoGenerateTest;
var
  BaseDir, FileName: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir]);

  FileName := FNamespace + FClassName + 'Test.pas';

  GenerateFile('T' + FLayerName + 'Test.pas', FileName, BaseDir + FileName);

  FDprojTestParser.AddUnit(FileName, '..\..\' + FFilePath + FileName);

end;

procedure TCodeGenerator.DoGenerateView;
var
  BaseDir, UnitName, ViewName: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir]);

  UnitName := FNamespace + FClassName + 'View.pas';

  ViewName := FNamespace + FClassName + 'View.dfm';

  GenerateFile('View.dfm', ViewName, BaseDir + ViewName);
  GenerateFile('View.pas', UnitName, BaseDir + UnitName);

  FDprojParser.AddForm(UnitName, FModelName + 'View', '..\..\' + FFilePath + UnitName);

end;

procedure TCodeGenerator.GenerateFile(const Template, UnitName, FileName: string);
var
  FStringList: TStringList;
begin

  FStringList := TStringList.Create;

  try

    FStringList.LoadFromFile(FAppPath + 'templates\' + Template);

    FStringList.Text := FStringList.Text.Replace('{ModelName}', FModelName, [rfReplaceAll]);
    FStringList.Text := FStringList.Text.Replace('{ModuleName}', FModuleName, [rfReplaceAll]);
    FStringList.Text := FStringList.Text.Replace('{LayerName}', FLayerName, [rfReplaceAll]);
    FStringList.Text := FStringList.Text.Replace('{GUID}', GuidCreate, [rfReplaceAll]);

    FStringList.SaveToFile(FileName);

  finally
    FStringList.Free;
  end;

end;

procedure TCodeGenerator.GenerateModel(const ModuleName, ModelName: string);
begin

  FSourceDir := FPackage.SourceDir;

  Prepare(ModuleName, 'Entity', ModelName);

  DoGenerateModel('IModel.pas', 'TModel.pas');

end;

procedure TCodeGenerator.GenerateRepository(const ModuleName, ModelName: string);
begin

  FSourceDir := FPackage.SourceDir;

  Prepare(ModuleName, 'Repository', ModelName);

  DoGenerateModel('IModelRepository.pas', 'TModelRepository.pas');

end;

procedure TCodeGenerator.GenerateService(const ModuleName, ModelName: string);
begin

  FSourceDir := FPackage.SourceDir;

  Prepare(ModuleName, 'Service', ModelName);

  DoGenerateModel('IModelService.pas', 'TModelService.pas');

end;

procedure TCodeGenerator.GenerateTest(const ModuleName, LayerName, ModelName: string);
begin

  FSourceDir := FPackage.TestsDir;

  Prepare(ModuleName, LayerName, ModelName);

  DoGenerateTest;

end;

procedure TCodeGenerator.GenerateView(const ModuleName, ModelName: string);
begin

  FSourceDir := FPackage.SourceDir;

  Prepare(ModuleName, 'View', ModelName);

  DoGenerateView;

end;

procedure TCodeGenerator.GenerateViewModel(const ModuleName, ModelName: string);
begin

  FSourceDir := FPackage.SourceDir;

  Prepare(ModuleName, 'ViewModel', ModelName);

  DoGenerateModel('IViewModel.pas', 'TViewModel.pas');

end;

procedure TCodeGenerator.MountClassName;
begin

  FClassName := FModelName;

  if AnsiMatchText(FLayerName, ['viewmodel', 'service', 'repository']) then
    FClassName := FClassName + FLayerName;

end;

procedure TCodeGenerator.MountNamespaceAndFilePath;
var
  ModuleName, ModuleDir, LayerName, LayerDir: string;
begin

  if FPackage.Modular then
  begin
    ModuleName := FModuleName + '.';
    ModuleDir := 'modulos\' + FModuleName.ToLower + '\';
  end
  else
  begin
    ModuleName := EmptyStr;
    ModuleDir := EmptyStr;
  end;

  LayerName := FLayerName + '.';
  LayerDir := FLayerName.ToLower + '\';

  if AnsiMatchText(FLayerName, ['entity', 'service', 'repository']) then
  begin
    LayerName := 'Model.' + LayerName;
    LayerDir := 'model\' + LayerDir;
  end;

  FNamespace := FPackage.AppNamespace + ModuleName + LayerName;

  FFilePath := FSourceDir + ModuleDir + LayerDir;

end;

procedure TCodeGenerator.Prepare(const ModuleName, LayerName, ModelName: string);
begin

  FModuleName := Capitalize(ModuleName.Replace('.', ''));
  FLayerName := Capitalize(LayerName.Replace('.', ''));
  FModelName := Capitalize(ModelName.Replace('.', ''));

  MountNamespaceAndFilePath;

  MountClassName;

end;

end.
