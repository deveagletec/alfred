unit Eagle.Alfred.Core.CodeGenerator;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Command.Common.DprojParser,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Core.Exceptions;

type

  ICodeGenerator = interface
    ['{A00AFE2B-290C-4515-A1AD-C923D25BB6D8}']
    procedure GenerateView(const ModuleName, ModelName: string);
    procedure GenerateViewModel(const ModuleName, ModelName: string);
    procedure GenerateModel(const ModuleName, ModelName: string);
    procedure GenerateService(const ModuleName, ModelName: string);
    procedure GenerateRepository(const ModuleName, ModelName: string);
    procedure GenerateTest(const ModuleName, LayerName, ModelName: string);
    procedure SetForceGenerate(const Value: Boolean);
    procedure SetTemplateName(const Name: string);
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
    FForce: Boolean;
    FTemplateName: string;

    FDprojParser : IDprojParser;
    FDprojTestParser : IDprojParser;
    FCoveragePaths: TStringList;
    FCoveragePathsFileName: string;
    FCoverageUnits: TStringList;
    FCoverageUnitsFileName: string;

    procedure AddUnitToCoverage(ClassName: string; const Path: string);
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
    procedure SetForceGenerate(const Value: Boolean);
    procedure SetTemplateName(const Name: string);
  end;

implementation

{ TCodeGenerator }

constructor TCodeGenerator.Create(const AppPath: string; APackage: TPackage);
begin
  FAppPath := AppPath;
  FPackage := APackage;

  FCoveragePathsFileName := FPackage.CoverageConfigDir + 'dcov_paths.lst';
  FCoverageUnitsFileName := FPackage.CoverageConfigDir + 'dcov_units.lst';

  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Name);

  FDprojTestParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Name + 'Test');

  FCoveragePaths := TStringList.Create;
  FCoverageUnits := TStringList.Create;

  if FileExists(FCoveragePathsFileName) then
    FCoveragePaths.LoadFromFile(FCoveragePathsFileName);

  if FileExists(FCoverageUnitsFileName) then
    FCoverageUnits.LoadFromFile(FCoverageUnitsFileName);
end;

destructor TCodeGenerator.Destroy;
begin
  if Assigned(FDprojParser) then
    FDprojParser.Save;
  if Assigned(FDprojTestParser) then
    FDprojTestParser.Save;

  if Assigned(FCoveragePaths) then
  begin
    FCoveragePaths.SaveToFile(FCoveragePathsFileName);
    FCoveragePaths.Free;
  end;

  if Assigned(FCoverageUnits) then
  begin
    FCoverageUnits.SaveToFile(FCoverageUnitsFileName);
    FCoverageUnits.Free;
  end;

  inherited;
end;

procedure TCodeGenerator.AddUnitToCoverage(ClassName: string; const Path: string);
begin
  if FCoveragePaths.IndexOf(Path) < 0 then
    FCoveragePaths.Add(Path);

  if FCoverageUnits.IndexOf(ClassName) < 0 then
    FCoverageUnits.Add(ClassName);
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

  FDprojTestParser.AddPathInReleaseUnitSearchPath('..\..\' + FFilePath);
  FDprojTestParser.AddPathInReleaseUnitSearchPath('..\..\' + FFilePath + 'impl\');

  AddUnitToCoverage(FNamespace + 'Impl.' + FClassName, '..\..\..\' + FFilePath + 'impl');
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
  TemplatePath: string;
begin
  if FileExists(FileName) and not FForce then
    raise EFileAlwaysExistsException.Create('File ' + FileName.QuotedString + ' already exists');

  TemplatePath := FAppPath + 'templates\' + FTemplateName + '\' + Template;

  if not FileExists(TemplatePath) then
    raise EFileNotFoundException.Create('Template File ' + TemplatePath.QuotedString + ' not found');

  FStringList := TStringList.Create;

  try

    FStringList.LoadFromFile(TemplatePath);

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

  FNamespace := FPackage.Namespace + '.' + ModuleName + LayerName;

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

procedure TCodeGenerator.SetForceGenerate(const Value: Boolean);
begin
  FForce := Value;
end;

procedure TCodeGenerator.SetTemplateName(const Name: string);
begin
  FTemplateName := Name;
end;

end.
