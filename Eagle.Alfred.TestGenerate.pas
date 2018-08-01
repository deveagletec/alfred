unit Eagle.Alfred.TestGenerate;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Data,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Utils;

type

  ITestGenerate = interface
    ['{3E5D1E77-C61F-486A-BA60-3BFECBF8F492}']
    procedure Execute(const ModuleName, LayerName, ModelName: string);
  end;

  TTestGenerate = class(TInterfacedObject, ITestGenerate)
  private
    FAppPath: string;
    FPackage: TPackage;
    FModuleName: string;
    FLayerName: string;
    FModelName: string;
    FClassName: string;
    FDprojParser: IDprojParser;

    FNamespace: string;
    FFilePath: string;

    procedure MountNamespaceAndFilePath;
    procedure MountClassName;
    procedure DoExecute;
    procedure GenerateFile(const Template, UnitName, FileName: string);
    function GuidCreate: string;

  public
    constructor Create(const AppPath: string; APackage: TPackage);
    procedure Execute(const ModuleName, LayerName, ModelName: string);
  end;

implementation

{ TTestGenerate }

constructor TTestGenerate.Create(const AppPath: string; APackage: TPackage);
begin
  FAppPath := AppPath;
  FPackage := APackage;
  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id + 'Test');
end;

procedure TTestGenerate.DoExecute;
var
  BaseDir, FileName, FilePath: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir]);

  FileName := FNamespace + FClassName + 'Test.pas';

  GenerateFile('T' + FLayerName + 'Test.pas', FileName, BaseDir + FNamespace + FClassName + 'Test.pas');

  FDprojParser.AddUnit(FileName, '..\..\' + FFilePath + FileName);

end;

procedure TTestGenerate.Execute(const ModuleName, LayerName, ModelName: string);
begin

  FModuleName := Capitalize(ModuleName.Replace('.', ''));
  FLayerName := Capitalize(LayerName.Replace('.', ''));
  FModelName := Capitalize(ModelName.Replace('.', ''));

  MountNamespaceAndFilePath;

  MountClassName;

  DoExecute;

end;

procedure TTestGenerate.GenerateFile(const Template, UnitName, FileName: string);
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

function TTestGenerate.GuidCreate: string;
var
  ID: TGUID;
begin

  ID := TGUID.NewGuid;

  Result := GUIDToString(ID);

end;

procedure TTestGenerate.MountClassName;
begin

  FClassName := FModelName;

  if AnsiMatchText(FLayerName, ['viewmodel', 'service', 'repository']) then
    FClassName := FClassName + FLayerName;

end;

procedure TTestGenerate.MountNamespaceAndFilePath;
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

  FFilePath := FPackage.TestsDir + ModuleDir + LayerDir;

end;

end.
