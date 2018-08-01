unit Eagle.Alfred.Generate.UnitGenerate;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Data,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Utils;

type
  IUnitGenerate = interface
    ['{3E5D1E77-C61F-486A-BA60-3BFECBF8F492}']
    procedure Execute(const ModuleName, LayerName, ModelName: string);
  end;

  TUnitGenerate = class abstract (TInterfacedObject, IUnitGenerate)
  protected
    FAppPath: string;
    FPackage: TPackage;
    FModuleName: string;
    FLayerName: string;
    FModelName: string;
    FClassName: string;
    FNamespace: string;
    FFilePath: string;
    FSourceDir: string;

    procedure MountNamespaceAndFilePath;
    procedure MountClassName;
    procedure GenerateFile(const Template, UnitName, FileName: string);
    procedure DoExecute; virtual; abstract;

  public
    constructor Create(const AppPath: string; APackage: TPackage);
    procedure Execute(const ModuleName, LayerName, ModelName: string);
  end;

implementation

{ TUnitGenerate }

constructor TUnitGenerate.Create(const AppPath: string; APackage: TPackage);
begin
  FAppPath := AppPath;
  FPackage := APackage;
  FSourceDir := FPackage.SourceDir;
end;

procedure TUnitGenerate.Execute(const ModuleName, LayerName, ModelName: string);
begin

  FModuleName := Capitalize(ModuleName.Replace('.', ''));
  FLayerName := Capitalize(LayerName.Replace('.', ''));
  FModelName := Capitalize(ModelName.Replace('.', ''));

  MountNamespaceAndFilePath;

  MountClassName;

  DoExecute;

end;

procedure TUnitGenerate.GenerateFile(const Template, UnitName, FileName: string);
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

procedure TUnitGenerate.MountClassName;
begin

  FClassName := FModelName;

  if AnsiMatchText(FLayerName, ['viewmodel', 'service', 'repository']) then
    FClassName := FClassName + FLayerName;

end;

procedure TUnitGenerate.MountNamespaceAndFilePath;
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

end.
