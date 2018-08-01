unit Eagle.Alfred.Generate.Model;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Data,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Utils,
  Eagle.Alfred.Generate.UnitGenerate;

type
  TModelGenerate = class abstract (TUnitGenerate)
  protected
    FDprojParser : IDprojParser;

    procedure Generate(const InterfaceTemplate, ClassTemplate: string);
  public
    constructor Create(const AppPath: string; APackage: TPackage);
  end;

implementation

{ TViewModelGenerate }

constructor TModelGenerate.Create(const AppPath: string; APackage: TPackage);
begin
  inherited Create(AppPath, APackage);
  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id);
end;

procedure TModelGenerate.Generate(const InterfaceTemplate, ClassTemplate: string);
var
  BaseDir, InterfaceName, ClassName, FilePath: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir, BaseDir + 'impl\']);

  InterfaceName := FNamespace + FClassName + '.pas';
  ClassName := FNamespace + 'Impl.' + FClassName + '.pas';

  GenerateFile(InterfaceTemplate, InterfaceName, BaseDir + InterfaceName);
  GenerateFile(ClassTemplate, ClassName, BaseDir + 'impl\' + ClassName);

  FDprojParser.AddUnit(InterfaceName, '..\..\' + FFilePath + InterfaceName);
  FDprojParser.AddUnit(ClassName, '..\..\' + FFilePath + 'impl\' + ClassName);

end;

end.

