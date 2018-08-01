unit Eagle.Alfred.Generate.View;

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
  TViewGenerate = class(TUnitGenerate)
  protected
    FDprojParser : IDprojParser;

    procedure DoExecute; override;

  public
    constructor Create(const AppPath: string; APackage: TPackage);
  end;

implementation

{ TViewGenerate }

constructor TViewGenerate.Create(const AppPath: string; APackage: TPackage);
begin
  inherited Create(AppPath, APackage);
  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id);
end;

procedure TViewGenerate.DoExecute;
var
  BaseDir, UnitName, ViewName: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir]);

  UnitName := FNamespace + FClassName + 'View.pas';

  ViewName := FNamespace + FClassName + 'View.dfm';

  GenerateFile('View.dfm', ViewName, BaseDir + ViewName);
  GenerateFile('View.pas', UnitName, BaseDir + UnitName);

  FDprojParser.AddUnit(UnitName, '..\..\' + FFilePath + UnitName);

  FDprojParser.AddForm(UnitName, FModelName + 'View', '..\..\' + FFilePath + UnitName);

end;

end.
