unit Eagle.Alfred.Generate.Test;

interface
uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Eagle.Alfred.Data,
  Eagle.Alfred.Generate.UnitGenerate,
  Eagle.Alfred.DprojParser,
  Eagle.Alfred.Utils;

type

  TTestGenerate = class(TUnitGenerate)
  protected
    FDprojParser : IDprojParser;

    procedure DoExecute; override;

  public
    constructor Create(const AppPath: string; APackage: TPackage);
  end;

implementation

{ TTestGenerate }

constructor TTestGenerate.Create(const AppPath: string; APackage: TPackage);
begin
  inherited Create(AppPath, APackage);

  FSourceDir := FPackage.TestsDir;

  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Id + 'Test');
end;

procedure TTestGenerate.DoExecute;
var
  BaseDir, FileName, FilePath: string;
begin

  BaseDir := FPackage.BaseDir + FFilePath;

  CreateDiretories([BaseDir]);

  FileName := FNamespace + FClassName + 'Test.pas';

  GenerateFile('T' + FLayerName + 'Test.pas', FileName, BaseDir + FileName);

  FDprojParser.AddUnit(FileName, '..\..\' + FFilePath + FileName);

end;

end.
