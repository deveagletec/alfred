unit Eagle.Alfred.Command.Config.Project.Show;

interface

uses
  SysUtils, Classes, StrUtils,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Command.Config.Project.Service.ExtractorValuePackage;

type

  [Command('config:project', 'show', 'Show the config project in console')]
  TConfigProjectShow = class(TCommandAbstract)
  private
    FKeyName: string;
    FSaveInFile: Boolean;
    procedure Init;
    procedure SaveValueInFile(const Value: string);
  public
    procedure Execute; override;

    [Param(1, 'key name', False)]
    procedure SetKeyName(const KeyName: string);

    [Option('save-file', 's', 'Save result in file txt')]
    procedure SetSaveInFile;

  end;

implementation

procedure TConfigProjectShow.Execute;
var
  Data: string;
begin

  if FKeyName.IsEmpty then
    Data := TJSON.Stringify<TPackage>(FPackage, True)
  else
    Data := TExtractorValuePackage.Extract(FPackage, FKeyName);

  FConsoleIO.NewEmptyLine();
  FConsoleIO.WriteInfo(Data);
  FConsoleIO.NewEmptyLine();

  if FSaveInFile then
    SaveValueInFile(Data);

end;

procedure TConfigProjectShow.Init;
begin
  FSaveInFile := False;
end;

procedure TConfigProjectShow.SaveValueInFile(const Value: string);
var
  Data: TStringList;
  FileName: string;
begin

  FileName :=  '.\' + IfThen(FKeyName.IsEmpty, 'package-cópia', FKeyName) + '.txt';
  Data := TStringList.Create;

  try

    Data.Add(Value);

    Data.SaveToFile(FileName);
    
  finally
    Data.Free;
  end;

end;

procedure TConfigProjectShow.SetKeyName(const KeyName: string);
begin
  FKeyName := KeyName.Trim;
end;

procedure TConfigProjectShow.SetSaveInFile;
begin
  FSaveInFile := True;
end;

initialization

TAlfred.GetInstance.Register(TConfigProjectShow);

end.
