unit Eagle.Alfred.Command.Generate.CrudFile;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Exceptions,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.CodeGenerator;

type
  TGenerateCrudFileCommand = class abstract(TCommandAbstract)
  protected
    FName: string;
    FModuleName: string;
    FCodeGenerator: ICodeGenerator;

    procedure Init; override;

    procedure CheckProjectConfiguration;
  public

    [ParamAttribute(1, 'Class name')]
    procedure SetName(const Name: string);

    [ParamAttribute(2, 'Module name')]
    procedure SetModuleName(const Name: string);
  end;
implementation

{ TGenerateCrudFileCommand }

procedure TGenerateCrudFileCommand.CheckProjectConfiguration;
begin
  if not Assigned(FPackage) then
    raise EAlfredException.Create('Projeto não configurado! Arquivo package.json não encontrado.');

  FPackage.Validate;
end;

procedure TGenerateCrudFileCommand.Init;
begin
  inherited;
  FCodeGenerator := TCodeGenerator.Create(FCurrentPath, FPackage);
end;

procedure TGenerateCrudFileCommand.SetModuleName(const Name: string);
begin
  FModuleName := Name;
end;

procedure TGenerateCrudFileCommand.SetName(const Name: string);
begin
  FName := Name;
end;

end.
