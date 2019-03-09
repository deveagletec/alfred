unit Eagle.Alfred.Command.Generate.CrudFile;

interface
uses
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.CodeGenerator;

type

  [PackageRequired]
  TGenerateCrudFileCommand = class abstract(TCommandAbstract)
  protected
    FName: string;
    FModuleName: string;
    FCodeGenerator: ICodeGenerator;
    FForce: Boolean;
    FSkipTests: Boolean;
    FTemplateName: string;

    procedure Init; override;
  public

    [ParamAttribute(1, 'Class name')]
    procedure SetName(const Name: string);

    [ParamAttribute(2, 'Module name')]
    procedure SetModuleName(const Name: string);

    [ParamAttribute('template', 'Template name', False)]
    procedure SetTemplateName(const Name: string);

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [OptionAttribute('skip-tests', '-S', 'Skip creating tests files.')]
    procedure SkipTests;

  end;
implementation

{ TGenerateCrudFileCommand }

procedure TGenerateCrudFileCommand.Force;
begin
  FForce := True;
  FCodeGenerator.SetForceGenerate(FForce);
end;

procedure TGenerateCrudFileCommand.Init;
begin
  inherited;
  FCodeGenerator := TCodeGenerator.Create(FAppPath, FPackage);
  FCodeGenerator.SetTemplateName('default');
end;

procedure TGenerateCrudFileCommand.SetModuleName(const Name: string);
begin
  FModuleName := Name;
end;

procedure TGenerateCrudFileCommand.SetName(const Name: string);
begin
  FName := Name;
end;

procedure TGenerateCrudFileCommand.SetTemplateName(const Name: string);
begin
  FTemplateName := Name.Trim;

  if FTemplateName.IsEmpty then
    FTemplateName := 'default';

  FCodeGenerator.SetTemplateName(FTemplateName);
end;

procedure TGenerateCrudFileCommand.SkipTests;
begin
  FSkipTests := True;
end;

end.
