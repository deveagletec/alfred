unit Eagle.Alfred.Command.Generate.Test;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Command.Generate.CrudFile;

type
  [Command('generate', 'test', 'Generates a Test')]
  TGenerateTestCommand = class(TGenerateCrudFileCommand)
  private
    FLayerName: string;
  public
    procedure Execute; override;

    [ParamAttribute(3, 'Layer Name')]
    procedure SetLayerName(const Value: string);
  end;

implementation

{ TGenerateTestCommand }

procedure TGenerateTestCommand.Execute;
begin
  FCodeGenerator.GenerateTest(FModuleName, FLayerName, FName);

  FConsoleIO.WriteInfo('Created Test');

end;

procedure TGenerateTestCommand.SetLayerName(const Value: string);
begin
  FLayerName := Value;
end;

initialization
  TAlfred.GetInstance.Register(TGenerateTestCommand);
end.
