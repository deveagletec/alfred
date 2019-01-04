unit Eagle.Alfred.Command.Generate.Crud;

interface
uses
  System.IOUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type
  [Command('generate', 'crud', 'Generates a Crud')]
  TGenerateCrudCommand = class(TCommandAbstract)
  private

  public
    procedure Execute; override;
  end;

implementation

{ TGenerateCrudCommand }

procedure TGenerateCrudCommand.Execute;
begin
  inherited;

end;

initialization
  TAlfred.GetInstance.Register(TGenerateCrudCommand);
end.
