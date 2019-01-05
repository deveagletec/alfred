unit Eagle.Alfred.Command.Delete.DeleteCrudFile;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type
  TDeleteCrudFileCommand = class abstract(TCommandAbstract)
  protected
    FName: string;
    FModuleName: string;
    FForce: Boolean;
  public
    [ParamAttribute(1, 'Class name')]
    procedure SetName(const Value: string);

    [ParamAttribute(2, 'Module name')]
    procedure SetModuleName(const Value: string);

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;
  end;

implementation

{ TDeleteCrudFileCommand }

procedure TDeleteCrudFileCommand.Force;
begin
  FForce := True;
end;

procedure TDeleteCrudFileCommand.SetModuleName(const Value: string);
begin
  FModuleName := Value;
end;

procedure TDeleteCrudFileCommand.SetName(const Value: string);
begin
  FName := Value;
end;

end.
