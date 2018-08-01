unit Eagle.Alfred.Generate.Entity;

interface
uses
  Eagle.Alfred.Generate.Model;

type
  TEntityGenerate = class(TModelGenerate)
  protected
    procedure DoExecute; override;
  end;

implementation

{ TEntityGenerate }

procedure TEntityGenerate.DoExecute;
begin
  Generate('IModel.pas', 'TModel.pas');
end;

end.
