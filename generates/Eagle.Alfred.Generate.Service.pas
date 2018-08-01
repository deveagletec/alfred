unit Eagle.Alfred.Generate.Service;

interface
uses
  Eagle.Alfred.Generate.Model;

type
  TServiceGenerate = class(TModelGenerate)
  protected
    procedure DoExecute; override;
  end;

implementation

{ TEntityGenerate }

procedure TServiceGenerate.DoExecute;
begin
  Generate('IModelService.pas', 'TModelService.pas');
end;

end.

