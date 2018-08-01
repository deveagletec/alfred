unit Eagle.Alfred.Generate.Repository;

interface
uses
  Eagle.Alfred.Generate.Model;

type
  TRepositoryGenerate = class(TModelGenerate)
  protected
    procedure DoExecute; override;
  end;

implementation

{ TEntityGenerate }

procedure TRepositoryGenerate.DoExecute;
begin
  Generate('IModelRepository.pas', 'TModelRepository.pas');
end;

end.

