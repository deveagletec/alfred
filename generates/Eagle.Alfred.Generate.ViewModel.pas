unit Eagle.Alfred.Generate.ViewModel;

interface
uses
  Eagle.Alfred.Generate.Model;

type
  TViewModelGenerate = class(TModelGenerate)
  protected
    procedure DoExecute; override;
  end;

implementation

{ TViewModelGenerate }

procedure TViewModelGenerate.DoExecute;
begin

  Generate('IViewModel.pas', 'TViewModel.pas');

end;

end.
