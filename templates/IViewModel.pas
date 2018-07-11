unit Eagle.ERP.{ModuleName}.ViewModel.{ModelName}ViewModel;

interface

type
  I{ModelName}ViewModel = interface
    ['{GUID}']
    function GetCodigo: Integer;
    function GetNome: string;
    procedure SetNome(const Value: string);
    function GetIsInativo: Boolean;
    procedure SetIsInativo(const Value: Boolean);

    property Codigo: Integer read GetCodigo;
    property Nome: string read GetNome write SetNome;
    property IsInativo: Boolean read GetIsInativo write SetIsInativo;
  end;

implementation

end.
