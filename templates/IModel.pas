unit Eagle.ERP.{ModuleName}.Model.Entity.{ModelName};

interface

uses
  Eagle.ERP.Common.Model.Entity.Entity;

type
  I{ModelName} = interface(IEntity)
    ['{GUID}']

    function GetId: Integer;
    procedure SetId(const Value: Integer);
    function GetNome: string;
    procedure SetNome(const Value: string);
    function GetModificado: TDateTime;
    procedure SetModificado(const Value: TDateTime);
    function GetInativo: Boolean;
    procedure SetInativo(const Value: Boolean);

    property Id: Integer read GetId write SetId;
    property Nome: string read GetNome write SetNome;
    property Modificado: TDateTime read GetModificado write SetModificado;
    property Inativo: Boolean read GetInativo write SetInativo;
  end;

implementation

end.
