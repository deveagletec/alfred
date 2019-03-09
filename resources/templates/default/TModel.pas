unit Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

interface

uses
  Spring.Persistence.Mapping.Attributes,
  Spring.Container,

  Eagle.ERP.Common.Model.Entity.Impl.Entity,
  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName};

type

  [Entity]
  [Table('{ModelName}S', '')]
  [Sequence('{ModelName}S', 1, 1)]
  T{ModelName} = class(TEntity, I{ModelName})
  private
    [Column('ID', [cpPrimaryKey])]
    FId: Integer;

    [Column('NOME')]
    FNome: string;

    [Column('MODIFICADO')]
    FModificado: TDateTime;

    [Column('INATIVO')]
    FInativo: Boolean;

  public
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

{ T{ModelName} }

function T{ModelName}.GetId: Integer;
begin
  Result := FId;
end;

function T{ModelName}.GetInativo: Boolean;
begin
  Result := FInativo;
end;

function T{ModelName}.GetModificado: TDateTime;
begin
  Result := FModificado;
end;

function T{ModelName}.GetNome: string;
begin
  Result := FNome;
end;

procedure T{ModelName}.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure T{ModelName}.SetInativo(const Value: Boolean);
begin
  FInativo := Value;
end;

procedure T{ModelName}.SetModificado(const Value: TDateTime);
begin
  FModificado := Value;
end;

procedure T{ModelName}.SetNome(const Value: string);
begin
  FNome := Value;
end;

initialization

GlobalContainer.RegisterType<T{ModelName}>.Implements<I{ModelName}>;

end.
