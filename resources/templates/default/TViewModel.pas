unit Eagle.ERP.{ModuleName}.ViewModel.Impl.{ModelName}ViewModel;

interface
uses
  System.SysUtils,
  Spring.Container,
  Spring.Container.Common,
  
  Vcl.Dialogs,

  EventBus.Attributes,

  Eagle.Core.Bind.Attributes,

  Eagle.ERP.Common.ViewModel.CrudNavigateViewModel,

  Eagle.ERP.Core.ViewModel.Utils.SearchUtils,

  Eagle.Core.Common.Exception.WrongValuesException,

  Eagle.ERP.{ModuleName}.ViewModel.{ModelName}ViewModel,
  Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

type

  T{ModelName}SearchResponse = class(TSearchResponse<T{ModelName}>);

  T{ModelName}ViewModel = class(TCrudNavigateViewModel<I{ModelName}, I{ModelName}Repository>, I{ModelName}ViewModel)
  public

    procedure OnValidate(); override;

    procedure OnSearch(Sender: TObject);

    [Subscribe, NotifyChange('*')]
    procedure OnSearchResponse(Event: T{ModelName}SearchResponse); virtual;

    function GetCodigo: Integer;
    function GetNome: string;
    procedure SetNome(const Value: string);
    function GetIsInativo: Boolean;
    procedure SetIsInativo(const Value: Boolean);

    property Codigo: Integer read GetCodigo;
    property Nome: string read GetNome write SetNome;
    property IsInativo: Boolean read GetIsInativo write SetIsInativo;

    [Secured('permissao:{perfil}:CanNew')]
    property CanNew;
    [Secured('permissao:{perfil}:CanEdit')]
    property CanEdit;
    [Secured('permissao:{perfil}:CanDelete')]
    property CanDelete;

 end;

implementation

{ T{ModelName}ViewModel }

function T{ModelName}ViewModel.GetCodigo: Integer;
begin
  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Id;
end;

function T{ModelName}ViewModel.GetIsInativo: Boolean;
begin
  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Inativo;
end;

function T{ModelName}ViewModel.GetNome: string;
begin
  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Nome;
end;

procedure T{ModelName}ViewModel.OnSearch(Sender: TObject);
begin
  { TODO 3 : Alterar o nome da pesquisa das entidades {ModelName}s }
  FDialogWindow.ShowSearch('C{ModelName}S', T{ModelName}SearchResponse);
end;

procedure T{ModelName}ViewModel.OnValidate();
const
  TAMANHO_MAX_NOME = 50;
  MSG_VIOLACAO_TAMANHO_NOME = 'O Nome do "{ModelName}" não pode possuir mais que %d caracteres!';
var
  WrongValuesException: EWrongValuesException;
begin
  WrongValuesException := EWrongValuesException.Create([]);

  if FEntity.Nome.IsEmpty then
    WrongValuesException.Add('Nome', 'Campo requerido!');

  {if FRepository.IsDuplicate(FEntity) then
    WrongValuesException.Add('Nome', MSG_VIOLACAO_DUPLICIDADE);}

  if FEntity.Nome.Length > TAMANHO_MAX_NOME then
    WrongValuesException.AddFmt('Nome', MSG_VIOLACAO_TAMANHO_NOME, [TAMANHO_MAX_NOME]);

  if not WrongValuesException.IsEmpty then
    raise WrongValuesException;
end;

procedure T{ModelName}ViewModel.OnSearchResponse(Event: T{ModelName}SearchResponse);
begin
  DoOnSearchResponse(Event.Records.First);
  Event.Free;
end;

procedure T{ModelName}ViewModel.SetIsInativo(const Value: Boolean);
begin
  FEntity.Inativo := Value;
end;

procedure T{ModelName}ViewModel.SetNome(const Value: string);
begin
  FEntity.Nome := Value.Trim;
end;

initialization

GlobalContainer.RegisterType<T{ModelName}ViewModel>('{ModelName}ViewModel').Implements<I{ModelName}ViewModel>;

end.
