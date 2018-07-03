unit Eagle.ERP.{ModuleName}.ViewModel.Impl.{ModelName}ViewModel;

interface
uses
  System.SysUtils,
  Spring.Container,
  Spring.Container.Common,

  EventBus.Attributes,

  DSharp.Core.Validations,

  Eagle.ERP.Common.Bind.Attributes,

  Eagle.ERP.Common.ViewModel.CrudNavigateViewModel,

  Eagle.ERP.Common.ViewModel.Utils.SearchUtils,

  Eagle.ERP.Common.Exception.WrongValueException,
  Eagle.ERP.Common.Exception.WrongValuesException,

  Eagle.ERP.{ModuleName}.ViewModel.{ModelName}ViewModel,
  Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

type

  T{ModelName}SearchResponse = class(TSearchResponse<T{ModelName}>);

  T{ModelName}ViewModel = class(TCrudNavigateViewModel<I{ModelName}, I{ModelName}Repository>, I{ModelName}ViewModel, IDataErrorInfo)
  private
    procedure NotifyOfEntityChange; override;
  public
    [NotifyChange('CanSalvar, CanEdit, CanDelete, Editable, CanPrior, CanNext')]
    procedure OnSave(Sender: TObject); override;

    procedure OnSearch(Sender: TObject);

    [Subscribe, NotifyChange('CanPrior, CanNext')]
    procedure OnSearchResponse(Event: T{ModelName}SearchResponse); virtual;

    function GetError: string;
    function GetItem(const Name: string): string;

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

{ T{ModelName}ViewModel }

function T{ModelName}ViewModel.GetCodigo: Integer;
begin
  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Id;
end;

function T{ModelName}ViewModel.GetError: string;
begin

end;

function T{ModelName}ViewModel.GetIsInativo: Boolean;
begin

  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Inativo;

end;

function T{ModelName}ViewModel.GetItem(const Name: string): string;
begin

end;

function T{ModelName}ViewModel.GetNome: string;
begin

  if not Assigned(FEntity) then
    Exit;

  Result := FEntity.Nome;

end;

procedure T{ModelName}ViewModel.NotifyOfEntityChange;
begin
  inherited;
  NotifyOfPropertyChange('Codigo');
  NotifyOfPropertyChange('Nome');
  NotifyOfPropertyChange('IsInativo');
end;

procedure T{ModelName}ViewModel.OnSearch(Sender: TObject);
begin
  FDialogService.ShowSearch('C{ModelName}S', T{ModelName}SearchResponse);
end;

procedure T{ModelName}ViewModel.OnSave(Sender: TObject);
begin

  if FEntity.Nome.IsEmpty then
    raise EWrongValueException.Create('Nome', 'Campo requerido!');

  inherited;

end;

procedure T{ModelName}ViewModel.OnSearchResponse(Event: T{ModelName}SearchResponse);
begin

  FEntity := Event.Records.First;

  FCanPrior := not FRepository.IsFirst(FEntity);

  FCanNext := not FRepository.IsLast(FEntity);

  NotifyOfEntityChange;

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
