unit Eagle.ERP.{ModuleName}.Model.Repository.Impl.{ModelName}Repository;

interface
uses
  System.SysUtils,
  Spring,
  Spring.Container,
  Spring.Container.Common,

  Eagle.Core.DB.Connection,
  Eagle.Core.DB.SessionHelper,

  Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

type

  [Interceptor('transactional')]
  T{ModelName}Repository = class(TInterfacedObject, I{ModelName}Repository)
  private

    [Inject('FiredacConnection')]
    FConnection : IConnection;

  public
    constructor Create; overload;
    constructor Create(Con: IConnection); overload;
    function IsFirst({ModelName}: I{ModelName}): Boolean;
    function IsLast({ModelName}: I{ModelName}): Boolean;
    function GetFirst(): I{ModelName};
    function GetPrior(Current: I{ModelName}): I{ModelName};
    function GetNext(Current: I{ModelName}): I{ModelName};
    function GetLast(): I{ModelName};
    function Reload(Current: I{ModelName}): I{ModelName};
    function Get(const Id: Integer): I{ModelName};
    procedure Save({ModelName}: I{ModelName});
    procedure Delete({ModelName}: I{ModelName});
  end;

implementation

{ T{ModelName}Repository }

constructor T{ModelName}Repository.Create;
begin

end;

constructor T{ModelName}Repository.Create(Con: IConnection);
begin
  inherited Create;
  FConnection := Con;
end;

procedure T{ModelName}Repository.Delete({ModelName}: I{ModelName});
begin
  FConnection.GetSession.Delete({ModelName} as TInterfacedObject);
end;

function T{ModelName}Repository.Get(const Id: Integer): I{ModelName};
begin
  Result := FConnection.GetSession.FindOne<T{ModelName}>(Id);
end;

function T{ModelName}Repository.GetFirst: I{ModelName};
begin
  Result := FConnection.GetSession.GetFirst<T{ModelName}>;
end;

function T{ModelName}Repository.GetLast: I{ModelName};
begin
  Result := FConnection.GetSession.GetLast<T{ModelName}>;
end;

function T{ModelName}Repository.GetNext(Current: I{ModelName}): I{ModelName};
begin
   Result := FConnection.GetSession.GetNext<T{ModelName}>(Current as TInterfacedObject);
end;

function T{ModelName}Repository.GetPrior(Current: I{ModelName}): I{ModelName};
begin
  Result := FConnection.GetSession.GetPrior<T{ModelName}>(Current as TInterfacedObject);
end;

function T{ModelName}Repository.IsFirst({ModelName}: I{ModelName}): Boolean;
begin
  if not Assigned({ModelName}) then
    Exit(False);

  Result := FConnection.GetSession.IsFirst<T{ModelName}>({ModelName} as TInterfacedObject);
end;

function T{ModelName}Repository.IsLast({ModelName}: I{ModelName}): Boolean;
begin
  if not Assigned({ModelName}) then
    Exit(False);

  Result := FConnection.GetSession.IsLast<T{ModelName}>({ModelName} as TInterfacedObject);
end;

function T{ModelName}Repository.Reload(Current: I{ModelName}): I{ModelName};
begin
  Result := FConnection.GetSession.FindOne<T{ModelName}>(Current.Id);
end;

procedure T{ModelName}Repository.Save({ModelName}: I{ModelName});
begin
  FConnection.GetSession.InsertOrUpdate<T{ModelName}>({ModelName} as TInterfacedObject);
end;

initialization
  GlobalContainer.RegisterType<T{ModelName}Repository>('{ModelName}Repository').Implements<I{ModelName}Repository>;
end.
