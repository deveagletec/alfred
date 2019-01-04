unit Eagle.ERP.{ModuleName}.Model.Repository.Impl.{ModelName}Repository;

interface
uses
  System.SysUtils,
  Spring,
  Spring.Container,
  Spring.Container.Common,

  Eagle.ERP.Common.DB.Connection,

  Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

type

  [Interceptor('transactional')]
  T{ModelName}Repository = class(TInterfacedObject, I{ModelName}Repository)
  private
    [Inject]
    FConnection : IConnection;

    function GetRecord(const Sql: string): I{ModelName};
  public
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

procedure T{ModelName}Repository.Delete({ModelName}: I{ModelName});
begin
  FConnection.GetSession.Delete({ModelName} as TInterfacedObject);
end;

function T{ModelName}Repository.Get(const Id: Integer): I{ModelName};
begin
  Result := FConnection.GetSession.FindOne<T{ModelName}>(Id);
end;

function T{ModelName}Repository.GetFirst: I{ModelName};
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S ORDER BY {ModelName}S_ID';
begin
  Result := GetRecord(SQL_ID);
end;

function T{ModelName}Repository.GetLast: I{ModelName};
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S ORDER BY {ModelName}S_ID DESC';
begin
  Result := GetRecord(SQL_ID);
end;

function T{ModelName}Repository.GetNext(Current: I{ModelName}): I{ModelName};
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S WHERE {ModelName}S_ID > ? ORDER BY {ModelName}S_ID';
begin
  Result := GetRecord(SQL_ID.Replace('?', string.Parse(Current.Id).QuotedString));
end;

function T{ModelName}Repository.GetPrior(Current: I{ModelName}): I{ModelName};
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S WHERE {ModelName}S_ID < ? ORDER BY 1 DESC';
begin
  Result := GetRecord(SQL_ID.Replace('?', string.Parse(Current.Id).QuotedString));
end;

function T{ModelName}Repository.GetRecord(const Sql: string): I{ModelName};
var
  Id: string;
begin
  Id := FConnection.GetSession.ExecuteScalar<string>(Sql, []);
  Result := FConnection.GetSession.FindOne<T{ModelName}>(Id);
end;

function T{ModelName}Repository.IsFirst({ModelName}: I{ModelName}): Boolean;
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S WHERE {ModelName}S_ID < ?';
var
  Sql: string;
begin

  if not Assigned({ModelName}) then
    Exit(False);

  Sql := SQL_ID.Replace('?', string.Parse({ModelName}.Id).QuotedString);

  Result := FConnection.GetSession.ExecuteScalar<string>(Sql, []).Equals('')

end;

function T{ModelName}Repository.IsLast({ModelName}: I{ModelName}): Boolean;
const
  SQL_ID = 'SELECT FIRST 1 {ModelName}S_ID FROM {ModelName}S WHERE {ModelName}S_ID > ?';
var
  Sql: string;
begin

  if not Assigned({ModelName}) then
    Exit(False);

  Sql := SQL_ID.Replace('?', string.Parse({ModelName}.Id).QuotedString);

  Result := FConnection.GetSession.ExecuteScalar<string>(Sql, []).Equals('')

end;

function T{ModelName}Repository.Reload(Current: I{ModelName}): I{ModelName};
begin
  Result := FConnection.GetSession.FindOne<T{ModelName}>(Current.Id);
end;

procedure T{ModelName}Repository.Save({ModelName}: I{ModelName});
begin
  FConnection.GetSession.Save({ModelName} as TInterfacedObject);
end;

initialization

GlobalContainer.RegisterType<T{ModelName}Repository>('{ModelName}Repository').Implements<I{ModelName}Repository>;


end.
