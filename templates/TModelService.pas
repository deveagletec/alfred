unit Eagle.ERP.{ModuleName}.Model.Service.Impl.{ModelName}Service;

interface

uses
  Spring.Container,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},

  Eagle.ERP.{ModuleName}.Model.Service.{ModelName}Service;

type

  T{ModelName}Service = class(TInterfacedObject, I{ModelName}Service)
  private

  end;

implementation

initialization
  GlobalContainer.RegisterType<T{ModelName}Service>.Implements<I{ModelName}Service>;

end.
