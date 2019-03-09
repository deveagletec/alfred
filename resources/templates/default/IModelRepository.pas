unit Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository;

interface
uses
  Eagle.ERP.Common.Model.Repository.CrudNavigateRepository,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName};

type
  I{ModelName}Repository = interface(ICrudNavigateRepository<I{ModelName}>)
    ['{GUID}']
  end;

implementation

end.
