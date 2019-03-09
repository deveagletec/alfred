unit Eagle.ERP.{ModuleName}.Model.Entity.{ModelName}Test;

interface
uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName};

type

  [TestFixture]
  T{ModelName}Test = class(TObject)
  public
    [Test]
    procedure TestExample;   
  end;

implementation

procedure T{ModelName}Test.TestExample;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(T{ModelName}Test);
end.
