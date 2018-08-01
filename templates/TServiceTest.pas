unit Eagle.ERP.{ModuleName}.Model.Service.{ModelName}ServiceTest;

interface
uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  
  Eagle.ERP.{ModuleName}.Model.Service.{ModelName}Service,
  Eagle.ERP.{ModuleName}.Model.Service.Impl.{ModelName}Service;

type

  [TestFixture]
  T{ModelName}ServiceTest = class(TObject)
  private
	FService: I{ModelName}Service;
  public
	[SetupFixture]
    procedure SetupFixture;

    [Test]
    procedure TestExample;   
  end;

implementation

procedure T{ModelName}ServiceTest.SetupFixture;
begin

  FService := T{ModelName}Service.Create;

end;

procedure T{ModelName}ServiceTest.TestExample;
begin
  //Assert.
end;

initialization
  TDUnitX.RegisterTestFixture(T{ModelName}ServiceTest);
end.
