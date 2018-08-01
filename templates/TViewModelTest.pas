unit Eagle.ERP.{ModuleName}.ViewModel.{ModelName}ViewModelTest;

interface
uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  
  Eagle.ERP.{ModuleName}.ViewModel.Impl.{ModelName}ViewModel;

type

  [TestFixture]
  T{ModelName}ViewModelTest = class(TObject)
  public
    [Test]
    procedure TestExample;   
  end;

implementation

procedure T{ModelName}ViewModelTest.TestExample;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(T{ModelName}ViewModelTest);
end.
