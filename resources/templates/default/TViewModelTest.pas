unit Eagle.ERP.{ModuleName}.ViewModel.{ModelName}ViewModelTest;

interface
uses
  System.Rtti,

  DUnitX.TestFramework,
  Delphi.Mocks,

  Spring.Collections,

  EventBus,

  Eagle.ERP.Common.Test.DataBaseTestHelper,

  Eagle.ERP.Common.Exception.WrongValuesException,
  Eagle.ERP.Common.Exception.Helper.WrongValuesExceptionHelper,

  Eagle.ERP.Common.ViewModel.DialogService,

  Eagle.ERP.{ModuleName}.Model.Entity.{ModelName},
  Eagle.ERP.{ModuleName}.Model.Entity.Impl.{ModelName},

  Eagle.ERP.{ModuleName}.ViewModel.Impl.{ModelName}ViewModel,

  Eagle.ERP.{ModuleName}.Model.Repository.{ModelName}Repository,
  Eagle.ERP.{ModuleName}.Model.Repository.Impl.{ModelName}Repository;

type

  [TestFixture]
  [Category('view-model')]
  T{ModelName}ViewModelTest = class(TObject)
  private
    F{ModelName}ViewModel: T{ModelName}ViewModel;
    FDataBase: TDataBaseTestHelper;
    FRepository: I{ModelName}Repository;

    procedure DoTestValidation(const Msg: string);
    procedure ResetDataBase;
  public
    [SetupFixture]
    procedure SetupFixture;

    [Setup]
    procedure Setup;

    [TearDownFixture]
    procedure TearDown;

    [Test]
    [TestCase('Nome vazio', ',Campo requerido!')]
    [TestCase('Nome maior que o permitido', 'dajfçasjdfjasçdfjaçskdfjçaksjdfçaksjdfkajsdfkajsdçfkjasdçfkjasçdkfjasd,O Nome do "{ModelName}" não pode possuir mais que')]
    procedure Test_deveria_validar_nomes_com_sucesso(const Value, Msg: string);

    [Test]
    [Category('integration,db')]
    procedure Test_deveria_realizar_o_cadastro_sem_erros;

    [Test]
    [Category('integration,db')]
    procedure Test_deveria_excluir_o_cadastro_sem_erros;

    [Test]
    [Category('integration,db')]
    procedure Test_deveria_navegar_pelos_registro_sem_erros;

    [Test]
    procedure Test_deveria_carregar_as_propriedades_sem_erros;

    [Test]
    procedure Test_deveria_tratar_a_selecao_de_registro_sem_erros;

    [Test]
    procedure Test_deveria_configurar_os_controles_das_acoes_sem_erros;

  end;

implementation

procedure T{ModelName}ViewModelTest.DoTestValidation(const Msg: string);
begin
  try
    F{ModelName}ViewModel.OnValidate;

    Assert.Fail();
  except
    on E: EWrongValuesException do
      Assert.IsTrue(E.ContainsMsgLike(Msg));
  end;
end;

procedure T{ModelName}ViewModelTest.ResetDataBase;
begin
  FDataBase.Clean('{ModelName}');
  FDataBase.Load('..\..\..\tests\resources\Load_{ModelName}.sql');
end;

procedure T{ModelName}ViewModelTest.Setup;
begin
  F{ModelName}ViewModel.SetRepository(FRepository);
end;

procedure T{ModelName}ViewModelTest.SetupFixture;
begin
  FDataBase := TDataBaseTestHelper.GetInstance;
  F{ModelName}ViewModel := T{ModelName}ViewModel.Create;

  FRepository := T{ModelName}Repository.Create(FDataBase.GetConnection);

  TEventBus.GetDefault.RegisterSubscriber(F{ModelName}ViewModel);
end;

procedure T{ModelName}ViewModelTest.TearDown;
begin
  TEventBus.GetDefault.Unregister(F{ModelName}ViewModel);
  F{ModelName}ViewModel.Free;
end;

procedure T{ModelName}ViewModelTest.Test_deveria_realizar_o_cadastro_sem_erros;
begin
  F{ModelName}ViewModel.OnNew(nil);

  F{ModelName}ViewModel.Nome := 'XPTO 1';
  F{ModelName}ViewModel.IsInativo := False;

  F{ModelName}ViewModel.OnSave(nil);

  FDataBase.AssertHas('{ModelName}', 'NOME', 'XPTO 1');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_carregar_as_propriedades_sem_erros;
var
  RepositoryMock: TMock<I{ModelName}Repository>;
  Entity: I{ModelName};
begin

  Entity := T{ModelName}.Create;
  Entity.Id := 1;
  Entity.Nome := 'XPTO';
  Entity.Inativo := True;

  RepositoryMock := TMock<I{ModelName}Repository>.Create;

  RepositoryMock.Setup.WillReturn(TValue.From<I{ModelName}>(Entity)).When.GetFirst;

  F{ModelName}ViewModel.SetRepository(RepositoryMock.Instance);

  F{ModelName}ViewModel.OnFirst(nil);

  Assert.AreEqual(1, F{ModelName}ViewModel.Codigo, 'Código');
  Assert.AreEqual('XPTO', F{ModelName}ViewModel.Nome, 'Nome');
  Assert.AreEqual(True, F{ModelName}ViewModel.IsInativo, 'IsInativo');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_configurar_os_controles_das_acoes_sem_erros;
var
  DialogServiceMock: TMock<IDialogService>;
  RepositoryMock: TMock<I{ModelName}Repository>;
  Entity: I{ModelName};
begin

  Entity := T{ModelName}.Create;

  DialogServiceMock := TMock<IDialogService>.Create();

  DialogServiceMock.Setup.WillReturn(True).When.ShowQuestion('Deseja cancelar a operação?');

  F{ModelName}ViewModel.SetDialogService(DialogServiceMock.Instance);

  RepositoryMock := TMock<I{ModelName}Repository>.Create;

  RepositoryMock.Setup.WillReturn(TValue.From<I{ModelName}>(Entity)).When.GetLast;

  F{ModelName}ViewModel.SetRepository(RepositoryMock.Instance);

  F{ModelName}ViewModel.OnNew(nil);

  Assert.IsFalse(F{ModelName}ViewModel.CanNew, 'OnNew - CanNew');
  Assert.IsFalse(F{ModelName}ViewModel.CanEdit, 'OnNew - CanEdit');
  Assert.IsFalse(F{ModelName}ViewModel.CanDelete, 'OnNew - CanDelete');
  Assert.IsFalse(F{ModelName}ViewModel.CanSearch, 'OnNew - CanSearch');
  Assert.IsTrue(F{ModelName}ViewModel.CanSave, 'OnNew - CanSave');

  F{ModelName}ViewModel.OnCancel(nil);

  Assert.IsTrue(F{ModelName}ViewModel.CanNew, 'OnCancel - CanNew');
  Assert.IsTrue(F{ModelName}ViewModel.CanEdit, 'OnCancel - CanEdit');
  Assert.IsTrue(F{ModelName}ViewModel.CanDelete, 'OnCancel - CanDelete');
  Assert.IsTrue(F{ModelName}ViewModel.CanSearch, 'OnCancel - CanSearch');
  Assert.IsFalse(F{ModelName}ViewModel.CanSave, 'OnCancel - CanSave');

  F{ModelName}ViewModel.OnEdit(nil);

  Assert.IsFalse(F{ModelName}ViewModel.CanNew, 'OnEdit - CanNew');
  Assert.IsFalse(F{ModelName}ViewModel.CanEdit, 'OnEdit - CanEdit');
  Assert.IsFalse(F{ModelName}ViewModel.CanDelete, 'OnEdit - CanDelete');
  Assert.IsFalse(F{ModelName}ViewModel.CanSearch, 'OnEdit - CanSearch');
  Assert.IsTrue(F{ModelName}ViewModel.CanSave, 'OnEdit - CanSave');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_excluir_o_cadastro_sem_erros;
var
  Entity: I{ModelName};
  DialogServiceMock: TMock<IDialogService>;
begin
  ResetDataBase;

  DialogServiceMock := TMock<IDialogService>.Create();

  DialogServiceMock.Setup.WillReturn(True).When.ShowQuestion('ATENÇÃO!||Confirma a exclusão do registro?');

  Entity := T{ModelName}.Create;
  Entity.Id := 1;

  F{ModelName}ViewModel.SetDialogService(DialogServiceMock.Instance);

  F{ModelName}ViewModel.SetEntity(Entity);

  F{ModelName}ViewModel.OnDelete(nil);

  FDataBase.AssertMissing('{ModelName}', 'ID', '1');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_navegar_pelos_registro_sem_erros;
begin
  ResetDataBase;

  F{ModelName}ViewModel.OnFirst(nil);

  Assert.AreEqual(1, F{ModelName}ViewModel.Codigo, 'OnFirst');
  Assert.IsFalse(F{ModelName}ViewModel.CanPrior, 'CanPrior');
  Assert.IsTrue(F{ModelName}ViewModel.CanNext, 'CanNext');

  F{ModelName}ViewModel.OnNext(nil);

  Assert.AreEqual(2, F{ModelName}ViewModel.Codigo, 'OnNext');

  F{ModelName}ViewModel.OnLast(nil);

  Assert.AreEqual(4, F{ModelName}ViewModel.Codigo, 'OnLast');

  F{ModelName}ViewModel.OnPrior(nil);

  Assert.AreEqual(3, F{ModelName}ViewModel.Codigo, 'OnPrior');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_tratar_a_selecao_de_registro_sem_erros;
var
  DialogServiceMock: TMock<IDialogService>;
begin

  DialogServiceMock := TMock<IDialogService>.Create();

  DialogServiceMock.Setup.WillExecute(function(const args : TArray<TValue>; const ReturnType : TRttiType):TValue
    var
      Event: T{ModelName}SearchResponse;
      Entity: T{ModelName};
    begin
      Entity := T{ModelName}.Create;
      Entity.Id := 1;
      Entity.Nome := 'XPTO';

      Event := T{ModelName}SearchResponse.Create;
      Event.Records := TCollections.CreateList<T{ModelName}>;
      Event.Records.Add(Entity);

      TEventBus.GetDefault.Post(Event);

    end).When.ShowNewSearch('C{ModelName}', T{ModelName}SearchResponse);

  F{ModelName}ViewModel.SetDialogService(DialogServiceMock.Instance);

  F{ModelName}ViewModel.OnSearch(nil);

  Assert.AreEqual(1, F{ModelName}ViewModel.Codigo, 'Codigo');
  Assert.AreEqual('XPTO', F{ModelName}ViewModel.Nome, 'Nome');
end;

procedure T{ModelName}ViewModelTest.Test_deveria_validar_nomes_com_sucesso(const Value, Msg: string);
begin
  F{ModelName}ViewModel.OnNew(nil);

  F{ModelName}ViewModel.SetNome(Value);

  DoTestValidation(Msg);
end;

initialization
  TDUnitX.RegisterTestFixture(T{ModelName}ViewModelTest);
end.