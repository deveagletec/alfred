unit Eagle.Alfred.Command.DB.MigrateInit;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,

  Eagle.Alfred.Core.Command,

  Eagle.Alfred.Migrate.Service.MigrateService,
  Eagle.Alfred.Migrate.Repository.MigrateRepository;

type

  [Command('db:migrate', 'init', 'Generates Migrate Estructure')]
  TMigrateInit = class(TCommandAbstract)
  private

    FMigrateService: IMigrateService;
    FMigrateRepository: IMigrateRepository;

    procedure showMessageSucess(const msg: String);
    procedure showMessageAlert(const msg: String);

  public
    procedure Execute; override;
    procedure Init; override;

  end;

implementation

procedure TMigrateInit.Execute;
begin
  inherited;

  if FMigrateRepository.TableMigrateExists() then
    showMessageAlert('Migrate structure in database already exists!')
  else
  begin
    FMigrateRepository.CreateTableMigrate();
    showMessageSucess('Migrate structure in database Created!');
  end;

  if FMigrateService.MigrateDirectoryExists() then
    showMessageAlert('Migrate directory already exists!')
  else
  begin
    FMigrateService.CreateMigrateDirectory();
    showMessageSucess('Migrate directory Created!');
  end;

end;

procedure TMigrateInit.Init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);
  FMigrateRepository := TMigrateRepository.Create(FPackage);

end;

procedure TMigrateInit.showMessageSucess(const msg: String);
begin
  FConsoleIO.WriteSucess(sLineBreak + '* ------- ');
  FConsoleIO.WriteSucess(Format('| %s', [msg]));
  FConsoleIO.WriteSucess('* ----------------------------------------------------- ' + sLineBreak);
end;

procedure TMigrateInit.showMessageAlert(const msg: String);
begin
  FConsoleIO.WriteAlert(sLineBreak + '* ------- ');
  FConsoleIO.WriteAlert(Format('| %s', [msg]));
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ' + sLineBreak);
end;

initialization
  TAlfred.GetInstance.Register(TMigrateInit);

end.
