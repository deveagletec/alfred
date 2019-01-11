unit Eagle.Alfred.Command.DB.UpdateCheck;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,

  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,

  Eagle.Alfred.Update.Service.UpdateService;

type

  [Command('db:update', 'check', 'Checa conflitos de SQL nos migrates de determinada versão')]
  TUpdateCheck = class(TCommandAbstract)
  private

    FVersion: String;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    procedure showMessageError(const error: String);
    procedure showMessageFounded(conflictsMigrates: TDictionary < String, TList < String >> );
    procedure showMessageMigratesNotFounded;
    procedure showMessageNotFounded;

  public
    destructor Destroy; override;

    procedure execute; override;
    procedure init; override;

    [ParamAttribute(1, 'Filtro de versão')]
    procedure setVersion(const version: String);

  end;

implementation

destructor TUpdateCheck.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TUpdateCheck.execute;
var
  conflictsMigrates: TDictionary<String, TList<String>>;
begin

  try
    FMigrates := FMigrateService.getMigratesByVersion(FVersion.Trim());
  except

    on e: EAlfredException do
    begin
      showMessageError(e.Message);
      exit;
    end
    else
      raise;

  end;

  if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
  begin
    showMessageMigratesNotFounded();
    exit;
  end;

  conflictsMigrates := FUpdateService.getConflictMigrates(FMigrates);

  try

    if (not Assigned(conflictsMigrates)) or (conflictsMigrates.Count = 0) then
      showMessageNotFounded()
    else
      showMessageFounded(conflictsMigrates);

  finally

    if Assigned(conflictsMigrates) then
      conflictsMigrates.Free();

  end;

end;

procedure TUpdateCheck.init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);
  FUpdateService := TUpdateService.Create();

end;

procedure TUpdateCheck.setVersion(const version: String);
begin
  FVersion := version;
end;

procedure TUpdateCheck.showMessageError(const error: String);
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('%s :(', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateCheck.showMessageFounded(conflictsMigrates: TDictionary < String, TList < String >> );
var
  key, listMigrates: String;
  conflictMigrateList: TList<String>;
begin

  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError('| Conflicts founded :( ');
  FConsoleIO.WriteError('* ----------------------------------------------------- ');

  try

    for key in conflictsMigrates.Keys do
    begin

      conflictsMigrates.TryGetValue(key, conflictMigrateList);

      if not conflictsMigrates.Count > 1 then
        continue;

      listMigrates := String.Join(', ', conflictMigrateList.ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [key, listMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    if Assigned(conflictMigrateList) then
      conflictMigrateList.Free();

  end;

end;

procedure TUpdateCheck.showMessageMigratesNotFounded;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateCheck.showMessageNotFounded;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| Conflicts not found :) ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

initialization

TAlfred.GetInstance.Register(TUpdateCheck);

end.
