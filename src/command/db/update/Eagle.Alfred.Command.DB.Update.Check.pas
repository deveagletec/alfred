unit Eagle.Alfred.Command.DB.Update.Check;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,

  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Exceptions,

  Eagle.Alfred.Migrate.Model.Migrate,
  Eagle.Alfred.Migrate.Service.MigrateService,

  Eagle.Alfred.Commond.DB.Update.UpdateService;

type

  [Command('db:update', 'check', 'Check conflicts in migrates')]
  TUpdateCheck = class(TCommandAbstract)
  private

    FVersion: String;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    procedure ShowMessageError(const error: String);
    procedure ShowMessageFounded(ConflictsMigrates: TDictionary<string,TList<string
        >>);
    procedure ShowMessageMigratesNotFounded;
    procedure ShowMessageNotFounded;

  public
    destructor Destroy; override;

    procedure Execute; override;
    procedure Init; override;

    [ParamAttribute(1, 'Migrates version')]
    procedure SetVersion(const version: String);

  end;

implementation

destructor TUpdateCheck.Destroy;
begin

  if Assigned(FMigrates) then
    FMigrates.Free();

  inherited;
end;

procedure TUpdateCheck.Execute;
var
  conflictsMigrates: TDictionary<String, TList<String>>;
begin

  try
    FMigrates := FMigrateService.GetMigratesByVersion(FVersion.Trim());
  except

    on E: EAlfredException do
    begin
      ShowMessageError(E.Message);
      Exit;
    end
    else
      raise;

  end;

  if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
  begin
    ShowMessageMigratesNotFounded();
    Exit;
  end;

  conflictsMigrates := FUpdateService.GetConflictMigrates(FMigrates);

  try

    if (not Assigned(conflictsMigrates)) or (conflictsMigrates.Count = 0) then
      ShowMessageNotFounded()
    else
      ShowMessageFounded(conflictsMigrates);

  finally

    if Assigned(conflictsMigrates) then
      conflictsMigrates.Free();

  end;

end;

procedure TUpdateCheck.Init;
begin
  inherited;

  FMigrateService := TMigrateService.Create(FPackage);
  FUpdateService := TUpdateService.Create();

end;

procedure TUpdateCheck.SetVersion(const version: String);
begin
  FVersion := version;
end;

procedure TUpdateCheck.ShowMessageError(const error: String);
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError(Format('%s :(', [error]));
  FConsoleIO.WriteError('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateCheck.ShowMessageFounded(ConflictsMigrates: TDictionary<string,TList<string >> );
var
  Key, ListMigrates: String;
  ConflictMigrateList: TList<String>;
begin

  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteError('* ------- ');
  FConsoleIO.WriteError('| Conflicts founded :( ');
  FConsoleIO.WriteError('* ----------------------------------------------------- ');

  try

    for Key in ConflictsMigrates.Keys do
    begin

      ConflictsMigrates.TryGetValue(Key, ConflictMigrateList);

      if not ConflictsMigrates.Count > 1 then
        continue;

      ListMigrates := String.Join(', ', ConflictMigrateList.ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [Key, ListMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    if Assigned(ConflictMigrateList) then
      ConflictMigrateList.Free();

  end;

end;

procedure TUpdateCheck.ShowMessageMigratesNotFounded;
begin
  FConsoleIO.WriteInfo(' ');
  FConsoleIO.WriteInfo('* ------- ');
  FConsoleIO.WriteInfo('| None Migrate Founded! ');
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');
  FConsoleIO.WriteInfo(' ');
end;

procedure TUpdateCheck.ShowMessageNotFounded;
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
