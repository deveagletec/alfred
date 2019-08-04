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

  Eagle.Alfred.Command.Common.Migrate.Model,
  Eagle.Alfred.Command.Common.Migrate.Service,

  Eagle.Alfred.Commond.DB.Update.UpdateService;

type

  [Command('db:update', 'check', 'Check conflicts in migrates')]
  TUpdateCheck = class(TCommandAbstract)
  private

    FVersion: String;

    FMigrates: TList<TMigrate>;
    FMigrateService: IMigrateService;
    FUpdateService: IUpdateService;

    procedure ShowMessageFounded(ConflictsMigrates: TDictionary < string, TList < string >> );
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
  ConflictsMigrates: TDictionary<String, TList<String>>;
begin

  FMigrates := FMigrateService.GetMigratesByVersion(FVersion.Trim());

  if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
  begin
    ShowMessageNotFounded();
    Exit;
  end;

  ConflictsMigrates := FUpdateService.GetConflictMigrates(FMigrates);

  try

    if (not Assigned(ConflictsMigrates)) or (ConflictsMigrates.Count = 0) then
      ShowMessageNotFounded()
    else
      ShowMessageFounded(ConflictsMigrates);

  finally

    if Assigned(ConflictsMigrates) then
      ConflictsMigrates.Free();

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

procedure TUpdateCheck.ShowMessageFounded(ConflictsMigrates: TDictionary < string, TList < string >> );
var
  Key, ListMigrates: String;
begin

  FConsoleIO.NewEmptyLine;
  FConsoleIO.WriteAlert('* ------- ');
  FConsoleIO.WriteAlert('| Conflicts founded :( ');
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');

  try

    for Key in ConflictsMigrates.Keys do
    begin

      ListMigrates := String.Join(', ', ConflictsMigrates.Items[Key].ToArray());

      FConsoleIO.WriteInfo(Format('| %s >>>> %s', [Key, ListMigrates]));
      FConsoleIO.WriteInfo('  --------------');

    end;

  finally

    for Key in ConflictsMigrates.Keys do
      ConflictsMigrates.Items[Key].Free;

  end;

end;

procedure TUpdateCheck.ShowMessageNotFounded;
begin
  DoShowMessageSuccessful('Conflicts not found');
end;

initialization

TAlfred.GetInstance.Register(TUpdateCheck);

end.
