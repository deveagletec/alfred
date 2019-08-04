unit Eagle.Alfred.Command.DB.Migrate.List;

interface

uses
  SysUtils, StrUtils,
  System.Generics.Collections,

  Eagle.Alfred,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Command.Common.Migrate.Model,

  Eagle.Alfred.Command.Common.Migrate.Service;

type

  [Command('db:migrate', 'list', 'List Migrates')]
  TMigrateListCommand = class(TCommandAbstract)
  private
    FMigrateService: IMigrateService;
    FMigrates: TList<TMigrate>;

    FVersion: string;
    FIsInDetailedMode: Boolean;

    procedure DoShowView;
    procedure DoShowHeader;
    procedure DoShowFooter;

  public

    procedure Execute(); override;
    procedure Init(); override;

    [Param(1, 'Version filter', False)]
    procedure SetVersion(const Version: string);

    [Option('detailed', 'd', 'Detailed View')]
    procedure SetIsDetailed();

  end;

const
  SEPARATOR_LINE = '* ---------------------------------------------------------------------- ';
  BIG_SEPARATOR_LINE = '* --------------------------------------------------------------------------------------------------------------------------------------- ';

implementation

procedure TMigrateListCommand.Execute;
begin
  inherited;

  if FVersion.IsEmpty then
    FMigrates := FMigrateService.GetMigratesByMigrationDir()
  else
    FMigrates := FMigrateService.GetMigratesByVersion(FVersion);

  try

    if (not Assigned(FMigrates)) or (FMigrates.Count = 0) then
      raise EMigrationsNotFoundException.Create('No migration found');

    DoShowView();

  finally

    if Assigned(FMigrates) then
      FMigrates.Free();

  end;

end;

procedure TMigrateListCommand.DoShowView;
var
  Migrate: TMigrate;
  DescriptionMigrate: string;
begin

  DoShowHeader;

  for Migrate in FMigrates do
  begin

    DescriptionMigrate := Migrate.Id.PadRight(17, ' ') + Migrate.Issue.PadRight(14, ' ') + Migrate.Version.PadRight(14, ' ') + Migrate.Author.Substring(0, 15).PadRight(17, ' ') +
      IfThen(Migrate.WasExecuted, 'Executed', 'Not Executed').PadRight(15, ' ');

    if FIsInDetailedMode then
      DescriptionMigrate := DescriptionMigrate + Migrate.Description;

    if Migrate.WasExecuted then
      FConsoleIO.WriteSuccessFmt('| %s', [DescriptionMigrate])
    else
      FConsoleIO.WriteInfoFmt('| %s', [DescriptionMigrate]);

  end;

  DoShowFooter;

end;

procedure TMigrateListCommand.DoShowFooter;
begin
  FConsoleIO.WriteAlert('| ');
  FConsoleIO.WriteAlert(IfThen(FIsInDetailedMode, BIG_SEPARATOR_LINE, SEPARATOR_LINE));
end;

procedure TMigrateListCommand.DoShowHeader;
begin
  FConsoleIO.WriteAlert(sLineBreak + '* -------------------------');
  FConsoleIO.WriteAlert('| MIGRATES');
  FConsoleIO.WriteAlert(IfThen(FIsInDetailedMode, BIG_SEPARATOR_LINE, SEPARATOR_LINE));
  FConsoleIO.WriteAlert('| ID               ISSUE         VERSION       AUTHOR           STATUS         ' + IfThen(FIsInDetailedMode, 'DESCRIPTION', ''));
  FConsoleIO.WriteAlert('|');
end;

procedure TMigrateListCommand.Init;
begin
  inherited;
  FMigrateService := TMigrateService.Create(FPackage);

  FIsInDetailedMode := False;
end;

procedure TMigrateListCommand.SetIsDetailed;
begin
  FIsInDetailedMode := True;
end;

procedure TMigrateListCommand.SetVersion(const Version: string);
begin
  FVersion := Version;
end;

initialization

TAlfred.GetInstance.Register(TMigrateListCommand);

end.
