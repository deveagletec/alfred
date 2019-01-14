unit Eagle.Alfred.Command.New.Migrate;

interface

uses
  Classes,
  System.DateUtils,
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('new', 'migrate', 'Cria um novo arquivo de migrate')]
  TNewMigrateCommand = class(TCommandAbstract)
  private

    FMigrateName: String;
    FVersion: String;

  public

    procedure Execute; override;
    procedure Help; override;

    [ParamAttribute(1, 'Nome do migrate')]
    procedure setName(const name: String);

    [ParamAttribute(2, 'Vers�o em que o migrate dever� ser executado', False)]
    procedure setVersion(const version: String);

  end;

implementation

procedure TNewMigrateCommand.Execute;
var
  fileName, TimeStamp: string;
  Migrate: TStringList;
begin

  if FVersion.Trim().IsEmpty() then
    FVersion := FPackage.version;

  TimeStamp := DateTimeToUnix(Now).ToString;

  Migrate := TStringList.Create;

  Migrate.LoadFromFile('.\templates\FileMigrate.json');

  Migrate.Text := Migrate.Text.Replace('{IssueIdentifier}', FMigrateName.ToUpper, [rfReplaceAll]);
  Migrate.Text := Migrate.Text.Replace('{UnixIdentifier}', TimeStamp, [rfReplaceAll]);
  Migrate.Text := Migrate.Text.Replace('{Version}', FVersion, [rfReplaceAll]);

  fileName := Format('%s%s%s_%s.json', [FPackage.BaseDir, FPackage.MigrationDir, TimeStamp, FMigrateName.ToUpper]);

  try

    Migrate.SaveToFile(fileName);

  finally
    Migrate.Free;
  end;

end;

procedure TNewMigrateCommand.Help;
begin

  FConsoleIO.WriteInfo('');
  FConsoleIO.WriteInfo('         Alfred - Code Generate for Delphi');
  FConsoleIO.WriteInfo('-----------------------------------------------------');

  FConsoleIO.WriteInfo('              Cria��o de novo migrate,');
  FConsoleIO.WriteInfo(' Par�metros esperados:');
  FConsoleIO.WriteInfo('| [nome do migration] > Nome identificador do migrate');
  FConsoleIO.WriteInfo('| [vers�o (n�o obrigat�rio)] > Vers�o em que o migrate dever� ser executado');

end;

procedure TNewMigrateCommand.setName(const name: String);
begin
  FMigrateName := name;
end;

procedure TNewMigrateCommand.setVersion(const version: String);
begin
  FVersion := version;
end;

initialization

TAlfred.GetInstance.Register(TNewMigrateCommand);

end.
