unit Eagle.Alfred.Command.Generate.Migrate;

interface

uses
  Classes,
  System.DateUtils,
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('generate', 'migrate', 'Cria um novo arquivo de migrate')]
  TGenerateMigrateCommand = class(TCommandAbstract)
  private

    FMigrateName: String;
    FVersion: String;

  public

    procedure Execute; override;

    [ParamAttribute(1, 'Nome do migrate')]
    procedure setName(const name: String);

    [ParamAttribute(2, 'Versão em que o migrate deverá ser executado', False)]
    procedure setVersion(const version: String);

  end;

implementation

procedure TGenerateMigrateCommand.Execute;
var
  fileName, TimeStamp: string;
  Migrate: TStringList;
begin

  if FVersion.Trim().IsEmpty() then
    FVersion := FPackage.version;

  TimeStamp := DateTimeToUnix(Now).ToString;

  Migrate := TStringList.Create;

  try

    Migrate.LoadFromFile('.\templates\FileMigrate.json');

    Migrate.Text := Migrate.Text.Replace('{IssueIdentifier}', FMigrateName.ToUpper, [rfReplaceAll]);
    Migrate.Text := Migrate.Text.Replace('{UnixIdentifier}', TimeStamp, [rfReplaceAll]);
    Migrate.Text := Migrate.Text.Replace('{Version}', FVersion, [rfReplaceAll]);

    fileName := Format('%s%s%s_%s.json', [FPackage.BaseDir, FPackage.MigrationDir, TimeStamp, FMigrateName.ToUpper]);

    Migrate.SaveToFile(fileName);

    FConsoleIO.WriteInfo(' * ------- ');
    FConsoleIO.WriteInfo(' | Migrate Created Sucessfull ;) ');
    FConsoleIO.WriteInfo(' * ----------------------------------------------------- ');

  finally
    Migrate.Free;
  end;

end;

procedure TGenerateMigrateCommand.setName(const name: String);
begin
  FMigrateName := name;
end;

procedure TGenerateMigrateCommand.setVersion(const version: String);
begin
  FVersion := version;
end;

initialization

TAlfred.GetInstance.Register(TGenerateMigrateCommand);

end.
