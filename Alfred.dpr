program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Eagle.Alfred.CreateCommand in 'Eagle.Alfred.CreateCommand.pas',
  Eagle.Alfred in 'Eagle.Alfred.pas',
  Eagle.Alfred.DprojParser in 'Eagle.Alfred.DprojParser.pas',
  Eagle.ConsoleIO in 'Eagle.ConsoleIO.pas',
  Console in 'Console.pas',
  Eagle.Alfred.MigrateCommand in 'Eagle.Alfred.MigrateCommand.pas',
  Eagle.Alfred.Command in 'Eagle.Alfred.Command.pas',
  Eagle.Alfred.HelpCommand in 'Eagle.Alfred.HelpCommand.pas',
  Eagle.Alfred.ProjectCommand in 'Eagle.Alfred.ProjectCommand.pas';

begin
  try
    with TAlfred.Create do
    try
      Run;
    finally
      Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
