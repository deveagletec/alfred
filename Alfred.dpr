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
  Eagle.Alfred.ProjectCommand in 'Eagle.Alfred.ProjectCommand.pas',
  Eagle.Alfred.Attributes in 'Eagle.Alfred.Attributes.pas',
  Eagle.Alfred.CrudCommand in 'Eagle.Alfred.CrudCommand.pas',
  Eagle.Alfred.CommandRegister in 'Eagle.Alfred.CommandRegister.pas',
  Eagle.Alfred.Data in 'Eagle.Alfred.Data.pas';

begin

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
