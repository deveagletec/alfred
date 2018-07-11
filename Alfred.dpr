program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Eagle.Alfred in 'Eagle.Alfred.pas',
  Eagle.Alfred.DprojParser in 'Eagle.Alfred.DprojParser.pas',
  Eagle.ConsoleIO in 'Eagle.ConsoleIO.pas',
  Eagle.Alfred.Command in 'Eagle.Alfred.Command.pas',
  Eagle.Alfred.Attributes in 'Eagle.Alfred.Attributes.pas',
  Eagle.Alfred.Data in 'Eagle.Alfred.Data.pas',
  Eagle.Alfred.CommandRegister in 'Eagle.Alfred.CommandRegister.pas',
  Eagle.Alfred.Command.Crud in 'commands\Eagle.Alfred.Command.Crud.pas',
  Eagle.Alfred.Command.Migrate in 'commands\Eagle.Alfred.Command.Migrate.pas',
  Eagle.Alfred.Command.Project in 'commands\Eagle.Alfred.Command.Project.pas',
  Console in 'libs\Console.pas',
  Eagle.Alfred.Exceptions in 'Eagle.Alfred.Exceptions.pas';

begin

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
