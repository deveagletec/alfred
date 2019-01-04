program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Eagle.Alfred.Attributes in '..\src\Eagle.Alfred.Attributes.pas',
  Eagle.Alfred.CodeGenerator in '..\src\Eagle.Alfred.CodeGenerator.pas',
  Eagle.Alfred.Data in '..\src\Eagle.Alfred.Data.pas',
  Eagle.Alfred.DprojParser in '..\src\Eagle.Alfred.DprojParser.pas',
  Eagle.Alfred.Exceptions in '..\src\Eagle.Alfred.Exceptions.pas',
  Eagle.Alfred.MigrateService in '..\src\Eagle.Alfred.MigrateService.pas',
  Eagle.Alfred in '..\src\Eagle.Alfred.pas',
  Eagle.Alfred.Utils in '..\src\Eagle.Alfred.Utils.pas',
  Eagle.ConsoleIO in '..\src\Eagle.ConsoleIO.pas',
  Eagle.Alfred.Core.Command in '..\src\core\Eagle.Alfred.Core.Command.pas',
  Console in '..\libs\Console.pas',
  Eagle.Alfred.Command.New.Project in '..\src\command\new\Eagle.Alfred.Command.New.Project.pas';

begin

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
