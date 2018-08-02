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
  Eagle.Alfred.Command.Migrate in 'commands\Eagle.Alfred.Command.Migrate.pas',
  Eagle.Alfred.Command.Project in 'commands\Eagle.Alfred.Command.Project.pas',
  Console in 'libs\Console.pas',
  Eagle.Alfred.Exceptions in 'Eagle.Alfred.Exceptions.pas',
  Eagle.Alfred.Command.Generate in 'commands\Eagle.Alfred.Command.Generate.pas',
  Eagle.Alfred.MigrateService in 'Eagle.Alfred.MigrateService.pas',
  Eagle.Alfred.Utils in 'Eagle.Alfred.Utils.pas',
  Eagle.Alfred.Generate.View in 'generates\Eagle.Alfred.Generate.View.pas',
  Eagle.Alfred.Generate.ViewModel in 'generates\Eagle.Alfred.Generate.ViewModel.pas',
  Eagle.Alfred.Generate.Model in 'generates\Eagle.Alfred.Generate.Model.pas',
  Eagle.Alfred.Generate.Entity in 'generates\Eagle.Alfred.Generate.Entity.pas',
  Eagle.Alfred.Generate.Repository in 'generates\Eagle.Alfred.Generate.Repository.pas',
  Eagle.Alfred.Generate.Service in 'generates\Eagle.Alfred.Generate.Service.pas',
  Eagle.Alfred.Generate.Test in 'generates\Eagle.Alfred.Generate.Test.pas',
  Eagle.Alfred.Generate.UnitGenerate in 'generates\Eagle.Alfred.Generate.UnitGenerate.pas',
  Eagle.Alfred.CodeGenerator in 'Eagle.Alfred.CodeGenerator.pas';

begin

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
