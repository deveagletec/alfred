program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  System.SysUtils,
  Eagle.Alfred.Attributes in '..\src\Eagle.Alfred.Attributes.pas',
  Eagle.Alfred.Data in '..\src\Eagle.Alfred.Data.pas',
  Eagle.Alfred.DprojParser in '..\src\Eagle.Alfred.DprojParser.pas',
  Eagle.Alfred.Exceptions in '..\src\Eagle.Alfred.Exceptions.pas',
  Eagle.Alfred.MigrateService in '..\src\Eagle.Alfred.MigrateService.pas',
  Eagle.Alfred in '..\src\Eagle.Alfred.pas',
  Eagle.Alfred.Utils in '..\src\Eagle.Alfred.Utils.pas',
  Eagle.ConsoleIO in '..\src\Eagle.ConsoleIO.pas',
  Eagle.Alfred.Core.Command in '..\src\core\Eagle.Alfred.Core.Command.pas',
  Console in '..\libs\Console.pas',
  Eagle.Alfred.Command.New.Project in '..\src\command\new\Eagle.Alfred.Command.New.Project.pas',
  Eagle.Alfred.Core.CommandRegister in '..\src\core\Eagle.Alfred.Core.CommandRegister.pas',
  Eagle.Alfred.Command.Generate.View in '..\src\command\generate\Eagle.Alfred.Command.Generate.View.pas',
  Eagle.Alfred.Command.Generate.ViewModel in '..\src\command\generate\Eagle.Alfred.Command.Generate.ViewModel.pas',
  Eagle.Alfred.Command.Generate.Model in '..\src\command\generate\Eagle.Alfred.Command.Generate.Model.pas',
  Eagle.Alfred.Command.Generate.Service in '..\src\command\generate\Eagle.Alfred.Command.Generate.Service.pas',
  Eagle.Alfred.Command.Generate.Repository in '..\src\command\generate\Eagle.Alfred.Command.Generate.Repository.pas',
  Eagle.Alfred.Command.Generate.Test in '..\src\command\generate\Eagle.Alfred.Command.Generate.Test.pas',
  Eagle.Alfred.Command.Generate.Crud in '..\src\command\generate\Eagle.Alfred.Command.Generate.Crud.pas',
  Eagle.Alfred.Core.CodeGenerator in '..\src\core\Eagle.Alfred.Core.CodeGenerator.pas',
  Eagle.Alfred.Command.Generate.CrudFile in '..\src\command\generate\Eagle.Alfred.Command.Generate.CrudFile.pas',
  Eagle.Alfred.Command.Delete.DeleteCrudFile in '..\src\command\delete\Eagle.Alfred.Command.Delete.DeleteCrudFile.pas',
  Eagle.Alfred.Command.Delete.Model in '..\src\command\delete\Eagle.Alfred.Command.Delete.Model.pas',
  Eagle.Alfred.Core.IOUtils in '..\src\core\Eagle.Alfred.Core.IOUtils.pas';

begin

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
	{$ENDIF}

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
