program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  System.SysUtils,
  Eagle.Alfred.Attributes in '..\src\Eagle.Alfred.Attributes.pas',
  Eagle.Alfred.Data in '..\src\Eagle.Alfred.Data.pas',
  Eagle.Alfred.DprojParser in '..\src\Eagle.Alfred.DprojParser.pas',
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
  Eagle.Alfred.DB.Connection in '..\src\db\Eagle.Alfred.DB.Connection.pas',
  Eagle.Alfred.DB.Impl.FiredacConnection in '..\src\db\impl\Eagle.Alfred.DB.Impl.FiredacConnection.pas',
  Eagle.Alfred.Migrate.Model.Migrate in '..\src\migrate\model\Eagle.Alfred.Migrate.Model.Migrate.pas',
  Eagle.Alfred.Migrate.Repository.MigrateRepository in '..\src\migrate\repository\Eagle.Alfred.Migrate.Repository.MigrateRepository.pas',
  Eagle.Alfred.Core.Exceptions in '..\src\core\Exception\Eagle.Alfred.Core.Exceptions.pas',
  Eagle.Alfred.Command.DB.MigrateExecute in '..\src\command\db\Eagle.Alfred.Command.DB.MigrateExecute.pas',
  Eagle.Alfred.Migrate.Service.MigrateService in '..\src\migrate\service\Eagle.Alfred.Migrate.Service.MigrateService.pas',
  Eagle.Alfred.Core.Enums in '..\src\core\Eagle.Alfred.Core.Enums.pas',
  Eagle.Alfred.Command.DB.MigrateRollback in '..\src\command\db\Eagle.Alfred.Command.DB.MigrateRollback.pas',
  Eagle.Alfred.Core.IOUtils in '..\src\core\Eagle.Alfred.Core.IOUtils.pas',
  Eagle.Alfred.Command.Generate.Migrate in '..\src\command\generate\Eagle.Alfred.Command.Generate.Migrate.pas',
  Eagle.Alfred.Command.DB.UpdateCheck in '..\src\command\db\Eagle.Alfred.Command.DB.UpdateCheck.pas',
  Eagle.Alfred.Update.Service.UpdateService in '..\src\update\service\Eagle.Alfred.Update.Service.UpdateService.pas',
  Eagle.Alfred.Command.DB.UpdateJoin in '..\src\command\db\Eagle.Alfred.Command.DB.UpdateJoin.pas',
  Eagle.Alfred.Commom.Repository in '..\src\commom\repository\Eagle.Alfred.Commom.Repository.pas',
  Eagle.Alfred.Command.DB.UpdateRun in '..\src\command\db\Eagle.Alfred.Command.DB.UpdateRun.pas',
  Eagle.Alfred.Command.Init in '..\src\command\Eagle.Alfred.Command.Init.pas',
  Eagle.Alfred.Command.Config.Edit in '..\src\command\config\Eagle.Alfred.Command.Config.Edit.pas';

begin

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
	{$ENDIF}

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
    begin
      TextColor(Red);
      Writeln(E.Message);
    end;
  end;

end.
