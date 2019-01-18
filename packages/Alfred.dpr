program Alfred;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  System.SysUtils,
  Console in '..\libs\Console.pas',
  Eagle.Alfred.DprojParser in '..\src\Eagle.Alfred.DprojParser.pas',
  Eagle.Alfred in '..\src\Eagle.Alfred.pas',
  Eagle.Alfred.Utils in '..\src\Eagle.Alfred.Utils.pas',
  Eagle.Alfred.Migrate.Repository.MigrateRepository in '..\src\migrate\repository\Eagle.Alfred.Migrate.Repository.MigrateRepository.pas',
  Eagle.Alfred.Migrate.Model.Migrate in '..\src\migrate\model\Eagle.Alfred.Migrate.Model.Migrate.pas',
  Eagle.Alfred.Core.CodeGenerator in '..\src\core\Eagle.Alfred.Core.CodeGenerator.pas',
  Eagle.Alfred.Core.Command in '..\src\core\Eagle.Alfred.Core.Command.pas',
  Eagle.Alfred.Core.CommandRegister in '..\src\core\Eagle.Alfred.Core.CommandRegister.pas',
  Eagle.Alfred.Core.Enums in '..\src\core\Eagle.Alfred.Core.Enums.pas',
  Eagle.Alfred.Core.IOUtils in '..\src\core\Eagle.Alfred.Core.IOUtils.pas',
  Eagle.Alfred.Command.Init in '..\src\command\Eagle.Alfred.Command.Init.pas',
  Eagle.Alfred.Command.Config.Edit in '..\src\command\config\Eagle.Alfred.Command.Config.Edit.pas',
  Eagle.Alfred.Command.Generate.Crud in '..\src\command\generate\Eagle.Alfred.Command.Generate.Crud.pas',
  Eagle.Alfred.Command.Generate.CrudFile in '..\src\command\generate\Eagle.Alfred.Command.Generate.CrudFile.pas',
  Eagle.Alfred.Command.Generate.Migrate in '..\src\command\generate\Eagle.Alfred.Command.Generate.Migrate.pas',
  Eagle.Alfred.Command.Generate.Model in '..\src\command\generate\Eagle.Alfred.Command.Generate.Model.pas',
  Eagle.Alfred.Command.Generate.Repository in '..\src\command\generate\Eagle.Alfred.Command.Generate.Repository.pas',
  Eagle.Alfred.Command.Generate.Service in '..\src\command\generate\Eagle.Alfred.Command.Generate.Service.pas',
  Eagle.Alfred.Command.Generate.Test in '..\src\command\generate\Eagle.Alfred.Command.Generate.Test.pas',
  Eagle.Alfred.Command.Generate.View in '..\src\command\generate\Eagle.Alfred.Command.Generate.View.pas',
  Eagle.Alfred.Command.Generate.ViewModel in '..\src\command\generate\Eagle.Alfred.Command.Generate.ViewModel.pas',
  Eagle.Alfred.Command.Destroy in '..\src\command\destroy\Eagle.Alfred.Command.Destroy.pas',
  Eagle.Alfred.Command.Destroy.Model in '..\src\command\destroy\Eagle.Alfred.Command.Destroy.Model.pas',
  Eagle.Alfred.Command.New in '..\src\command\Eagle.Alfred.Command.New.pas',
  Eagle.Alfred.Core.Types in '..\src\core\Eagle.Alfred.Core.Types.pas',
  Eagle.Alfred.Command.DB.Update.Check in '..\src\command\db\update\Eagle.Alfred.Command.DB.Update.Check.pas',
  Eagle.Alfred.Command.DB.Update.Join in '..\src\command\db\update\Eagle.Alfred.Command.DB.Update.Join.pas',
  Eagle.Alfred.Command.DB.Update.Run in '..\src\command\db\update\Eagle.Alfred.Command.DB.Update.Run.pas',
  Eagle.Alfred.Command.DB.Migrate.Execute in '..\src\command\db\migrate\Eagle.Alfred.Command.DB.Migrate.Execute.pas',
  Eagle.Alfred.Command.DB.Migrate.Rollback in '..\src\command\db\migrate\Eagle.Alfred.Command.DB.Migrate.Rollback.pas',
  Eagle.Alfred.Migrate.Service.MigrateService in '..\src\migrate\service\Eagle.Alfred.Migrate.Service.MigrateService.pas',
  Eagle.Alfred.Commond.DB.Update.UpdateService in '..\src\command\db\update\Eagle.Alfred.Commond.DB.Update.UpdateService.pas',
  Eagle.Alfred.Command.DB.Common.Repository in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.Repository.pas',
  Eagle.Alfred.Command.DB.Common.Connection in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.Connection.pas',
  Eagle.Alfred.Command.DB.Common.FiredacConnection in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.FiredacConnection.pas',
  Eagle.Alfred.Core.Exceptions in '..\src\core\Eagle.Alfred.Core.Exceptions.pas',
  Eagle.Alfred.Core.Attributes in '..\src\core\Eagle.Alfred.Core.Attributes.pas',
  Eagle.Alfred.Core.ConsoleIO in '..\src\core\Eagle.Alfred.Core.ConsoleIO.pas',
  Eagle.Alfred.Command.New.Project in '..\src\command\new\Eagle.Alfred.Command.New.Project.pas',
  Eagle.Alfred.Command.DB.MigrateInit in '..\src\command\db\Eagle.Alfred.Command.DB.MigrateInit.pas';

var
  OldColor: Byte;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
	{$ENDIF}

  try

    TAlfred.GetInstance.Run();

  except
    on E: Exception do
    begin
      OldColor := TextColor;
      TextColor(Red);
      Writeln(E.Message);
      TextColor(OldColor);
    end;
  end;
end.
