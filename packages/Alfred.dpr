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
  Eagle.Alfred.Commond.DB.Update.UpdateService in '..\src\command\db\update\Eagle.Alfred.Commond.DB.Update.UpdateService.pas',
  Eagle.Alfred.Command.DB.Common.Repository in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.Repository.pas',
  Eagle.Alfred.Command.DB.Common.Connection in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.Connection.pas',
  Eagle.Alfred.Command.DB.Common.FiredacConnection in '..\src\command\db\common\Eagle.Alfred.Command.DB.Common.FiredacConnection.pas',
  Eagle.Alfred.Core.Exceptions in '..\src\core\Eagle.Alfred.Core.Exceptions.pas',
  Eagle.Alfred.Core.Attributes in '..\src\core\Eagle.Alfred.Core.Attributes.pas',
  Eagle.Alfred.Core.ConsoleIO in '..\src\core\Eagle.Alfred.Core.ConsoleIO.pas',
  Eagle.Alfred.Command.DB.MigrateInit in '..\src\command\db\Eagle.Alfred.Command.DB.MigrateInit.pas',
  Eagle.Alfred.DependencyResolver in '..\src\Eagle.Alfred.DependencyResolver.pas',
  Eagle.Alfred.Command.Install in '..\src\command\Eagle.Alfred.Command.Install.pas',
  Eagle.Alfred.Command.Uninstall in '..\src\command\Eagle.Alfred.Command.Uninstall.pas',
  Eagle.Alfred.Command.Update in '..\src\command\Eagle.Alfred.Command.Update.pas',
  Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader in '..\src\command\common\downloader\Eagle.Alfred.Command.Common.Downloaders.BitbucketDownloader.pas',
  Eagle.Alfred.Command.Common.Downloaders.Downloader in '..\src\command\common\downloader\Eagle.Alfred.Command.Common.Downloaders.Downloader.pas',
  Eagle.Alfred.Command.Common.Downloaders.GithubDownloader in '..\src\command\common\downloader\Eagle.Alfred.Command.Common.Downloaders.GithubDownloader.pas',
  Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader in '..\src\command\common\downloader\Eagle.Alfred.Command.Common.Downloaders.GitlabDownloader.pas',
  Eagle.Alfred.Command.Common.Downloaders.SourceForgeDownloader in '..\src\command\common\downloader\Eagle.Alfred.Command.Common.Downloaders.SourceForgeDownloader.pas',
  Eagle.Alfred.Command.Common.Migrate.Model in '..\src\command\common\migrate\Eagle.Alfred.Command.Common.Migrate.Model.pas',
  Eagle.Alfred.Command.Common.Migrate.Repository in '..\src\command\common\migrate\Eagle.Alfred.Command.Common.Migrate.Repository.pas',
  Eagle.Alfred.Command.Common.Migrate.Service in '..\src\command\common\migrate\Eagle.Alfred.Command.Common.Migrate.Service.pas',
  Eagle.Alfred.Command.DB.Migrate.List in '..\src\command\db\migrate\Eagle.Alfred.Command.DB.Migrate.List.pas',
  Eagle.Alfred.Command.DB.Migrate.Delete in '..\src\command\db\migrate\Eagle.Alfred.Command.DB.Migrate.Delete.pas';

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
