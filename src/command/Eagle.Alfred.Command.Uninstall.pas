unit Eagle.Alfred.Command.Uninstall;

interface
uses
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.DependencyResolver;

type

  [PackageRequired]
  [Command('uninstall', '', 'Remove a package')]
  TUninstallCommand = class(TCommandAbstract)
  private
    FDependencyResolver : IDependencyResolver;
    FDependency: string;
    FSaveProd: Boolean;
    FSaveDev: Boolean;
    FForce: Boolean;
    FGlobal: Boolean;
  protected
    procedure Init; override;
  public
    procedure Execute; override;

    [Param(1, 'Dependency')]
    procedure SetDependency(const Name: string);

    [Option('save-prod', '-P', 'Package will appear in your dependencies.')]
    procedure SaveProd;

    [Option('save-dev', '-D', 'Package will appear in your devDependencies.')]
    procedure SaveDev;

    [Option('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [Option('global', '-g', 'Uninstalls the current package context as a global package.')]
    procedure Global;
  end;

implementation

{ TUninstallCommand }

procedure TUninstallCommand.Execute;
var
  Dependency: TDependency;
begin
  inherited;
  FDependencyResolver.Uninstall(Dependency);
end;

procedure TUninstallCommand.Force;
begin
  FForce := True;
end;

procedure TUninstallCommand.Global;
begin
  FGlobal := True;
end;

procedure TUninstallCommand.Init;
begin
  inherited;
  FDependencyResolver := TDependencyResolver.Create(FPackage, FConsoleIO);
  FSaveProd := True;
end;

procedure TUninstallCommand.SaveDev;
begin
  FSaveDev := True;
end;

procedure TUninstallCommand.SaveProd;
begin
  FSaveProd := True;
end;

procedure TUninstallCommand.SetDependency(const Name: string);
begin
  FDependency := Name.Trim;
end;

initialization
  TAlfred.GetInstance.Register(TUninstallCommand);
end.
