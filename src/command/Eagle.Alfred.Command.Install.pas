unit Eagle.Alfred.Command.Install;

interface
uses
  System.SysUtils,
  System.IOUtils,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.DependencyResolver;

type
  [PackageRequired]
  [Command('install', '', 'Generates a Test')]
  TInstallCommand = class(TCommandAbstract)
  private
    FDependencyResolver : IDependencyResolver;
    FDependency: string;
    FSaveProd: Boolean;
    FSaveDev: Boolean;
    FForce: Boolean;
    FGlobal: Boolean;

    procedure Init; override;
  public
    procedure Execute; override;

    [ParamAttribute(1, 'Dependency', False)]
    procedure SetDependency(const Name: string);

    [OptionAttribute('save-prod', '-P', 'Package will appear in your dependencies.')]
    procedure SaveProd;

    [OptionAttribute('save-dev', '-D', 'Package will appear in your devDependencies.')]
    procedure SaveDev;

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [OptionAttribute('global', '-g', 'The -g or --global argument will cause npm to install the package globally rather than locally')]
    procedure Global;
  end;

implementation

{ TInstallCommand }

procedure TInstallCommand.Execute;
begin

  if FDependency.IsEmpty then
    FDependencyResolver.ResolverAll
 // else
 //   FDependencyResolver.Install(FDependency);

end;

procedure TInstallCommand.Force;
begin
  FForce := True;
end;

procedure TInstallCommand.Global;
begin
  FGlobal := True;
end;

procedure TInstallCommand.Init;
begin
  inherited;
  FDependencyResolver := TDependencyResolver.Create(FPackage, FConsoleIO);
  FSaveProd := True;
end;

procedure TInstallCommand.SaveDev;
begin
  FSaveDev := True;
end;

procedure TInstallCommand.SaveProd;
begin
  FSaveProd := True;
end;

procedure TInstallCommand.SetDependency(const Name: string);
begin
  FDependency := Name;
end;

initialization
  TAlfred.GetInstance.Register(TInstallCommand);
end.
