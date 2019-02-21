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
    FForce: Boolean;
    FGlobal: Boolean;
  protected
    procedure Init; override;
  public
    procedure Execute; override;

    [Param(1, 'Dependency', False)]
    procedure SetDependency(const Name: string);

    [Option('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [Option('global', '-g', 'The -g or --global argument will cause npm to install the package globally rather than locally')]
    procedure Global;
  end;

implementation

{ TInstallCommand }

procedure TInstallCommand.Execute;
begin

  FDependencyResolver.SetForce(FForce);

  if FDependency.IsEmpty then
    FDependencyResolver.ResolverAll
  else
    FDependencyResolver.Install(FDependency);

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
end;

procedure TInstallCommand.SetDependency(const Name: string);
begin
  FDependency := Name;
end;

initialization
  TAlfred.GetInstance.Register(TInstallCommand);
end.
