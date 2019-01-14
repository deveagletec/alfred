unit Eagle.Alfred.Command.Install;

interface
uses
  System.IOUtils,

  XSuperObject,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
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

    procedure Init; override;
  public
    procedure Execute; override;

    //[ParamAttribute(1, 'Dependency')]
    procedure SetDependency(const Name: string);

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;

    [OptionAttribute('global', '-g', 'The -g or --global argument will cause npm to install the package globally rather than locally')]
    procedure Global;
  end;

implementation

{ TInstallCommand }

procedure TInstallCommand.Execute;
begin

  FDependencyResolver.ResolverAll;

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
