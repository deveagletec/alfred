unit Eagle.Alfred.Command.Update;

interface
uses
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.Command.Common.DependencyResolver;

type

  [PackageRequired]
  [Command('update', '', 'Update a package')]
  TUpdateCommand = class(TCommandAbstract)
  private
    FDependencyResolver : IDependencyResolver;
    FGlobal: Boolean;
  protected
    procedure Init; override;
  public
    procedure Execute; override;

    [Option('global', '-g', 'Update globally installed packages.')]
    procedure Global;
  end;

implementation

{ TUpdateCommand }

procedure TUpdateCommand.Execute;
begin
  inherited;
  FDependencyResolver.UpdateAll;
end;

procedure TUpdateCommand.Global;
begin
  FGlobal := True;
end;

procedure TUpdateCommand.Init;
begin
  inherited;
  FDependencyResolver := TDependencyResolver.Create(FPackage, FConsoleIO);
end;

initialization
  TAlfred.GetInstance.Register(TUpdateCommand);
end.
