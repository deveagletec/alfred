unit Eagle.Alfred.Command.Update;

interface
uses
  System.SysUtils,

  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.DependencyResolver;

type

  [PackageRequired]
  [Command('update', '', 'Update a package')]
  TUpdateCommand = class(TCommandAbstract)
  private
    FDependencyResolver : IDependencyResolver;
    FGlobal: Boolean;

    procedure Init; override;
  public
    procedure Execute; override;

    [OptionAttribute('global', '-g', 'Update globally installed packages.')]
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
