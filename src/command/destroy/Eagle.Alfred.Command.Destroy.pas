unit Eagle.Alfred.Command.Destroy;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Data,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command,
  Eagle.Alfred.DprojParser;

type

  [PackageRequired]
  TDestroyCommand = class(TCommandAbstract)
  protected
    FName: string;
    FModuleName: string;
    FForce: Boolean;
    FDprojParser : IDprojParser;
    FDprojTestParser : IDprojParser;

    procedure Init; override;
    procedure DoDestroyFile(const LayerName: string);
  public
    destructor Destroy; override;

    [ParamAttribute(1, 'Class name')]
    procedure SetName(const Value: string);

    [ParamAttribute(2, 'Module name')]
    procedure SetModuleName(const Value: string);

    [OptionAttribute('force', '-f', 'Forces overwriting of files.')]
    procedure Force;
  end;

implementation

destructor TDestroyCommand.Destroy;
begin
  if Assigned(FDprojParser) then
    FDprojParser.Save;
  if Assigned(FDprojTestParser) then
    FDprojTestParser.Save;
  inherited;
end;

procedure TDestroyCommand.DoDestroyFile(const LayerName: string);
begin

end;

procedure TDestroyCommand.Force;
begin
  FForce := True;
end;

procedure TDestroyCommand.Init;
begin
  inherited;
  FDprojParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Name);

  FDprojTestParser := TDprojParser.Create(FPackage.PackagesDir, FPackage.Name + 'Test');
end;

procedure TDestroyCommand.SetModuleName(const Value: string);
begin
  FModuleName := Value;
end;

procedure TDestroyCommand.SetName(const Value: string);
begin
  FName := Value;
end;

end.
