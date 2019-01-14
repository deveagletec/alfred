unit Eagle.Alfred.Command.Init;

interface
uses
  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type

  [Command('init', '', 'Generates package.json')]
  TInitCommand = class(TCommandAbstract)
  public
    procedure Execute; override;
  end;

implementation

{ TInitCommand }

procedure TInitCommand.Execute;
begin
  inherited;

  FConsoleIO.WriteInfo('init');
end;

initialization
  TAlfred.GetInstance.Register(TInitCommand);
end.
