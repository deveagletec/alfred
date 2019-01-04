unit Eagle.Alfred.Command.New.Project;

interface
uses
  System.IOUtils,

  Eagle.Alfred,
  Eagle.Alfred.Attributes,
  Eagle.Alfred.Core.Command;

type
  [Command('new', 'project', 'Cria um novo projeto')]
  TNewProjectCommand = class(TCommandAbstract)
  private
    FName: string;
    FVerbose: Boolean;
  public
    procedure Execute; override;
    procedure Help; override;

    [ParamAttribute('name', 'Nome do projeto')]
    procedure SetName(const Name: string);

    [OptionAttribute('verbose', 'v', 'Adds more details to output logging.')]
    procedure Verbose;
  end;

implementation

{ TNewProjectCommand }

procedure TNewProjectCommand.Execute;
begin
 FConsoleIO.WriteInfo('New project');
end;

procedure TNewProjectCommand.Help;
begin

end;

procedure TNewProjectCommand.SetName(const Name: string);
begin
  FName := Name;
end;

procedure TNewProjectCommand.Verbose;
begin
  FVerbose := True;
end;

initialization
  TAlfred.GetInstance.Register(TNewProjectCommand);
end.
