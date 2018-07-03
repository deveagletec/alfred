unit Eagle.Alfred;

interface
uses
  System.SysUtils,
  Eagle.ConsoleIO,
  Eagle.Alfred.Command,
  Eagle.Alfred.CreateCommand,
  Eagle.Alfred.MigrateCommand,
  Eagle.Alfred.ProjectCommand,
  Eagle.Alfred.HelpCommand,
  Eagle.Alfred.DprojParser;

type

  TAlfred = class
  private
    FCurrentPath: string;
    FAppPath: string;
    FDprojParser: TDprojParser;
    FConsoleIO: IConsoleIO;
    function GetCommand: ICommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run();
  end;

implementation

{ TAlfred }

constructor TAlfred.Create;
begin
  FDprojParser := TDprojParser.Create('.\packages\DelphiXE8\', 'EagleGestao');
  FCurrentPath := GetCurrentDir;
  FAppPath := ExtractFilePath(ParamStr(0));
  FConsoleIO := TConsoleIO.Create;
end;

destructor TAlfred.Destroy;
begin

  if Assigned(FDprojParser) then
    FreeAndNil(FDprojParser);

  inherited;
end;

function TAlfred.GetCommand: ICommand;
var
  Cmd: string;
begin

  Cmd := ParamStr(1).ToUpper;

  if Cmd.Equals('CREATE') then
  begin
    Result := TCreateCommand.Create(FAppPath, FConsoleIO, FDprojParser);
    Exit;
  end;

  if Cmd.Equals('PROJECT') then
  begin
    Result := TProjectCommand.Create(FAppPath, FConsoleIO, FDprojParser);
    Exit;
  end;

  if Cmd.Equals('MIGRATE') then
  begin
    Result := TMigrateCommand.Create(FAppPath, FConsoleIO);
    Exit;
  end;

  Result := THelpCommand.Create(FConsoleIO);

end;

procedure TAlfred.Run;
var
  Cmd: ICommand;
begin

  Cmd := GetCommand;

  Cmd.Execute;

end;

end.
