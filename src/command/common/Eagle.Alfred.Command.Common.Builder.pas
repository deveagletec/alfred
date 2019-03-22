unit Eagle.Alfred.Command.Common.Builder;

interface

uses
  System.SysUtils,
  System.IOUtils,
  Winapi.ShellApi,
  Windows,
  Eagle.Alfred.Core.Types;

type
  TAnoPipe = record
    Input: THandle;
    Output: THandle;
  end;

  IBuilder = interface
    ['{2E4E62F8-1909-49D4-BCAC-758863D6B762}']
    procedure Build(Dependency: TDependency; const Mode: string);
  end;

  TBuilder = class(TInterfacedObject, IBuilder)
  private
    function ExecuteProcess(const Cmd, Params: string): Boolean;
  public
    procedure Build(Dependency: TDependency; const Mode: string);
  end;

implementation

{ TBuilder }

procedure TBuilder.Build(Dependency: TDependency; const Mode: string);
var
  Params, ProjectFilePath: string;
begin
  ProjectFilePath := Dependency.VendorPath + '\source' + Dependency.PackageFile;

  if not TFile.Exists(ProjectFilePath) then
    raise Exception.CreateFmt('Project Package %s not found', [ProjectFilePath.QuotedString]);

  Params := string.Format('%s %s %s', [Dependency.VendorPathFull, ProjectFilePath, Mode]);

  if not ExecuteProcess('.\Scripts\Build.bat', Params) then
    raise Exception.Create('Error Build');
end;

function TBuilder.ExecuteProcess(const Cmd, Params: string): Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array [0 .. 255] of AnsiChar;
  BytesRead: Cardinal;
  Handle: Boolean;
  Output: string;
  ExitCode: DWORD;
begin
  Result := False;

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    Handle := CreateProcess(nil, PChar(Cmd + ' ' + Params), nil, nil, True, 0, nil, nil, SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Output := string(Buffer).Trim;
            writeln(Output);
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);

        GetExitCodeProcess(PI.hProcess, ExitCode);
        Result := ExitCode = 0;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

end.
