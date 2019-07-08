unit Eagle.Alfred.Core.ConsoleIO;

interface
uses
  System.SysUtils,
  System.StrUtils,
  Console;

type

   IConsoleIO = interface
      ['{53469311-2F04-4568-9928-5E85CB2822EC}']
      procedure NewEmptyLine;
      procedure WriteInfo(const Msg : string);
      procedure WriteInfoFmt(const Msg: string; const Args: array of const);
      procedure WriteError(const Msg : string);
      procedure WriteProcess(const Msg : string; const Color: Byte = LightGray);
      procedure WriteSuccess(const Msg: string);
      procedure WriteSuccessFmt(const Msg: string; const Args: array of const);
      function ReadInfo(const Msg: String; Color: Byte = LightGray): String;
      procedure WriteAlert(const msg: String);
      procedure WriteAlertFmt(const Msg: string; const Args: array of const);
      function ReadData(const Msg: string): string;
      function ReadBoolean(const Msg: string; const Default: Boolean): Boolean;
   end;

   TConsoleIO = class(TInterfacedObject, IConsoleIO)
   private
      procedure WriteColor(const Text: string; Color: Byte; const NewLine : Boolean = True);
   public
      procedure NewEmptyLine;
      function ReadInfo(const Msg: string; Color: Byte = LightGray): String;
      procedure WriteAlert(const msg: string);
      procedure WriteAlertFmt(const Msg: string; const Args: array of const);
      procedure WriteInfo(const Msg : string);
      procedure WriteInfoFmt(const Msg: string; const Args: array of const);
      procedure WriteError(const Msg : string);
      procedure WriteProcess(const Msg : string; const Color: Byte = LightGray);
      procedure WriteSuccess(const Msg: string);
      procedure WriteSuccessFmt(const Msg: string; const Args: array of const);
      function ReadData(const Msg: string): string;
      function ReadBoolean(const Msg: string; const Default: Boolean): Boolean;
   end;

implementation

procedure TConsoleIO.NewEmptyLine;
begin
  WriteInfo('');
end;

function TConsoleIO.ReadBoolean(const Msg: string; const Default: Boolean): Boolean;
var
  Value: string;
begin

  repeat
    Value := ReadData(Msg).Trim.ToLower;

    if MatchStr(Value, ['', 'yes', 'y', 'no', 'n']) then
      Break;

     WriteColor('Please answer yes, y, no or n', Red);
  until (False);

  if Value.IsEmpty then
    Exit(Default);

  Result := Value.StartsWith('y');
end;

function TConsoleIO.ReadData(const Msg: string): string;
begin
  WriteColor(Msg, LightGray, False);
  Readln(Result);
end;

function TConsoleIO.ReadInfo(const Msg: String; Color: Byte = LightGray): String;
begin
  WriteColor(Msg, Color);
  Result := ReadKey;

end;

procedure TConsoleIO.WriteAlert(const msg: String);
begin
  WriteColor(Msg, Yellow);
end;

procedure TConsoleIO.WriteAlertFmt(const Msg: string; const Args: array of const);
begin
  WriteColor(string.Format(Msg, Args), Yellow);
end;

procedure TConsoleIO.WriteColor(const Text: string; Color: Byte; const NewLine : Boolean = True);
var
  OldColor: Byte;
begin

  OldColor := TextColor;
  TextColor(Color);

  Write(Text);

  if NewLine then
    Writeln('');

  TextColor(OldColor);

end;

procedure TConsoleIO.WriteError(const Msg: string);
begin
   WriteColor(Msg, Red);
end;

procedure TConsoleIO.WriteInfo(const Msg: string);
begin
   WriteColor(Msg, LightGray);
end;

procedure TConsoleIO.WriteInfoFmt(const Msg: string; const Args: array of const);
begin
  WriteInfo(string.Format(Msg, Args));
end;

procedure TConsoleIO.WriteProcess(const Msg : string; const Color: Byte);
begin
   GotoXY(WhereX - (WhereX -1), WhereY);
   WriteColor(Msg, Color, False);
end;

procedure TConsoleIO.WriteSuccess(const Msg: string);
begin
   WriteColor(Msg, Green);
end;

procedure TConsoleIO.WriteSuccessFmt(const Msg: string; const Args: array of const);
begin
  WriteSuccess(string.Format(Msg, Args));
end;

end.
