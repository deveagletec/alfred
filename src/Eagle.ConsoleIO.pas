unit Eagle.ConsoleIO;

interface
uses Console;

type

   IConsoleIO = interface
      ['{53469311-2F04-4568-9928-5E85CB2822EC}']
      procedure WriteInfo(const Msg : string);
      procedure WriteError(const Msg : string);
      procedure WriteProcess(const Msg : string);
   end;

   TConsoleIO = class(TInterfacedObject, IConsoleIO)
   private
      procedure WriteColor(const Text: string; Color: Byte; const NewLine : Boolean = True);
   public
      procedure WriteInfo(const Msg : string);
      procedure WriteError(const Msg : string);
      procedure WriteProcess(const Msg : string);
   end;

implementation

{ TConsoleIO }

procedure TConsoleIO.WriteColor(const Text: string; Color: Byte; const NewLine : Boolean = True);
var
  OldColor: Byte;
  OldColor := TextColor;

end;

procedure TConsoleIO.WriteError(const Msg: string);
begin
   WriteColor(Msg, Red);
end;

procedure TConsoleIO.WriteInfo(const Msg: string);
begin
   WriteColor(Msg, LightGray);
end;

procedure TConsoleIO.WriteProcess(const Msg: string);
begin
   GotoXY(WhereX - (WhereX -1), WhereY);
   WriteColor(Msg, LightGray, False);
end;

end.