unit Eagle.Alfred.Commom.Utils.IOUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

type
  TIOUtils = class
  public
    class function FileIsEncodedUTF8(const fileIO: String): Boolean;
  end;

implementation

class function TIOUtils.FileIsEncodedUTF8(const fileIO: String): Boolean;
var
  Stream: TMemoryStream;
  BytesRead: Integer;
  ArrayBuff: array [0 .. 127] of byte;
  PreviousByte: byte;
  I: Integer;
  YesSequences, NoSequences: Integer;
begin

  YesSequences := 0;
  NoSequences := 0;

  Stream := TMemoryStream.Create;

  try

    Stream.LoadFromFile(fileIO);

    repeat

      BytesRead := Stream.Read(ArrayBuff, High(ArrayBuff) + 1);

      if BytesRead > 1 then
      begin
        for I := 1 to BytesRead - 1 do
        begin
          PreviousByte := ArrayBuff[I - 1];
          if ((ArrayBuff[I] and $C0) = $80) then
          begin
            if ((PreviousByte and $C0) = $C0) then
            begin
              inc(YesSequences)
            end
            else
            begin
              if ((PreviousByte and $80) = $0) then
                inc(NoSequences);
            end;
          end;
        end;
      end;
    until (BytesRead < (High(ArrayBuff) + 1));

  finally
    Stream.Free;
  end;

  Result := YesSequences > NoSequences;

end;

end.
