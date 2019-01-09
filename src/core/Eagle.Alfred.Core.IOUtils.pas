unit Eagle.Alfred.Core.IOUtils;

interface
uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Eagle.Alfred.Core.Exceptions;

type
  TIOUtils = class
  public
    class procedure CreateDir(const Path: string; const Force: Boolean = False);
    class procedure CreateFile(const Path, Contents: string; const Force: Boolean = False);
    class function FileIsEncodedUTF8(const fileIO: String): Boolean;

  end;

implementation

{ TIOUtils }

class procedure TIOUtils.CreateDir(const Path: string; const Force: Boolean);
var
  Exists: Boolean;
begin
  Exists := TDirectory.Exists(Path);

  try

    if Exists then
    begin
      if Force then
        TDirectory.Delete(Path, True)
      else
        raise EAlfredCreateDirException.Create('Diretório "'+Path+'" já existente');
    end;

    TDirectory.CreateDirectory(Path);

  except
    on E: EAlfredException do
      raise;
    on E: Exception do
      raise EAlfredCreateFileException.Create('Erro ao criar o diretório "'+Path+'". ' + E.Message);
  end;
end;

class procedure TIOUtils.CreateFile(const Path, Contents: string; const Force: Boolean);
var
  Exists: Boolean;
begin

  Exists := TFile.Exists(Path);

  try

    if Exists then
    begin
      if Force then
        TFile.Delete(Path)
      else
        raise EAlfredCreateFileException.Create('Arquivo "'+Path+'" já existente');
    end;

    TFile.WriteAllText(Path, Contents);

  except
    on E: EAlfredException do
      raise;
    on E: Exception do
      raise EAlfredCreateFileException.Create('Erro ao criar o arquivo "'+Path+'". ' + E.Message);
  end;

end;

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
