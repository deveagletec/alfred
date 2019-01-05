unit Eagle.Alfred.Core.IOUtils;

interface
uses
  System.SysUtils,
  System.IOUtils,

  Eagle.Alfred.Exceptions;

type
  TIOUtils = class
  public
    class procedure CreateDir(const Path: string; const Force: Boolean = False);
    class procedure CreateFile(const Path, Contents: string; const Force: Boolean = False);
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

end.
