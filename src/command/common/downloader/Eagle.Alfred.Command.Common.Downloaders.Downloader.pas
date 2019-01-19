unit Eagle.Alfred.Command.Common.Downloaders.Downloader;

interface
uses Classes, System.Zip, IdHTTP, System.SysUtils, System.IOUtils, IdComponent, IdSSLOpenSSL,
   Eagle.Alfred.Core.Types, Eagle.Alfred.Core.ConsoleIO;

type

   IDownloader = interface
      ['{D736B7F1-65D7-4125-992E-2ECC12420525}']
      procedure DownloadDependency(Dependency : TDependency);
   end;

   TDownloader = class(TInterfacedObject, IDownloader)
   protected
      FIO : IConsoleIO;
      FVendorDir : string;
      FDownloadSize : Int64;

      procedure UnZipDependency(const FileName : string);
      procedure DoDownloadDependency(Dependency : TDependency);
      function GetSourceDirName(const FileName : string) : string;
      procedure CopyDependency(const FileName : string);
      procedure DeleteDownloadedFiles(const FileName : string);

      procedure OnDownloadBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure OnDownloadWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
   public
      constructor Create(aIO : IConsoleIO; const VendorDir : string);
      procedure DownloadDependency(Dependency : TDependency);
      function GetUrlDependency(Dependency : TDependency) : string; Virtual; Abstract;
   end;

implementation

{ TDownloader }

procedure TDownloader.CopyDependency(const FileName: string);
var
   SourceDirName, DestDirName : string;
begin

   SourceDirName := GetSourceDirName(FileName);

   DestDirName := FVendorDir + FileName;

   if TDirectory.Exists(DestDirName) then
      TDirectory.Delete(DestDirName, True);

   TDirectory.Move(SourceDirName, DestDirName);

end;

constructor TDownloader.Create(aIO : IConsoleIO; const VendorDir: string);
begin
   FIO := aIO;
   FVendorDir := VendorDir;
end;

procedure TDownloader.DeleteDownloadedFiles(const FileName: string);
begin
   TDirectory.Delete(FileName, True);
   TFile.Delete(FileName + '.zip');
end;

procedure TDownloader.DoDownloadDependency(Dependency: TDependency);
var
  IdHTTP1: TIdHTTP;
  Stream: TMemoryStream;
  Url, FileName: String;
  ssl: TIdSSLIOHandlerSocketOpenSSL;
begin

  Url := GetUrlDependency(Dependency);
  Filename := Dependency.Id + '.zip';

  IdHTTP1 := TIdHTTP.Create;
  IdHTTP1.OnWorkBegin := OnDownloadBegin;
  IdHTTP1.OnWork := OnDownloadWork;
  IdHTTP1.Request.UserAgent :=  'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
  IdHTTP1.HandleRedirects := True;

  ssl := TIdSSLIOHandlerSocketOpenSSL.Create(IdHTTP1);

  ssl.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2, sslvSSLv23];
  ssl.SSLOptions.CipherList := 'ALL:!EXPORT:!LOW:!aNULL:!eNULL:!SSLv2';

  IdHTTP1.IOHandler := ssl;

  Stream := TMemoryStream.Create;
  try
    IdHTTP1.Get(Url, Stream);
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
    IdHTTP1.Free;
  end;

end;

procedure TDownloader.DownloadDependency(Dependency: TDependency);
begin

   FIO.WriteProcess('Downloading => 0%');
   DoDownloadDependency(Dependency);

   FIO.WriteInfo('');
   FIO.WriteInfo('Unzipping ...');
   UnZipDependency(Dependency.Id);

   FIO.WriteInfo('Copying ...');
   CopyDependency(Dependency.Id);

   FIO.WriteInfo('Cleaning swap ...');
   DeleteDownloadedFiles(Dependency.Id);

end;

function TDownloader.GetSourceDirName(const FileName: string): string;
var
   Dir : string;
   searchResult : TSearchRec;
begin

  if FindFirst(FileName + '\*', faDirectory, searchResult) = 0 then
  begin

    repeat
      Dir := searchResult.Name;

      if Dir.Equals('.') or Dir.Equals('..') then
         continue;

      Result := FileName + '\' + Dir;

    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;

end;

procedure TDownloader.OnDownloadBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
   FDownloadSize := AWorkCountMax;

   if FDownloadSize < 1 then
      FDownloadSize := 1;

end;

procedure TDownloader.OnDownloadWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
   Progress : Integer;
begin

   Progress := Round((AWorkCount / FDownloadSize) * 100);

   FIO.WriteProcess('Downloading => ' + String.Parse(Progress) + '%');

end;

procedure TDownloader.UnZipDependency(const FileName: string);
var
   Zipper: TZipFile;
begin

  Zipper := TZipFile.Create();

  try
    Zipper.Open(FileName + '.zip', zmRead);
    Zipper.ExtractAll(FileName);
    Zipper.Close;
  finally
    Zipper.Free;
  end;


end;

end.
