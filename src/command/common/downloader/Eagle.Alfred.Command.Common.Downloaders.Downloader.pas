unit Eagle.Alfred.Command.Common.Downloaders.Downloader;

interface
uses
  Classes, System.Zip, IdHTTP, System.SysUtils, System.IOUtils, IdComponent, IdSSLOpenSSL, Math,
  System.NetEncoding,
  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.ConsoleIO;

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
      FIdHTTP: TIdHTTP;

      procedure CreateDir(const RepoName: string);
      procedure UnZipDependency(const FileName : string);
      procedure DoDownloadDependency(Dependency : TDependency);
      function GetSourceDirName(const FileName : string) : string;
      procedure CopyDependency(Dependency : TDependency);
      procedure DeleteDownloadedFiles(const FileName : string);

      procedure OnDownloadBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure OnDownloadWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

      function MountUrl(Dependency : TDependency): string; virtual; abstract;
      procedure SetAuthentication(Dependency: TDependency); virtual; abstract;
   public
      constructor Create(aIO : IConsoleIO; const VendorDir : string);
      destructor Destroy; override;
      procedure DownloadDependency(Dependency : TDependency);
   end;

implementation

{ TDownloader }

procedure TDownloader.CopyDependency(Dependency : TDependency);
var
   SourceDirName, DestDirName : string;
begin
  SourceDirName := GetSourceDirName(Dependency.Project);

  DestDirName := FVendorDir + Dependency.Project;

  if TDirectory.Exists(DestDirName) then
    TDirectory.Delete(DestDirName, True);

  TDirectory.Move(SourceDirName, DestDirName);
end;

constructor TDownloader.Create(aIO : IConsoleIO; const VendorDir: string);
var
  ssl: TIdSSLIOHandlerSocketOpenSSL;
begin
  FIO := aIO;
  FVendorDir := VendorDir;

  FIdHTTP := TIdHTTP.Create;
  FIdHTTP.OnWorkBegin := OnDownloadBegin;
  FIdHTTP.OnWork := OnDownloadWork;
  FIdHTTP.Request.UserAgent :=  'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
  FIdHTTP.HandleRedirects := True;

  ssl := TIdSSLIOHandlerSocketOpenSSL.Create(FIdHTTP);

  ssl.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2, sslvSSLv23];
  ssl.SSLOptions.CipherList := 'ALL:!EXPORT:!LOW:!aNULL:!eNULL:!SSLv2';

  FIdHTTP.IOHandler := ssl;
end;

procedure TDownloader.CreateDir(const RepoName: string);
var
  Dir: string;
begin
  if RepoName.Contains('/') then
    Dir := RepoName.Split(['/'])[0];

  TDirectory.CreateDirectory(FVendorDir + Dir);
end;

procedure TDownloader.DeleteDownloadedFiles(const FileName: string);
begin
  TDirectory.Delete(FileName, True);
  TFile.Delete(FileName + '.zip');
end;

destructor TDownloader.Destroy;
begin
  if Assigned(FIdHTTP) then
    FIdHTTP.Free;
  inherited;
end;

procedure TDownloader.DoDownloadDependency(Dependency: TDependency);
var
  Stream: TMemoryStream;
  Url, FileName: String;
begin
  FIdHTTP.Request.RawHeaders.Clear;
  FIdHTTP.Request.BasicAuthentication := False;

  Url := MountUrl(Dependency);
  Filename := Dependency.Project + '.zip';

  SetAuthentication(Dependency);

  Stream := TMemoryStream.Create;

  try
    FIdHTTP.Get(Url, Stream);
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

procedure TDownloader.DownloadDependency(Dependency: TDependency);
begin
  try
    FIO.WriteProcess('Downloading => 0 Mb');
    DoDownloadDependency(Dependency);

    FIO.WriteInfo('');
    FIO.WriteProcess('Unzipping ...');
    UnZipDependency(Dependency.Project);
    FIO.WriteProcess('Unzipping... Done');

    FIO.WriteInfo('');
    FIO.WriteProcess('Copying...');
    CopyDependency(Dependency);
    FIO.WriteProcess('Copying... Done');

    FIO.WriteInfo('');
    FIO.WriteProcess('Cleaning swap...');
    DeleteDownloadedFiles(Dependency.Project);
    FIO.WriteProcess('Cleaning swap... Done');
  finally
    FIO.WriteInfo('');
  end;
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

procedure TDownloader.OnDownloadBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FDownloadSize := AWorkCountMax;

  if FDownloadSize < 1 then
    FDownloadSize := 1;
end;

procedure TDownloader.OnDownloadWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
  Progress : Double;
begin
  Progress := RoundTo((AWorkCount / 1024) / 1024, -3);

  FIO.WriteProcess('Downloading => ' + String.Parse(Progress) + ' Mb');
end;

procedure TDownloader.UnZipDependency(const FileName: string);
var
  Zipper: TZipFile;
begin
  Zipper := TZipFile.Create();

  if TDirectory.Exists(FileName) then
    TDirectory.Delete(FileName, True);

  try
    Zipper.Open(FileName + '.zip', zmRead);
    Zipper.ExtractAll(FileName);
    Zipper.Close;
  finally
    Zipper.Free;
  end;
end;

end.
