unit Eagle.Alfred.Command.Common.Downloaders.Downloader;

interface
uses
  Classes, IdHTTP, System.SysUtils, System.IOUtils, IdComponent, IdSSLOpenSSL, Math,
  System.NetEncoding,

  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Core.Exceptions;

type

  TProgressNotify = reference to procedure(const Value: Double);

  IDownloader = interface
    ['{D736B7F1-65D7-4125-992E-2ECC12420525}']
    procedure DownloadDependency(Dependency : TDependency; ProgressNotify: TProgressNotify);
  end;

  TDownloader = class(TInterfacedObject, IDownloader)
  private
    FOnProgressNotify: TProgressNotify;
    procedure DoDownloadDependency(Dependency: TDependency);
    procedure HandleHttpError(E: EIdHTTPProtocolException; Stream: TMemoryStream);
  protected
    FDownloadSize : Int64;
    FIdHTTP: TIdHTTP;

    procedure OnDownloadBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnDownloadWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);

    function MountUrl(Dependency : TDependency): string; virtual; abstract;
    procedure SetAuthentication(Dependency: TDependency); virtual; abstract;
  public
    constructor Create(const VendorDir: string);
    destructor Destroy; override;
    procedure DownloadDependency(Dependency : TDependency; ProgressNotify: TProgressNotify);
  end;

implementation

{ TDownloader }

constructor TDownloader.Create(const VendorDir: string);
var
  ssl: TIdSSLIOHandlerSocketOpenSSL;
begin
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

destructor TDownloader.Destroy;
begin
  if Assigned(FIdHTTP) then
    FIdHTTP.Free;
  inherited;
end;

procedure TDownloader.DoDownloadDependency(Dependency: TDependency);
var
  Stream: TMemoryStream;
  Url, FileName: string;
begin
  FIdHTTP.Request.RawHeaders.Clear;
  FIdHTTP.Request.CustomHeaders.Clear;
  FIdHTTP.Request.BasicAuthentication := False;

  Url := MountUrl(Dependency);
  Filename := Dependency.Project + '.zip';

  SetAuthentication(Dependency);

  Stream := TMemoryStream.Create;

  try
    try
      FIdHTTP.Get(Url, Stream);
      Stream.SaveToFile(FileName);
    except
      on E: EIdHTTPProtocolException do
        HandleHttpError(E, Stream);
      on E: Exception do
        raise EDownloadException.Create(E.Message);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TDownloader.DownloadDependency(Dependency : TDependency; ProgressNotify: TProgressNotify);
begin
  FOnProgressNotify := ProgressNotify;

  DoDownloadDependency(Dependency);
end;

procedure TDownloader.HandleHttpError(E: EIdHTTPProtocolException; Stream: TMemoryStream);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  StringStream.CopyFrom(Stream, Stream.Size);
 // FIdHTTP.Response.
  raise EDownloadException.Create(E.Message + StringStream.DataString);
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

  if Assigned(FOnProgressNotify) then
    FOnProgressNotify(Progress);
end;

end.
