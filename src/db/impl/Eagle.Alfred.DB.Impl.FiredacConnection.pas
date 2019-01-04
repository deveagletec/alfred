unit Eagle.Alfred.DB.Impl.FiredacConnection;

interface

uses
  System.Win.Registry,
  Winapi.Windows,

  IniFiles,
  SysUtils,
  Data.DB,

  FireDAC.Phys.FBDef,
  FireDAC.UI.Intf,
  FireDAC.Comp.UI,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Phys,
  FireDAC.Phys.IBBase,
  FireDAC.Comp.Client,
  FireDAC.Phys.FB,
  FireDAC.VCLUI.Wait,

  Eagle.Alfred.DB.Connection;

type
  TFireDacFirebirdConnection = class(TInterfacedObject, IConnection)
  private
    FDatabase: string;
    FHostName: string;
    FUserName: string;
    FPassword: string;
    FPort: string;

    FDDriverLink:TFDPhysFBDriverLink;
    FConnection:TFDConnection;

    FRegistry: TRegistry;

    procedure CreateConnection;
    procedure getConfigRegister;
    function readRegKey(const KeyName: String): String;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure Release;
    procedure Refresh;
    function  GetConnection:TFDConnection;
  end;

implementation

{ TFireDacFirebirdConnection }

constructor TFireDacFirebirdConnection.Create;
begin
  inherited;

  getConfigRegister;
  Initialize;
end;

procedure TFireDacFirebirdConnection.CreateConnection;
begin
  FDDriverLink := TFDPhysFBDriverLink.Create(nil);
  FDDriverLink.VendorLib := 'fbclient.dll';
  FConnection := TFDConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.DriverName := 'FB';

  FConnection.Params.Add('Database='+FDatabase);
  FConnection.Params.Add('Hostname='+FHostName);
  FConnection.Params.Add('User_name='+FUserName);
  FConnection.Params.Add('Password='+FPassword);
  FConnection.Params.Add('Port='+FPort);

end;

destructor TFireDacFirebirdConnection.Destroy;
begin
  Release;
  inherited;
end;

procedure TFireDacFirebirdConnection.getConfigRegister;
begin

  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_LOCAL_MACHINE;

  FHostName := readRegKey('dbHostName');
  FDatabase := readRegKey('dbFileName');
  FUserName := readRegKey('dbUserName');
  FPassword := readRegKey('dbPassword');
  FPort     := '3050';

end;

function TFireDacFirebirdConnection.GetConnection: TFDConnection;
begin
  Result := FConnection;
end;

procedure TFireDacFirebirdConnection.Initialize;
begin
  if FConnection = nil then
     CreateConnection;
end;

function TFireDacFirebirdConnection.readRegKey(const KeyName: String): String;
var
  hkey: string;
const
  HKEY_ROOT = 'SOFTWARE\Eagle\EagleERP';
begin

  hkey := '\' + HKEY_ROOT;

  if FRegistry.OpenKey(hkey, false) then
  begin

    try
      result := FRegistry.ReadString(keyName);
    except

      raise Exception.Create('Erro ao ler a chave no registro: ' + QuotedStr(hkey));
    end;

  end
  else
    raise Exception.Create('Erro ao ler a chave no registro: ' +  QuotedStr(hkey));

end;

procedure TFireDacFirebirdConnection.Refresh;
begin
   Release;
   Initialize;
end;

procedure TFireDacFirebirdConnection.Release;
begin
  FreeAndNil(FConnection);
  FreeAndNil(FDDriverLink);
end;

end.
