unit Eagle.Alfred.DB.Impl.FiredacConnection;

interface

uses
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

    FDDriverLink: TFDPhysFBDriverLink;
    FConnection: TFDConnection;

    procedure CreateConnection;

  public
    constructor Create(const HostName: string; const DataBase: string; const UserName: string; const Password: string; const Port: string);
    destructor Destroy; override;
    procedure Initialize;
    procedure Release;
    procedure Refresh;
    function GetConnection: TFDConnection;
  end;

implementation

{ TFireDacFirebirdConnection }

constructor TFireDacFirebirdConnection.Create(const HostName: string; const DataBase: string; const UserName: string; const Password: string; const Port: string);
begin
  inherited Create();

  FDatabase := DataBase;
  FHostName := HostName;
  FUserName := UserName;
  FPassword := Password;
  FPort := Port;

  Initialize;

end;

procedure TFireDacFirebirdConnection.CreateConnection;
begin

  FDDriverLink := TFDPhysFBDriverLink.Create(nil);
  FDDriverLink.VendorLib := 'fbclient.dll';
  FConnection := TFDConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.DriverName := 'FB';

  FConnection.Params.Add('Database=' + FDatabase);
  FConnection.Params.Add('Hostname=' + FHostName);
  FConnection.Params.Add('User_name=' + FUserName);
  FConnection.Params.Add('Password=' + FPassword);
  FConnection.Params.Add('Port=' + FPort);

end;

destructor TFireDacFirebirdConnection.Destroy;
begin
  Release;
  inherited;
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
