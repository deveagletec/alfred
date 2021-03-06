unit Eagle.Alfred.Command.DB.Common.FiredacConnection;

interface

uses
  SysUtils,
  System.StrUtils,
  Data.DB,

  FireDAC.Phys.FBDef,
  FireDAC.UI.Intf,
  FireDAC.Comp.UI,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Phys,
  FireDAC.Phys.IBBase,
  FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait,

  Eagle.Alfred.Core.Types,
  Eagle.Alfred.Command.DB.Common.Connection,
  Eagle.Alfred.Command.DB.Common.Driver;

type
  TFireDacFirebirdConnection = class(TInterfacedObject, IConnection)
    private

      FDatabase: string;
      FHostName: string;
      FUserName: string;
      FPassword: string;
      FPort: string;
      FCharacterSet: string;
      FDriver: TDriver;

      FDDriverLink: TFDPhysDriverLink;
      FConnection: TFDConnection;

      procedure CreateConnection;

    public
      constructor Create(const DataBaseConfig: TDataBase);
      destructor Destroy; override;
      procedure Initialize;
      procedure Release;
      procedure Refresh;
      function GetConnection: TFDConnection;
  end;

implementation

constructor TFireDacFirebirdConnection.Create(const DataBaseConfig: TDataBase);
begin
  inherited Create();

  FDatabase := DataBaseConfig.&File;
  FHostName := DataBaseConfig.Host;
  FUserName := DataBaseConfig.User;
  FPassword := DataBaseConfig.Pass;
  FPort := DataBaseConfig.Port.ToString;
  FCharacterSet := DataBaseConfig.CharacterSet;
  FDriver := TDriver.construirDriver(DataBaseConfig.Driver);

  Initialize;

end;

procedure TFireDacFirebirdConnection.CreateConnection;
begin
  FDDriverLink := FDriver.GetDriverLink();
  FDDriverLink.VendorLib := FDriver.GetDLL();

  FConnection := TFDConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.DriverName := FDriver.getName();

  FConnection.Params.Add('Database=' + FDatabase);
  FConnection.Params.Add('Hostname=' + FHostName);
  FConnection.Params.Add('User_name=' + FUserName);
  FConnection.Params.Add('Password=' + FPassword);
  FConnection.Params.Add('Port=' + FPort);

  if not FCharacterSet.IsEmpty then
    FConnection.Params.Add('CharacterSet=' + FCharacterSet);

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
