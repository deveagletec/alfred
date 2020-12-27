unit Eagle.Alfred.Command.DB.Common.Driver;

interface

uses
  System.StrUtils,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.FB;

type

  TDriver = (FIREBIRD, POSTGRES);

  TDriverHelper = record helper for TDriver
    function GetName(): string;
    function GetDLL(): string;
    function GetDriverLink(): TFDPhysDriverLink;
    class function ConstruirDriver(const Value: string): TDriver; static; inline;
  end;

implementation

function TDriverHelper.GetName(): string;
const
  NOMES: TArray<string> = ['FB', 'PG'];
begin
  Result := NOMES[Integer(self)];
end;

function TDriverHelper.GetDLL(): string;
const
  DLLs: TArray<string> = ['fbclient.dll', 'libpq.dll'];
begin
  Result := DLLs[Integer(self)];
end;

class function TDriverHelper.ConstruirDriver(const Value: String): TDriver;
begin
  result := TDriver(AnsiIndexStr(Value, [FIREBIRD.GetName(), POSTGRES.GetName()]));
end;

function TDriverHelper.GetDriverLink(): TFDPhysDriverLink;
begin
  case self of
    FIREBIRD:
      Result := TFDPhysFBDriverLink.Create(nil);
    POSTGRES:
      Result := TFDPhysPGDriverLink.Create(nil);
  end;
end;

end.
