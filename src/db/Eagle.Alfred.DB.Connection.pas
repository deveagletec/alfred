unit Eagle.Alfred.DB.Connection;

interface

uses
  FireDAC.Comp.Client;

type
  IConnection = interface
    ['{031864A1-26C8-48DC-ADA0-DD0341BF8920}']

    function GetConnection: TFDConnection;
    procedure Initialize;
    procedure Refresh;

  end;

implementation

end.
