unit Eagle.Alfred.MigrateService;

interface
uses
  System.Classes,
  System.DateUtils,
  System.IOUtils,
  System.SysUtils,
  Eagle.Alfred.Data;

type

  IMigrateService = interface
    ['{A31CC6DC-9FE4-4FC1-9610-E476806C6D33}']
    procedure Generate(const Name: string);
  end;

  TMigrateService = class(TInterfacedObject, IMigrateService)
  private
    FPackage: TPackage;
  public
    constructor Create(APackage: TPackage);
    procedure Generate(const Name: string);
  end;

implementation

{ TMigrateService }

constructor TMigrateService.Create(APackage: TPackage);
begin
  FPackage := APackage;
end;

procedure TMigrateService.Generate(const Name: string);
var
  FileName, TimeStamp: string;
  Migrate: TStringList;
begin

  TimeStamp := DateTimeToUnix(Now).ToString;

  FileName := TimeStamp + '_' + Name + '.sql';

  Migrate := TStringList.Create;

  try

    Migrate.Add('INSERT INTO MIGRATIONS (ID) VALUES (' + TimeStamp + ');');

    Migrate.SaveToFile(FileName);

  finally
    Migrate.Free;
  end;

end;

end.
