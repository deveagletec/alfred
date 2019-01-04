unit Eagle.Alfred.Migrate.Model.Migrate;

interface

type
  IMigrate = interface
    ['{2522F6D7-976F-4D67-8B7B-67D5399877F3}']

    function getDescription: String;
    function getDown: String;
    function getIssueIdentifier: String;
    function getResponsible: String;
    function getUnixIdentifier: String;
    function getVersion: String;
    procedure setIssueIdentifier(const Value: String);
    procedure setResponsible(const Value: String);
    function getUp: String;
    procedure setDescription(const Value: String);
    procedure setDown(const Value: String);
    procedure setUnixIdentifier(const Value: String);
    procedure setUp(const Value: String);
    procedure setVersion(const Value: String);

    property IssueIdentifier: String read getIssueIdentifier write setIssueIdentifier;
    property UnixIdentifier: String read getUnixIdentifier write setUnixIdentifier;
    property Version: String read getVersion write setVersion;
    property Description: String read getDescription write setDescription;
    property Responsible: String read getResponsible write setResponsible;
    property Up: String read getUp write setUp;
    property Down: String read getDown write setDown;

  end;

type
  TMigrate = class(TInterfacedObject, IMigrate)
  private

    FIssueIdentifier: String;
    FUnixIdentifier: String;
    FVersion: String;
    FDescription: String;
    FResponsible: String;
    FUp: String;
    FDown: String;

    function getDescription: String;
    function getDown: String;
    function getIssueIdentifier: String;
    function getResponsible: String;
    function getUnixIdentifier: String;
    function getVersion: String;
    function getUp: String;
    procedure setDescription(const Value: String);
    procedure setDown(const Value: String);
    procedure setIssueIdentifier(const Value: String);
    procedure setResponsible(const Value: String);
    procedure setUnixIdentifier(const Value: String);
    procedure setUp(const Value: String);
    procedure setVersion(const Value: String);

  public

    property IssueIdentifier: String read getIssueIdentifier write setIssueIdentifier;
    property UnixIdentifier: String read getUnixIdentifier write setUnixIdentifier;
    property Version: String read getVersion write setVersion;
    property Description: String read getDescription write setDescription;
    property Responsible: String read getResponsible write setResponsible;
    property Up: String read getUp write setUp;
    property Down: String read getDown write setDown;

  end;

implementation

function TMigrate.getDescription: String;
begin
  Result := FDescription;
end;

function TMigrate.getDown: String;
begin
  Result := FDown;
end;

function TMigrate.getIssueIdentifier: String;
begin
  Result := FIssueIdentifier;
end;

function TMigrate.getResponsible: String;
begin
  Result := FResponsible;
end;

function TMigrate.getUnixIdentifier: String;
begin
  Result := FUnixIdentifier;
end;

function TMigrate.getUp: String;
begin
  Result := FUp;
end;

function TMigrate.getVersion: String;
begin
  Result := FVersion;
end;

procedure TMigrate.setDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TMigrate.setDown(const Value: String);
begin
  FDown := Value;
end;

procedure TMigrate.setIssueIdentifier(const Value: String);
begin
  FIssueIdentifier := Value;
end;

procedure TMigrate.setResponsible(const Value: String);
begin
  FResponsible := Value;
end;

procedure TMigrate.setUnixIdentifier(const Value: String);
begin
  FUnixIdentifier := Value;
end;

procedure TMigrate.setUp(const Value: String);
begin
  FUp := Value;
end;

procedure TMigrate.setVersion(const Value: String);
begin
  FVersion := Value;
end;

end.
