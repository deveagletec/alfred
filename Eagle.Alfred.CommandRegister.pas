unit Eagle.Alfred.CommandRegister;

interface
uses
  System.Generics.Collections,
  System.Rtti,
  Eagle.Alfred.Command;

type
  TMethodRegister = record
    Name: string;
    Description: string;
    Method: TRttiMethod;
  end;

  ICommandRegister = interface
    ['{6586858D-5708-44C2-9C32-20457EFCF003}']
    procedure AddMethod(const Name, Description: string; Method: TRttiMethod);
    function GetName: string;
    function GetDescription: string;
    function GetCommand: TObject;
    function GetAction(ActionName: string): TMethodRegister;
    function ContainsAction(ActionName: string): Boolean;
  end;

  TCommandRegister = class(TInterfacedObject, ICommandRegister)
  private
    FName: string;
    FDescription: string;
    FCommand: TObject;
    FActions: TDictionary<string, TMethodRegister>;
  public
    constructor Create(const AName, ADescription: string; ACommand: TObject);
    destructor Destroy; override;

    procedure AddMethod(const Name, Description: string; Method: TRttiMethod);
    function GetName: string;
    function GetDescription: string;
    function GetCommand: TObject;
    function GetAction(ActionName: string): TMethodRegister;
    function ContainsAction(ActionName: string): Boolean;
  end;

implementation

{ TCommandRegister }

procedure TCommandRegister.AddMethod(const Name, Description: string; Method: TRttiMethod);
var
  MethodRegister: TMethodRegister;
begin

  MethodRegister.Name := Name;
  MethodRegister.Description := Description;
  MethodRegister.Method := Method;

  FActions.Add(Name, MethodRegister);

end;

function TCommandRegister.ContainsAction(ActionName: string): Boolean;
begin
  Result := FActions.ContainsKey(ActionName);
end;

constructor TCommandRegister.Create(const AName, ADescription: string; ACommand: TObject);
begin
  FName := AName;
  FDescription := ADescription;
  FCommand := ACommand;
  FActions := TDictionary<string, TMethodRegister>.Create;
end;

destructor TCommandRegister.Destroy;
begin
  FActions.Free;
  FCommand.Free;
  inherited;
end;

function TCommandRegister.GetAction(ActionName: string): TMethodRegister;
begin
  Result := FActions.Items[ActionName];
end;

function TCommandRegister.GetCommand: TObject;
begin
  Result := FCommand;
end;

function TCommandRegister.GetDescription: string;
begin
  Result := FDescription;
end;

function TCommandRegister.GetName: string;
begin
  Result := FName;
end;

end.
