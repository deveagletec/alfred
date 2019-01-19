unit Eagle.Alfred.Core.CommandRegister;

interface
uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,

  Spring.Reflection,

  Eagle.Alfred.Core.Exceptions,
  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.Command;

type

  TCommandParam = record
    Method: TRttiMethod;
    Attrib: ParamAttribute;
  end;

  TCommandOption = record
    Method: TRttiMethod;
    Attrib: OptionAttribute;
  end;

  TCommandMetaData = record
    PackageRequired: Boolean;
    CommandAttrib: CommandAttribute;
    CommandClass: TClass;
    CommandType: TRttiType;
    CommandParams: TArray<TCommandParam>;
    CommandOptions: TArray<TCommandOption>;
  end;

  ICommandRegister = interface
    ['{B7642446-B1A6-474A-A062-C4D848A8DAEF}']
    procedure AddCommand(CommandClass: TClass);
    function GetCommand(const GroupName, CommandName: string): TCommandMetaData;
    function Contains(const GroupName, CommandName: string): Boolean;
    function GetGroupsCommand: TArray<string>;
    function GetGroup(const GroupName: string): TDictionary<string, TCommandMetaData>;
  end;

  TCommandRegister = class(TInterfacedObject, ICommandRegister)
  private
    FCommands: TDictionary<string, TDictionary<string, TCommandMetaData>>;

    function GetCommandOption(CommandOptionAttrib: OptionAttribute; Method: TRttiMethod): TCommandOption;
    function GetCommandParam(CommandParamAttrib: ParamAttribute; Method: TRttiMethod): TCommandParam;
  public
    constructor Create();
    destructor Destroy; override;
    procedure AddCommand(CommandClass: TClass);
    function GetCommand(const GroupName, CommandName: string): TCommandMetaData;
    function Contains(const GroupName, CommandName: string): Boolean;
    function GetGroupsCommand: TArray<string>;
    function GetGroup(const GroupName: string): TDictionary<string, TCommandMetaData>;
  end;

implementation

{ TCommandRegister }

procedure TCommandRegister.AddCommand(CommandClass: TClass);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  RttiMethods: TArray<TRttiMethod>;
  CmdAttrib: CommandAttribute;
  CommandParamAttrib: ParamAttribute;
  CommandOptionAttrib: OptionAttribute;
  CommandsGroup: TDictionary<string, TCommandMetaData>;
  CommandMetaData: TCommandMetaData;
  CommandParams: TList<TCommandParam>;
  CommandOptions: TList<TCommandOption>;
begin

  RttiContext := TRttiContext.Create;

  CommandParams := TList<TCommandParam>.Create;
  CommandOptions := TList<TCommandOption>.Create;

  try

    RttiType := RttiContext.GetType(CommandClass.ClassInfo);

    if not RttiType.TryGetCustomAttribute<CommandAttribute>(CmdAttrib) then
      raise Exception.Create('CommandAttribute Not Found');

    CommandMetaData.PackageRequired := RttiType.HasCustomAttribute(PackageRequiredAttribute, True);

    if not FCommands.ContainsKey(CmdAttrib.GroupName) then
      FCommands.Add(CmdAttrib.GroupName, TDictionary<string, TCommandMetaData>.Create());

    CommandsGroup := FCommands.Items[CmdAttrib.GroupName];

    CommandMetaData.CommandAttrib := CmdAttrib;
    CommandMetaData.CommandClass := CommandClass;
    CommandMetaData.CommandType := RttiType;

    RttiMethods := RttiType.GetMethods;

    for Method in RttiMethods do
    begin

      if Method.TryGetCustomAttribute<ParamAttribute>(CommandParamAttrib) then
      begin
        CommandParams.Add(GetCommandParam(CommandParamAttrib, Method));
        Continue;
      end;

      if Method.TryGetCustomAttribute<OptionAttribute>(CommandOptionAttrib) then
        CommandOptions.Add(GetCommandOption(CommandOptionAttrib, Method));
    end;

    CommandMetaData.CommandParams := CommandParams.ToArray;
    CommandMetaData.CommandOptions := CommandOptions.ToArray;

    CommandsGroup.Add(CmdAttrib.Name, CommandMetaData);
  finally
    RttiContext.Free;
    CommandParams.Free;
    CommandOptions.Free;
  end;

end;

function TCommandRegister.Contains(const GroupName, CommandName: string): Boolean;
begin
  Result := FCommands.ContainsKey(GroupName);
end;

constructor TCommandRegister.Create;
begin
  FCommands := TDictionary<string, TDictionary<string, TCommandMetaData>>.Create;
end;

destructor TCommandRegister.Destroy;
var
  Command: TDictionary<string, TCommandMetaData>;
begin

  if Assigned(FCommands) then
  begin
    for Command in FCommands.Values do
      Command.Free;

     FreeAndNil(FCommands);
  end;

  inherited;
end;

function TCommandRegister.GetCommand(const GroupName, CommandName: string): TCommandMetaData;
var
  CommandGroup: TDictionary<string, TCommandMetaData>;
begin

  if not FCommands.ContainsKey(GroupName) then
    raise ECommandGroupNotFoundException.Create('Command Group not found');

  CommandGroup := FCommands.Items[GroupName];

  if not CommandGroup.ContainsKey(CommandName) then
    raise ECommandNotFound.Create('Command not found');

  Result := CommandGroup.Items[CommandName];
end;

function TCommandRegister.GetCommandOption(CommandOptionAttrib: OptionAttribute; Method: TRttiMethod): TCommandOption;
var
  CommandOption: TCommandOption;
begin
  CommandOption.Attrib := CommandOptionAttrib;
  CommandOption.Method := Method;

  Result := CommandOption;
end;

function TCommandRegister.GetCommandParam(CommandParamAttrib: ParamAttribute; Method: TRttiMethod): TCommandParam;
var
  CommandParam: TCommandParam;
begin
  CommandParam.Attrib := CommandParamAttrib;
  CommandParam.Method := Method;

  Result := CommandParam;
end;

function TCommandRegister.GetGroup(const GroupName: string): TDictionary<string, TCommandMetaData>;
begin
  Result := FCommands.Items[GroupName];
end;

function TCommandRegister.GetGroupsCommand: TArray<string>;
begin
  Result := FCommands.Keys.ToArray;
end;

end.
