unit Eagle.Alfred.Command.Common.HelpBuilder;

interface

uses
  SysUtils, StrUtils,
  Generics.Collections,

  Eagle.Alfred.Core.Attributes,
  Eagle.Alfred.Core.CommandRegisterData,

  Eagle.Alfred.Core.ConsoleIO;

type
  IHelpBuilder = interface
    procedure ShowCommand(const Command: TCommandMetaData);
    procedure ShowSimpleList(const Items: TArray<string>; const ListName, SingularListName: string);
    procedure ShowCommandsOfGroup(const GroupName: string; const Commands: TDictionary<string, TCommandMetaData>);
  end;

  THelpBuilder = class(TInterfacedObject, IHelpBuilder)
  private
    FConsoleIO: IConsoleIO;
    procedure ShowCommandOptions(const Options: TArray<TCommandOption>);
    procedure ShowCommandParams(const Params: TArray<TCommandParam>);
    procedure ShowFooter(const DescriptionHelp: string);
    procedure ShowHeader(const DescriptionUsage, Title: string);
  public

    constructor Create;

    procedure ShowCommand(const Command: TCommandMetaData);
    procedure ShowCommandsOfGroup(const GroupName: string; const Commands: TDictionary<string, TCommandMetaData>);
    procedure ShowSimpleList(const Items: TArray<string>; const ListName, SingularListName: string);

  end;

const
  SPACE = ' ';

implementation

constructor THelpBuilder.Create;
begin
  FConsoleIO := TConsoleIO.Create;
end;

procedure THelpBuilder.ShowCommand(const Command: TCommandMetaData);
var
  DescriptionHeader, CommandName: string;
  HasParams, HasOptions: Boolean;
begin

  DescriptionHeader := Format('%s %s <params> <options>', [Command.CommandAttrib.GroupName.ToLower, Command.CommandAttrib.Name.ToLower]);
  ShowHeader(DescriptionHeader, Format('COMMAND: %s', [Command.CommandAttrib.Name.ToUpper]));

  CommandName := Command.CommandAttrib.Name.ToUpper.PadRight(20, ' ');
  FConsoleIO.WriteInfo(Format('| %s %s', [CommandName, Command.CommandAttrib.Description.Trim]));
  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');

  HasParams := Length(Command.CommandParams) > 0;
  HasOptions := Length(Command.CommandOptions) > 0;

  if HasParams then
    ShowCommandParams(Command.CommandParams);

  if HasOptions then
    ShowCommandOptions(Command.CommandOptions);

end;

procedure THelpBuilder.ShowCommandOptions(const Options: TArray<TCommandOption>);
const
  LENGTH_OPTION_NAME = 25;
  LENGTH_OPTION_ALIAS = 8;
  LENGTH_OPTION_DESCRIPTION = 38;
var
  CommandOption: TCommandOption;
  OptionName, OptionAlias, OptionDescription: string;
begin

  FConsoleIO.NewEmptyLine;
  FConsoleIO.NewEmptyLine;

  FConsoleIO.WriteAlert('* -----------------');
  FConsoleIO.WriteAlert('| OPTIONS');
  FConsoleIO.WriteAlert('* -----------------------------------------------------------------------------');
  FConsoleIO.WriteAlert('| NAME                     ALIAS     DESCRIPTION');
  FConsoleIO.WriteAlert('* -----------------------------------------------------------------------------');

  for CommandOption in Options do
  begin
    OptionName := '--' + CommandOption.Attrib.Name.PadRight(LENGTH_OPTION_NAME, SPACE);
    OptionAlias := CommandOption.Attrib.Alias.PadRight(LENGTH_OPTION_ALIAS, SPACE);
    OptionDescription := CommandOption.Attrib.Description.PadRight(LENGTH_OPTION_DESCRIPTION, SPACE);
    FConsoleIO.WriteInfo(Format('| %s -%s%s', [OptionName, OptionAlias, OptionDescription]));
  end;

  FConsoleIO.WriteInfo('* ----------------------------------------------------------------------------- ');
end;

procedure THelpBuilder.ShowCommandParams(const Params: TArray<TCommandParam>);
const
  LENGTH_PARAMETER_NAME = 21;
  LENGTH_PARAMETER_TYPE = 9;
  LENGTH_PARAMTER_DESCRIPTION = 29;
  FIRST = 0;
var
  CommandParam: TCommandParam;
  Parameter: ParamAttribute;
  ParameterName, ParameterType, ParameterDescription, ParameterPosition, ParameterIsRequired: string;
begin

  FConsoleIO.NewEmptyLine;
  FConsoleIO.NewEmptyLine;

  FConsoleIO.WriteAlert('* -----------------');
  FConsoleIO.WriteAlert('| PARAMS');
  FConsoleIO.WriteAlert('* -----------------------------------------------------------------------------');
  FConsoleIO.WriteAlert('| NAME                 TYPE     DESCRIPTION                  POSITION  REQUIRED');
  FConsoleIO.WriteAlert('* -----------------------------------------------------------------------------');

  for CommandParam in Params do
  begin
    Parameter := CommandParam.Attrib;
    ParameterName := '';

    if not Parameter.Name.Trim.IsEmpty then
      ParameterName := Format('%s=<value>', [Parameter.Name.ToLower]);

    ParameterName := ParameterName.PadRight(LENGTH_PARAMETER_NAME, SPACE);

    ParameterType := CommandParam.Method.GetParameters[FIRST].ParamType.Name.PadRight(LENGTH_PARAMETER_TYPE, SPACE);
    ParameterDescription := Parameter.Description.PadRight(LENGTH_PARAMTER_DESCRIPTION, SPACE);
    ParameterPosition := IfThen(Parameter.Index > 0, Parameter.Index.ToString(), '');
    ParameterIsRequired := IfThen(Parameter.Required, 'X', '');

    FConsoleIO.WriteInfo(Format('| %s%s%s    %s        %s', [ParameterName, ParameterType, ParameterDescription, ParameterPosition, ParameterIsRequired]));

  end;

  FConsoleIO.WriteInfo('* -----------------------------------------------------------------------------');

end;

procedure THelpBuilder.ShowCommandsOfGroup(const GroupName: string; const Commands: TDictionary<string, TCommandMetaData>);
var
  Command: TCommandMetaData;
  CommandName, DescriptionHeaderFooter: string;
begin

  DescriptionHeaderFooter := Format('%s <command>', [GroupName.ToLower]);

  ShowHeader(DescriptionHeaderFooter, 'COMMANDS');

  for Command in Commands.Values do
  begin
    CommandName := Command.CommandAttrib.Name.PadRight(20, SPACE);
    FConsoleIO.WriteInfo(Format('| %s %s', [CommandName, Command.CommandAttrib.Description.Trim]));
  end;

  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');

  DescriptionHeaderFooter := Format('%s <command>', [GroupName.ToLower]);
  ShowFooter(DescriptionHeaderFooter);

end;

procedure THelpBuilder.ShowFooter(const DescriptionHelp: string);
begin
  FConsoleIO.NewEmptyLine();
  FConsoleIO.WriteInfo(Format('alfred %s [-h | --help] to show helper ', [DescriptionHelp.ToLower]));
end;

procedure THelpBuilder.ShowHeader(const DescriptionUsage, Title: string);
begin
  FConsoleIO.NewEmptyLine();
  FConsoleIO.WriteAlert(Format('| Usage: alfred %s', [DescriptionUsage.ToLower]));
  FConsoleIO.WriteAlert('* -----------------------------------------------------');
  FConsoleIO.WriteAlert(Format('| %s', [Title.ToUpper]));
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');
end;

procedure THelpBuilder.ShowSimpleList(const Items: TArray<string>; const ListName, SingularListName: string);
var
  Value: string;
  OrderedItems: TArray<string>;
begin

  FConsoleIO.NewEmptyLine();
  FConsoleIO.WriteAlert(Format('| Usage: alfred <%s>', [SingularListName.ToLower]));
  FConsoleIO.WriteAlert('* -----------------------------------------------------');
  FConsoleIO.WriteAlert(Format('| %s:', [ListName.ToUpper]));
  FConsoleIO.WriteAlert('* ----------------------------------------------------- ');

  OrderedItems := Items;
  TArray.Sort<string>(OrderedItems);

  for Value in OrderedItems do
    FConsoleIO.WriteInfo(Format('| %s', [Value]));

  FConsoleIO.WriteInfo('* ----------------------------------------------------- ');

  FConsoleIO.NewEmptyLine();
  FConsoleIO.NewEmptyLine();

  FConsoleIO.WriteInfo(Format('alfred <%s> [-h | --help]  to show helper', [SingularListName]));

end;

end.
