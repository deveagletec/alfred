unit Eagle.Alfred.Attributes;

interface
uses
  System.SysUtils;

type
  CommandAttribute = class(TCustomAttribute)
  private
    FName: string;
    FAlias: string;
    FGroupName: string;
    FGroupAlias: string;
    FDescription: string;
  public
    constructor Create(const AGroup, AName, ADescription: string);
    property GroupName: string read FGroupName;
    property GroupAlias: string read FGroupAlias;
    property Name: string read FName;
    property Alias: string read FAlias;
    property Description: string read FDescription;
  end;

  OptionAttribute = class(TCustomAttribute)
  private
    FName: string;
    FAlias: string;
    FDescription: string;
  public
    constructor Create(const AName, AAlias, ADescription: string);
    property Name: string read FName;
    property Alias: string read FAlias;
    property Description: string read FDescription;
  end;

  ParamAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
    FIndex: Integer;
    FRequired: Boolean;
  public
    constructor Create(const AName, ADescription: string; const ARequired: Boolean = True); overload;
    constructor Create(const AIndex: Integer; const ADescription: string; const ARequired: Boolean = True); overload;
    property &Index: Integer read FIndex;
    property Name: string read FName;
    property Description: string read FDescription;
    property Required: Boolean read FRequired;
  end;

implementation

{ CommandAttribute }

constructor CommandAttribute.Create(const AGroup, AName, ADescription: string);
begin
  FGroupName := AGroup;
  FName := AName;
  FDescription := ADescription;
end;

{ OptionAttribute }

constructor OptionAttribute.Create(const AName, AAlias, ADescription: string);
begin
  FName := AName.Replace('-','', [rfReplaceAll]);
  FAlias := AAlias.Replace('-','', [rfReplaceAll]);
  FDescription := ADescription;
end;

{ ParamAttribute }

constructor ParamAttribute.Create(const AName, ADescription: string; const ARequired: Boolean);
begin
  FName := AName;
  FDescription := ADescription;
  FRequired := ARequired;
end;

constructor ParamAttribute.Create(const AIndex: Integer; const ADescription: string; const ARequired: Boolean);
begin
  FIndex := AIndex;
  FDescription := ADescription;
  FRequired := ARequired;
end;

end.
