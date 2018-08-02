unit Eagle.Alfred.Attributes;

interface

type
  CommandAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
  public
    constructor Create(const AName, ADescription: string);
    property Name: string read FName;
    property Description: string read FDescription;
  end;

  ActionAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
  public
    constructor Create(const AName, ADescription: string);
    property Name: string read FName;
    property Description: string read FDescription;
  end;

  OptionAttribute = class(TCustomAttribute)

  end;

  ParamAttribute = class(TCustomAttribute)

  end;

implementation

{ CommandAttribute }

constructor CommandAttribute.Create(const AName, ADescription: string);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ ActionAttribute }

constructor ActionAttribute.Create(const AName, ADescription: string);
begin
  FName := AName;
  FDescription := ADescription;
end;

end.
