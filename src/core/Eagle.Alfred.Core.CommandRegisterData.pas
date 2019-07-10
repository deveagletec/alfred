unit Eagle.Alfred.Core.CommandRegisterData;

interface

uses
  System.Rtti,

  Eagle.Alfred.Core.Attributes;

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

implementation

end.
