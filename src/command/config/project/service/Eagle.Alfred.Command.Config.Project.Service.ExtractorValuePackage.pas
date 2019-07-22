unit Eagle.Alfred.Command.Config.Project.Service.ExtractorValuePackage;

interface

uses
  System.Rtti, SysUtils,

  XSuperObject,

  Spring.Reflection,

  Eagle.Alfred.Core.Types;

type
  TExtractorValuePackage = class
  public
    class function Extract(Package: TPackage; const PropertyName: string): string;
  end;

implementation

class function TExtractorValuePackage.Extract(Package: TPackage; const PropertyName: string): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  AliasAttribute: Alias;
begin
  RttiContext := TRttiContext.Create;

  RttiType := RttiContext.GetType(Package.ClassInfo);

  for Field in RttiType.GetFields do
  begin

    if not Field.TryGetCustomAttribute<Alias>(AliasAttribute) then
      continue;

    if not AliasAttribute.Name.Equals(PropertyName) then
      continue;

    if Field.GetValue(Package).IsObject then
      Exit(TJSON.Stringify(Field.GetValue(Package), True));

    Exit(Field.GetValue(Package).AsString);

  end;

  Result := '';

end;

end.
