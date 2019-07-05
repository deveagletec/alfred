unit Eagle.Alfred.Commom.Utils.GroupExtractorRegularExpression;

interface

uses
  System.RegularExpressions,
  Generics.Collections;

type

  TGroupExtractorRegularExpression = class
  public
    class function ExtractListGroups(const RegularExpression, Value: string; const GroupIndex: Integer): TList<string>;
  end;

implementation

class function TGroupExtractorRegularExpression.ExtractListGroups(const RegularExpression, Value: string; const GroupIndex: Integer): TList<string>;
const
  EMPTY = '';
var
  Extractor: TRegEx;
  QuantidadeGrupos: Integer;
  Matches: TMatchCollection;
  Index: Integer;
  ValueMatched: string;
  ValuesMatcheds: TList<string>;
begin

  ValuesMatcheds := TList<string>.Create();

  if GroupIndex < 0 then
    Exit(ValuesMatcheds);

  Extractor := TRegEx.Create(RegularExpression);

  Matches := Extractor.Matches(Value);

  for Index := 0 to Matches.Count - 1 do
  begin
    ValueMatched := Matches.Item[Index].Groups.Item[GroupIndex].Value;
    ValuesMatcheds.Add(ValueMatched);
  end;

  Result := ValuesMatcheds;

end;

end.
