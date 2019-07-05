unit Eagle.Alfred.Command.Common.Service.ScaperJSON;

interface

uses
  SysUtils, Generics.Collections,

  Eagle.Alfred.Commom.Utils.GroupExtractorRegularExpression;

type
  TScaperJSON = class
  public
    class function Scape(const JSON: string): string;
  end;

implementation

class function TScaperJSON.Scape(const JSON: string): string;
const
  EXPRESSION_REGULAR = '(?:\''(?<identifier>[^\'']+)\s*(?<parameters>.*?)\'')';
  GROUP_INDEX = 1;
  CARACTER_TO_SCAPE = '"';
  CARACTER_SCAPED = '\"';
var
  JSONScaped: string;
  SentenceToScape, SentenceScaped: string;
  SentencesToScape: TList<string>;
begin

  SentencesToScape := TGroupExtractorRegularExpression.ExtractListGroups(EXPRESSION_REGULAR, JSON, GROUP_INDEX);

  try

    if SentencesToScape.Count <= 0 then
      Exit(JSON);

    JSONScaped := JSON;

    for SentenceToScape in SentencesToScape do
    begin

      if not SentenceToScape.Contains(CARACTER_TO_SCAPE) then
        continue;

      SentenceScaped := SentenceToScape.Replace(CARACTER_TO_SCAPE, CARACTER_SCAPED);

      JSONScaped := JSONScaped.Replace(SentenceToScape, SentenceScaped);

    end;


  finally
    SentencesToScape.Free();
  end;

  Result := JSONScaped;

end;

end.
