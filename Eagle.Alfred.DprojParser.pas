unit Eagle.Alfred.DprojParser;

interface
uses
  System.SysUtils,System.Win.ComObj, XML.adomxmldom, ActiveX, XMLDoc, MSXML, MSXMLDOM, XMLIntf,
  System.IOUtils, System.Classes, System.Generics.Collections,

  Eagle.Alfred.Exceptions;

type

  IDprojParser = interface
    ['{995EE77F-1CD3-45A1-9F77-C718A86293D5}']
    procedure AddForm(const UnitName, FormName, Path: string);
    procedure AddUnit(const Name, Path: string);
    procedure Save;
  end;

  TDprojParser = class(TInterfacedObject, IDprojParser)
    private
      FXMLDocument: IXMLDomDocument;
      FPackagePath: string;
      FProjectName: string;
      FDprojFile: string;
      FDprFile: string;
      FVersionString: string;
      FUnitsList: TList<string>;
      FChanged: Boolean;
      procedure SetVersionString(const Value: string);
      procedure UpdateDpr;
    public
      procedure ChangeVersion;
      constructor Create(const PackagePath, ProjectName: string);
      destructor Destroy; override;
      procedure AddForm(const UnitName, FormName, Path: string);
      procedure AddUnit(const Name, Path: string);
      procedure Save;

      property VersionString: string read FVersionString write SetVersionString;
  end;

implementation

procedure TDprojParser.AddForm(const UnitName, FormName, Path: string);
var
  ItemGroup, NodeBase, Node: IXMLDOMNode;
begin

  ItemGroup := FXMLDocument.selectSingleNode('/Project/ItemGroup');

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/DCCReference/Form').firstChild;

  Node := NodeBase.parentNode.parentNode.cloneNode(True);

  Node.attributes.getNamedItem('Include').Text := Path;

  Node.firstChild.Text := FormName;

  ItemGroup.appendChild(Node);

  FUnitsList.Add('  ' + UnitName.Replace('.pas', ' in ') + Path.QuotedString + ',');

  FChanged := True;

end;

procedure TDprojParser.AddUnit(const Name, Path: string);
var
  ItemGroup, NodeBase, Node: IXMLDOMNode;
begin

  ItemGroup := FXMLDocument.selectSingleNode('/Project/ItemGroup');

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/DCCReference');

  Node := NodeBase.cloneNode(True);

  Node.attributes.getNamedItem('Include').Text := Path;

  if node.hasChildNodes then
    Node.removeChild(Node.firstChild);

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/BuildConfiguration');

  ItemGroup.insertBefore(Node, NodeBase);

  FUnitsList.Add('  ' + Name.Replace('.pas', ' in ') + Path.QuotedString + ',');

  FChanged := True;

end;

procedure TDprojParser.ChangeVersion;
var
  Project, Node, VerInfo_Keys: IXMLNode;
  I, J, K: Integer;
  Keys_String: String;
  Keys : TArray<string>;
  Version: TArray<string>;
begin

 { try

    FXMLDocument.LoadFromFile(DprojFile);

    Project := FXMLDocument.ChildNodes.First;

    J := Project.ChildNodes.Count - 1;

    for I := 0 to J do
    begin
      Node := Project.ChildNodes.Nodes[I];
      VerInfo_Keys := Node.ChildNodes.FindNode('VerInfo_Keys');
      if VerInfo_Keys <> nil then
        begin
        Keys_String := VerInfo_Keys.NodeValue;
        Keys := Keys_String.Split([';']);
        for K := 0 to Length(Keys) - 1  do
          begin
            Version := Keys[K].Split(['=']);
            if Version[0]= 'FileVersion' then
              Keys[K] := 'FileVersion=' + FVersionString;
            if Version[0]= 'ProductVersion' then
              Keys[K] := 'ProductVersion=' + FVersionString;
          end;
        Keys_String := '';
        for K := 0 to Length(Keys) - 1 do
          Keys_String := Keys_String + Keys[K] + ';';
        Keys_String := Keys_String.Substring(0,Keys_String.Length -1);
        VerInfo_Keys.NodeValue := Keys_String;
        end;
    end;

    FXMLDocument.SaveToFile(Dprojfile);

  except
    on E: Exception do
      WriteLn(E.ClassName + ':' + E.Message)
  end;
           }
end;

constructor TDprojParser.Create(const PackagePath, ProjectName: string);
begin

  FPackagePath := PackagePath;
  FProjectName := ProjectName;

  FDprojFile := FPackagePath + ProjectName + '.dproj';
  FDprFile := FPackagePath + ProjectName + '.dpr';

  if not FileExists(FDprojFile) then
    raise EAlfredFileNotFoundException.Create('File ' + FDprojFile.QuotedString + ' not found');

  if not FileExists(FDprFile) then
    raise EAlfredFileNotFoundException.Create('File ' + FDprFile.QuotedString + ' not found');

   CoInitialize(nil);

  FUnitsList := TList<string>.Create;

  FXMLDocument := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  FXMLDocument.async := False;

  FXMLDocument.load(FDprojFile);

  FChanged := False;

end;

destructor TDprojParser.Destroy;
begin
  FUnitsList.Free;
end;

procedure TDprojParser.Save;
begin

  if not FChanged then
    Exit;

  UpdateDpr;

  FXMLDocument.save(FDprojFile);

end;

procedure TDprojParser.SetVersionString(const Value: string);
begin
  FVersionString := Value;
end;

procedure TDprojParser.UpdateDpr;
var
  DprFile: TStringList;
  I, Count: Integer;
  Line: string;
begin

  DprFile := TStringList.Create;

  try

    DprFile.LoadFromFile(FDprFile);

    Count := DprFile.Count -1;

    for I := 0 to Count do
    begin

      Line := DprFile.Strings[I];

      if Line.EndsWith('.pas'';') then
        Break;
    end;

    for Line in FUnitsList do
    begin
      DprFile.Insert(I, Line);
      Inc(I);
    end;

    DprFile.SaveToFile(FDprFile);

  finally
    DprFile.Free;
  end;

end;

end.
