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
    procedure AddPathInUnitSearchPath(const Path: string);
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
      FUnitsDeleted: TList<string>;
      FUnitSearchPathList: TList<string>;
      FUnitSearchPathNode: IXMLDOMNode;
      FChanged: Boolean;
      procedure SetVersionString(const Value: string);
      procedure UpdateDpr;
      function GetUnitSearchPathNode: IXMLDOMNode;
      procedure InitXMLDomDocument;
    public
      procedure ChangeVersion;
      constructor Create(const PackagePath, ProjectName: string);
      destructor Destroy; override;
      procedure AddForm(const UnitName, FormName, Path: string);
      procedure AddUnit(const Name, Path: string);
      procedure DeleteUnit(const Name, Path: string);
      procedure AddPathInUnitSearchPath(const Path: string);
      procedure DeletePathInUnitSearchPath(const Path: string);
      procedure Save;

      property VersionString: string read FVersionString write SetVersionString;
  end;

implementation

procedure TDprojParser.AddForm(const UnitName, FormName, Path: string);
var
  ItemGroup, NodeBase, Node: IXMLDOMNode;
begin

  ItemGroup := FXMLDocument.selectSingleNode('/Project/ItemGroup');

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/DCCReference/Form').parentNode;

  Node := NodeBase.cloneNode(True);

  Node.attributes.getNamedItem('Include').Text := Path;

  Node.selectSingleNode('//Form').text := FormName;

  ItemGroup.insertBefore(Node, ItemGroup.selectSingleNode('//BuildConfiguration'));

  FUnitsList.Add('  ' + UnitName.Replace('.pas', ' in ') + Path.QuotedString);

  FChanged := True;

end;

procedure TDprojParser.AddPathInUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin

  UnitPath := Path.Trim;

  if UnitPath.EndsWith('\') then
    UnitPath := UnitPath.Remove(UnitPath.Length-1);

  if UnitPath.IsEmpty or FUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FUnitSearchPathList.Add(UnitPath);

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

  while node.hasChildNodes do
    Node.removeChild(Node.firstChild);

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/BuildConfiguration');

  ItemGroup.insertBefore(Node, NodeBase);

  FUnitsList.Add('  ' + Name.Replace('.pas', ' in ') + Path.QuotedString);

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

  FUnitsDeleted := TList<string>.Create;

  FUnitSearchPathList := TList<string>.Create;

  InitXMLDomDocument;

  FChanged := False;

end;

procedure TDprojParser.DeletePathInUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin

  UnitPath := Path.Trim;

  if UnitPath.IsEmpty or not FUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FUnitSearchPathList.Remove(UnitPath);

  FChanged := True;

end;

procedure TDprojParser.DeleteUnit(const Name, Path: string);
begin

  FUnitsDeleted.Add(Name);

end;

destructor TDprojParser.Destroy;
begin

  if Assigned(FUnitsList) then
    FreeAndNil(FUnitsList);

  if Assigned(FUnitsDeleted) then
    FreeAndNil(FUnitsDeleted);

  if Assigned(FUnitSearchPathList) then
    FreeAndNil(FUnitSearchPathList);

end;

function TDprojParser.GetUnitSearchPathNode: IXMLDOMNode;
var
  Node, Attribute: IXMLDOMNode;
  NodeList: IXMLDOMNodeList;
  Condition, AttributeValue: string;
begin

  Condition := '$(Cfg_1_Win32)'.QuotedString + '!=' + ''.QuotedString;

  NodeList := FXMLDocument.selectNodes('/Project/PropertyGroup/DCC_UnitSearchPath');

  Node := NodeList.nextNode;

  while Node <> nil do
  begin

    Attribute := Node.parentNode.attributes.getNamedItem('Condition');

    if Attribute = nil then
      Continue;

    AttributeValue := Attribute.Text;

    if AttributeValue.Equals(Condition) then
    begin
      Result := Node;
      Break;
    end;

    Node := NodeList.nextNode;

  end;

end;

procedure TDprojParser.InitXMLDomDocument;
var
  UnitsList: string;
begin

  FXMLDocument := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  FXMLDocument.async := False;

  FXMLDocument.preserveWhiteSpace := True;

  FXMLDocument.load(FDprojFile);

  FUnitSearchPathNode := GetUnitSearchPathNode;

  if FUnitSearchPathNode = nil then
    Exit;

  UnitsList := FUnitSearchPathNode.text;

  FUnitSearchPathList.AddRange(UnitsList.Split([';']));

end;

procedure TDprojParser.Save;
begin

  if not FChanged then
    Exit;

  UpdateDpr;

  if FUnitSearchPathNode <> nil then
    FUnitSearchPathNode.text := string.Join(';', FUnitSearchPathList.ToArray);

  FXMLDocument.save(FDprojFile);

end;

procedure TDprojParser.SetVersionString(const Value: string);
begin
  FVersionString := Value;
end;

procedure TDprojParser.UpdateDpr;
var
  DprFile: TStringList;
  I, J, Count: Integer;
  Line: string;
begin

  if FUnitsList.Count = 0 then
    Exit;

  DprFile := TStringList.Create;

  try

    DprFile.LoadFromFile(FDprFile);

    Count := DprFile.Count -1;

    for I := 0 to Count do
    begin

      Line := DprFile.Strings[I];

      if Line.EndsWith('.pas'';') then
      begin
        DprFile.Delete(I);
        Line := Line.Replace(';', '');
        FUnitsList.Insert(0, Line);
        Break;
      end;
    end;

    for J := 0 to FUnitsList.Count - 2 do
    begin
      Line := FUnitsList.Items[J];
      DprFile.Insert(I, Line + ',');
      Inc(I);
    end;

    Line := FUnitsList.Items[J];
    DprFile.Insert(I, Line + ';');

    DprFile.SaveToFile(FDprFile);

  finally
    DprFile.Free;
  end;

end;

end.
