unit Eagle.Alfred.Command.Common.DprojParser;

interface
uses
  System.SysUtils,System.Win.ComObj, XML.adomxmldom, ActiveX, XMLDoc, MSXML, MSXMLDOM, XMLIntf,
  System.IOUtils, System.Classes, System.Generics.Collections,

  Eagle.Alfred.Core.Exceptions;

type

  IDprojParser = interface
    ['{995EE77F-1CD3-45A1-9F77-C718A86293D5}']
    procedure AddForm(const UnitName, FormName, Path: string);
    procedure AddUnit(const Name, Path: string);
    procedure AddPathInUnitSearchPath(const Path: string);
    procedure DeleteUnit(const Name, Path: string);
    procedure DeletePathInUnitSearchPath(const Path: string);
    procedure RemoveLibInSearchPath(const Name: string);
    procedure Save;
  end;

  TDprojParser = class(TInterfacedObject, IDprojParser)
    private
      FXMLDocument: IXMLDomDocument;
      FPackagePath: string;
      FProjectName: string;
      FDprojFile: string;
      FDprFile: string;
      FUnitsList: TList<string>;
      FUnitsDeleted: TList<string>;
      FUnitSearchPathList: TList<string>;
      FUnitSearchPathNode: IXMLDOMNode;
      FChanged: Boolean;
      procedure UpdateDpr;
      function GetUnitSearchPathNode: IXMLDOMNode;
      procedure InitXMLDomDocument;
    public
      constructor Create(const PackagePath, ProjectName: string);
      destructor Destroy; override;
      procedure AddForm(const UnitName, FormName, Path: string);
      procedure AddUnit(const Name, Path: string);
      procedure DeleteUnit(const Name, Path: string);
      procedure AddPathInUnitSearchPath(const Path: string);
      procedure DeletePathInUnitSearchPath(const Path: string);
      procedure RemoveLibInSearchPath(const Name: string);
      procedure Save;
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

  FUnitsList.Add(UnitName.Replace('.pas', ' in ') + Path.QuotedString);

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
  Expression: string;
begin
  Expression := '/Project/ItemGroup/DCCReference[@Include="' + Path.Replace('\', '\\') + '"][1]';

  Node := FXMLDocument.selectSingleNode(Expression);

  if Assigned(Node) then
    Exit;

  ItemGroup := FXMLDocument.selectSingleNode('/Project/ItemGroup');

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/DCCReference');

  Node := NodeBase.cloneNode(True);

  Node.attributes.getNamedItem('Include').Text := Path;

  while Node.hasChildNodes do
    Node.removeChild(Node.firstChild);

  NodeBase := FXMLDocument.selectSingleNode('/Project/ItemGroup/BuildConfiguration');

  ItemGroup.insertBefore(Node, NodeBase);

  FUnitsList.Add(Name.Replace('.pas', ' in ') + Path.QuotedString);

  FChanged := True;
end;

constructor TDprojParser.Create(const PackagePath, ProjectName: string);
begin
  FPackagePath := PackagePath;
  FProjectName := ProjectName;

  FDprojFile := FPackagePath + ProjectName + '.dproj';
  FDprFile := FPackagePath + ProjectName + '.dpr';

  if not FileExists(FDprojFile) then
    raise EFileNotFoundException.Create('File ' + FDprojFile.QuotedString + ' not found');

  if not FileExists(FDprFile) then
    raise EFileNotFoundException.Create('File ' + FDprFile.QuotedString + ' not found');

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
  Node, UnitSearchPathNode, ExeOutputNode: IXMLDOMNode;
  Expression, Condition: string;
begin
  Condition := '$(Base_Win32)'.QuotedString + '!=' + ''.QuotedString;

  Expression := '/Project/PropertyGroup[@Condition="' + Condition + '"]';

  Node := FXMLDocument.selectSingleNode(Expression);

  UnitSearchPathNode := Node.selectSingleNode('//DCC_UnitSearchPath');

  if Assigned(UnitSearchPathNode) then
    Exit(UnitSearchPathNode);

  ExeOutputNode := Node.selectSingleNode('//DCC_ExeOutput');

  UnitSearchPathNode := FXMLDocument.createNode(ExeOutputNode.nodeType, 'DCC_UnitSearchPath', ExeOutputNode.namespaceURI);

  Node.appendChild(UnitSearchPathNode);

  Result := UnitSearchPathNode;
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

procedure TDprojParser.RemoveLibInSearchPath(const Name: string);
var
  I, Count: Integer;
  Value: string;
begin
  if Name.IsEmpty then
    Exit;

  Count := FUnitSearchPathList.Count;

  I := 0;

  while I < Count do
  begin
    Value := FUnitSearchPathList.Items[I];

    if Value.Contains(Name) then
    begin
      FUnitSearchPathList.Delete(I);
      Dec(Count);
    end
    else
      Inc(I);
  end;

  FUnitSearchPathList.TrimExcess;
  FChanged := True;
end;

procedure TDprojParser.Save;
begin
  if not FChanged then
    Exit;

  UpdateDpr;

  if FUnitSearchPathNode <> nil then
    FUnitSearchPathNode.text := string.Join(';', FUnitSearchPathList.ToArray);

  FXMLDocument.save(FDprojFile);

  FChanged := False;
end;

procedure TDprojParser.UpdateDpr;
var
  DprFile: TStringList;
  I, J, Count: Integer;
  Line, UnitRecord: string;
begin
  if FUnitsList.Count = 0 then
    Exit;

  DprFile := TStringList.Create;

  try
    DprFile.LoadFromFile(FDprFile);

    Count := DprFile.Count -1;
    J := 0;

    for I := 0 to Count do
    begin
      Line := DprFile.Strings[I];

      UnitRecord := Line.Trim.Replace(',', '').Replace(';', '');

      if FUnitsList.Contains(UnitRecord) then
        FUnitsList.Remove(UnitRecord);

      if Line.EndsWith('.pas'';') then
      begin
        DprFile.Delete(I);
        FUnitsList.Insert(0, UnitRecord);
        Break;
      end;
    end;

    for J := 0 to FUnitsList.Count - 2 do
    begin
      Line := FUnitsList.Items[J];
      DprFile.Insert(I, '  ' + Line + ',');
      Inc(I);
    end;

    Line := FUnitsList.Items[J];
    DprFile.Insert(I, '  ' + Line + ';');

    DprFile.SaveToFile(FDprFile);

  finally
    DprFile.Free;
  end;
end;

end.
