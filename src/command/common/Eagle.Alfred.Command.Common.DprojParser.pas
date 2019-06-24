unit Eagle.Alfred.Command.Common.DprojParser;

interface
uses
  System.SysUtils,System.Win.ComObj, XML.adomxmldom, ActiveX, XMLDoc, MSXML, MSXMLDOM, XMLIntf,
  System.IOUtils, System.Classes, System.Generics.Collections,

  Eagle.Alfred.Core.Exceptions;

const
  RELEASE_MODE = '$(Cfg_1_Win32)';
  DEBUG_MODE = '$(Cfg_2_Win32)';

type

  IDprojParser = interface
    ['{995EE77F-1CD3-45A1-9F77-C718A86293D5}']
    procedure AddForm(const UnitName, FormName, Path: string);
    procedure AddUnit(const Name, Path: string);
    procedure DeleteUnit(const Name, Path: string);
    procedure AddPathInReleaseUnitSearchPath(const Path: string);
    procedure AddPathInDebugUnitSearchPath(const Path: string);
    procedure AddPathInUnitSourcePath(const Path: string);
    procedure DeletePathInReleaseUnitSearchPath(const Path: string);
    procedure DeletePathInDebugUnitSearchPath(const Path: string);
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
      FReleaseUnitSearchPathList: TList<string>;
      FDebugUnitSearchPathList: TList<string>;
      FUnitSourcePathList:  TList<string>;
      FReleaseUnitSearchPathNode: IXMLDOMNode;
      FDebugUnitSearchPathNode: IXMLDOMNode;
      FUnitSourcePathNode: IXMLDOMNode;
      FChanged: Boolean;
      procedure DoRemoveLibInDebugSearchPath(const Name: string);
      procedure DoRemoveLibInReleaseSeachPath(const Name: string);
      procedure UpdateDpr;
      function GetUnitPathNode(const Mode, NodeTag: string): IXMLDOMNode;
      procedure InitXMLDomDocument;
      procedure LoadDebugUnitSearchPathList;
      procedure LoadReleaseUnitSearchPathList;
      procedure LoadUnitSourcePathList;
    public
      constructor Create(const PackagePath, ProjectName: string);
      destructor Destroy; override;
      procedure AddForm(const UnitName, FormName, Path: string);
      procedure AddUnit(const Name, Path: string);
      procedure DeleteUnit(const Name, Path: string);
      procedure AddPathInReleaseUnitSearchPath(const Path: string);
      procedure AddPathInDebugUnitSearchPath(const Path: string);
      procedure AddPathInUnitSourcePath(const Path: string);
      procedure DeletePathInReleaseUnitSearchPath(const Path: string);
      procedure DeletePathInDebugUnitSearchPath(const Path: string);
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

procedure TDprojParser.AddPathInReleaseUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin
  UnitPath := Path.Trim;

  if UnitPath.EndsWith('\') then
    UnitPath := UnitPath.Remove(UnitPath.Length-1);

  if UnitPath.IsEmpty or FReleaseUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FReleaseUnitSearchPathList.Add(UnitPath);

  FChanged := True;
end;

procedure TDprojParser.AddPathInUnitSourcePath(const Path: string);
begin
end;

procedure TDprojParser.AddPathInDebugUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin
  UnitPath := Path.Trim;

  if UnitPath.EndsWith('\') then
    UnitPath := UnitPath.Remove(UnitPath.Length-1);

  if UnitPath.IsEmpty or FDebugUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FDebugUnitSearchPathList.Add(UnitPath);

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

  FReleaseUnitSearchPathList := TList<string>.Create;
  FDebugUnitSearchPathList := TList<string>.Create;
  FUnitSourcePathList := TList<string>.Create;

  InitXMLDomDocument;

  FChanged := False;
end;

procedure TDprojParser.DeletePathInDebugUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin
  UnitPath := Path.Trim;

  if UnitPath.IsEmpty or not FDebugUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FDebugUnitSearchPathList.Remove(UnitPath);

  FChanged := True;
end;

procedure TDprojParser.DeletePathInReleaseUnitSearchPath(const Path: string);
var
  UnitPath: string;
begin
  UnitPath := Path.Trim;

  if UnitPath.IsEmpty or not FReleaseUnitSearchPathList.Contains(UnitPath) then
    Exit;

  FReleaseUnitSearchPathList.Remove(UnitPath);

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

  if Assigned(FReleaseUnitSearchPathList) then
    FreeAndNil(FReleaseUnitSearchPathList);

  if Assigned(FDebugUnitSearchPathList) then
    FreeAndNil(FDebugUnitSearchPathList);

  if Assigned(FUnitSourcePathList) then
    FreeAndNil(FUnitSourcePathList);
end;

procedure TDprojParser.DoRemoveLibInDebugSearchPath(const Name: string);
var
  I: Integer;
  Count: Integer;
  Value: string;
begin
  Count := FDebugUnitSearchPathList.Count;

  I := 0;

  while I < Count do
  begin
    Value := FDebugUnitSearchPathList.Items[I];

    if Value.Contains(Name) then
    begin
      FDebugUnitSearchPathList.Delete(I);
      Dec(Count);
    end
    else
      Inc(I);
  end;

  FDebugUnitSearchPathList.TrimExcess;
  FChanged := True;
end;

procedure TDprojParser.DoRemoveLibInReleaseSeachPath(const Name: string);
var
  I: Integer;
  Count: Integer;
  Value: string;
begin
  Count := FReleaseUnitSearchPathList.Count;

  I := 0;

  while I < Count do
  begin
    Value := FReleaseUnitSearchPathList.Items[I];

    if Value.Contains(Name) then
    begin
      FReleaseUnitSearchPathList.Delete(I);
      Dec(Count);
    end
    else
      Inc(I);
  end;

  FReleaseUnitSearchPathList.TrimExcess;
  FChanged := True;
end;

function TDprojParser.GetUnitPathNode(const Mode, NodeTag: string): IXMLDOMNode;
var
  Node, UnitPathNode: IXMLDOMNode;
  Expression, Condition: string;
begin
  Condition := Mode.QuotedString + '!=' + ''.QuotedString;

  Expression := '/Project/PropertyGroup[@Condition="' + Condition + '"]';

  UnitPathNode := FXMLDocument.selectSingleNode(Expression + '/' + NodeTag);

  if Assigned(UnitPathNode) then
    Exit(UnitPathNode);

  Node := FXMLDocument.selectSingleNode(Expression);

  UnitPathNode := FXMLDocument.createNode(NODE_ELEMENT, NodeTag, Node.namespaceURI);

  Node.appendChild(UnitPathNode);

  Result := UnitPathNode;
end;

procedure TDprojParser.InitXMLDomDocument;
begin
  FXMLDocument := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  FXMLDocument.async := False;

  FXMLDocument.preserveWhiteSpace := True;

  FXMLDocument.load(FDprojFile);

  LoadReleaseUnitSearchPathList;

  LoadDebugUnitSearchPathList;

  LoadUnitSourcePathList;
end;

procedure TDprojParser.LoadDebugUnitSearchPathList;
var
  UnitsList: string;
begin
  FDebugUnitSearchPathNode := GetUnitPathNode(DEBUG_MODE, 'DCC_UnitSearchPath');

  if not Assigned(FDebugUnitSearchPathNode) then
    Exit;

  UnitsList := FDebugUnitSearchPathNode.text;

  FDebugUnitSearchPathList.AddRange(UnitsList.Split([';']));
end;

procedure TDprojParser.LoadReleaseUnitSearchPathList;
var
  UnitsList: string;
begin
  FReleaseUnitSearchPathNode := GetUnitPathNode(RELEASE_MODE, 'DCC_UnitSearchPath');

  if not Assigned(FReleaseUnitSearchPathNode) then
    Exit;

  UnitsList := FReleaseUnitSearchPathNode.text;

  FReleaseUnitSearchPathList.AddRange(UnitsList.Split([';']));
end;

procedure TDprojParser.LoadUnitSourcePathList;
var
  UnitsList: string;
begin
  FUnitSourcePathNode := GetUnitPathNode('$(Base_Win32)', 'Debugger_DebugSourcePath');

  if not Assigned(FUnitSourcePathNode) then
    Exit;

  UnitsList := FUnitSourcePathNode.text;

  FUnitSourcePathList.AddRange(UnitsList.Split([';']));
end;

procedure TDprojParser.RemoveLibInSearchPath(const Name: string);
begin
  if Name.IsEmpty then
    Exit;

  DoRemoveLibInReleaseSeachPath(Name);
  DoRemoveLibInDebugSearchPath(Name);
end;

procedure TDprojParser.Save;
begin
  if not FChanged then
    Exit;

  UpdateDpr;

  if FReleaseUnitSearchPathNode <> nil then
    FReleaseUnitSearchPathNode.text := string.Join(';', FReleaseUnitSearchPathList.ToArray);

  if FDebugUnitSearchPathNode <> nil then
    FDebugUnitSearchPathNode.text := string.Join(';', FDebugUnitSearchPathList.ToArray);

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
