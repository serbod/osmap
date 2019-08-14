(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2019  Sergey Bodrov

  This source is ported from libosmscout library
  Copyright (C) 2009  Tim Teulings

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

*)
(*
  OsMap routing
routing\DBFileOffset
  DBId -> TMapDBId
  DBFileOffset -> TMapDBFileOffset

routing\route
  Description -> TRouteItemDescription
  Node -> TRouteItem
  RouteDescription -> TRouteDescription

ObjectVariantDataFile

routing\RouteNode
  ObjectVariantData
  ObjectData -> TRouteNodeObjectData
  Exclude -> TRouteNodeExclude
  Path -> TRouteNodePath
  RouteNode -> TRouteNode

routing\RouteData
  RouteData
  RouteEntry
*)
unit OsMapRouting;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, OsMapTypes, OsMapGeometry, OsMapUtils, OsMapObjTypes,
  OsMapFiles;

const
  NODE_START_DESC        = 'NodeStart';       // start node (StartDescription)
  NODE_TARGET_DESC       = 'NodeTarget';      // target node (TargetDescription)
  WAY_NAME_DESC          = 'WayName';         // name of the way (NameDescription)
  WAY_NAME_CHANGED_DESC  = 'WayChangedName';  // a change of way name (NameChangedDescription)
  CROSSING_WAYS_DESC     = 'CrossingWays';    // list of way name crossing a node (CrossingWaysDescription)
  DIRECTION_DESC         = 'Direction';       // a turn (TurnDescription)
  TURN_DESC              = 'Turn';            // an explicit turn (TurnDescription)
  ROUNDABOUT_ENTER_DESC  = 'RountaboutEnter'; // entering a roundabout (RoundaboutEnterDescription)
  ROUNDABOUT_LEAVE_DESC  = 'RountaboutLeave'; // leaving a roundabout (RoundaboutLeaveDescription)
  MOTORWAY_ENTER_DESC    = 'MotorwayEnter';   // entering a motorway (MotorwayEnterDescription)
  MOTORWAY_CHANGE_DESC   = 'MotorwayChange';  // changing a motorway (MotorwayChangeDescription)
  MOTORWAY_LEAVE_DESC    = 'MotorwayLeave';   // leaving a motorway (MotorwayLeaveDescription)
  MOTORWAY_JUNCTION_DESC = 'MotorwayJunction'; // motorway junction (MotorwayJunctionDescription)
  CROSSING_DESTINATION_DESC = 'CrossingDestination'; // destination to choose at a junction
  WAY_MAXSPEED_DESC      = 'MaxSpeed';        // the maximum speed for the given way
  WAY_TYPE_NAME_DESC     = 'TypeName';        // type name of the way (TypeNameDescription)
  POI_AT_ROUTE_DESC      = 'POIAtRoute';      //

type
  TMapDatabaseId = LongWord;

  { Helper structure to implement a reference to a routing node in a given
    database (identified by a unique index). }

  { TMapDBId }

  TMapDBId = object
    DatabaseId: TMapDatabaseId;
    Id: TId;

    procedure Init(ADatabaseId: TMapDatabaseId; AId: TId);
  end;

  TMapDBIdArray = array of TMapDBId;

  { TMapDBFileOffset }

  TMapDBFileOffset = object
    DatabaseId: TMapDatabaseId;
    Offset: TFileOffset;

    procedure Init(ADatabaseId: TMapDatabaseId; AOffset: TFileOffset);
  end;

{ Description of a route, enhanced with information that are required to
  give a human textual (or narrative) drive instructions;

  A route consists of nodes. A Node can be the crossing point of a number of
  ways and is a route decision point (where the driver possibly has the change ways)
  that requires some potential action by the driver.

  For each node you can pass a number of descriptions. For the way from the current node
  to the next node also a number of descriptions can get retrieved.

  Descriptions are typed and must derive from class Description.. }

  TRouteItemType = (
    riStart,         // Start of the route
    riTarget,        // Target of the route
    riName,          // Something has a name. A name consists of a name and a optional alphanumeric reference (LIke B1 or A40).
    riNameChanged,   // Contain original and target names
    riCrossingWays,  // List the names of all ways, that are crossing the current node.
    riDirection,     // Describes the turn and the curve while getting from the previous node to the next node via the current node.
    riTurn,          // Signals an explicit turn
    riRoundaboutEnter, // Signals entering a roundabout
    riRoundaboutLeave, // Signals leaving a roundabout
    riMotorwayEnter, // Signals entering a motorway
    riMotorwayChange, // Signals changing a motorway
    riMotorwayLeave, // Signals leaving a motorway
    riMotorwayJunction, // A motorway junction
    riDestination,   // Destination of the route
    riMaxSpeed,      // Max speed
    riTypeName,      // Something has a type name. This is the name of the type of the way used.
    riPOIAtRoute     // POI at route
  );

  { Base class of all descriptions. }

  { TRouteItemDescription }

  TRouteItemDescription = class
  public
    ItemType: TRouteItemType;
    Values: array of string;
    function GetDebugString(): string; virtual;

    procedure CombineNameRef(out AValue: string; const AName, ARef: string);
    procedure ExtractNameRef(const AValue: string; out AName, ARef: string);
    function GetValue(AIndex: Integer): string;
    function AngleToDirectionStr(AAngle: Double): string;

    // riStart, riTarget, riName
    // riMotorwayEnter, riMotorwayLeave, riMotorwayJunction
    // riDestination, riTypeName
    function GetDescription(): string;
    procedure SetDescription(const AValue: string; AItemType: TRouteItemType);
    // riNameChanged
    procedure SetNameChanged(const AOrigin, ATarget: string);
    // riCrossingWays
    procedure SetCrossingWays(AExitCount: Integer; const AOrigin, ATarget: string);
    procedure AddCrossingWay(const ADescription: string);
    function GetExitCount(): Integer;
    // riDirection
    {  Describes the turn and the curve while getting from the previous node to the next node via the current node.
       The turn is the angle between the incoming way (previous node and current node)
     and the outgoing way (current node and next node) at the given node.
       The curve is a heuristic measurement that not only take the next node of the target way into
     account (which could only the start of a slight curve) but tries to determine the last node
     of the curve and this gives a better description of the curve the vehicle needs to take. }
    procedure SetDirection(ATurnAngle, ACurveAngle: Double);
    function GetTurnAngle(): Double;
    function GetCurveAngle(): Double;
    function GetTurn(): string;
    function GetCurve(): string;
    // riRoundaboutLeave
    procedure SetRoundaboutLeave(AExitCount: Integer);
    // riMotorwayChange
    procedure SetMotorwayChange(const AOrigin, ATarget: string);
    // riMaxSpeed
    procedure SetMaxSpeed(AValue: Integer);
    // riPOIAtRoute
    procedure SetPOIAtRoute(ADatabaseID: TMapDatabaseId; AObject: TObjectFileRef;
      AName: string; ADistance: TDistance);
  end;

  { TRouteItemDescriptionList }

  TRouteItemDescriptionList = class(TList)
  protected
    FStringHash: TSimpleStringHash;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AddItem(const AName: string; AItem: TRouteItemDescription);
    function GetItem(AIndex: Integer): TRouteItemDescription;
    function GetItemByName(const AName: string): TRouteItemDescription;
  end;

  TObjectFileRefArray = array of TObjectFileRef;

  TRouteItem = class
  private
    FDatabaseId: TMapDatabaseId;
    FCurrentNodeIndex: Integer;
    FObjects: TObjectFileRefArray;
    FPathObject: TObjectFileRef;
    FTargetNodeIndex: Integer;
    FDistance: TDistance;  // distance from route start (meters)
    FTime: TDateTime;   // time from route start
    FLocation: TGeoPoint;
    //FDescriptionMap: std::unordered_map<std::string,DescriptionRef> ;
    FDescriptions: TRouteItemDescriptionList;
  public
    constructor Create(ADatabaseId: TMapDatabaseId;
      ACurrentNodeIndex: Integer;
      const AObjects: TObjectFileRefArray;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer);

    { There exists a object/path from the current node to the next node in the route. }
    function HasPathObject(): Boolean;

    function GetDBFileOffset(): TMapDBFileOffset;

    function HasDescription(const AName: string): Boolean;
    function GetDescription(const AName: string): TRouteItemDescription;
    procedure AddDescription(const AName: string; ADescription: TRouteItemDescription);

    property CurrentNodeIndex: Integer read FCurrentNodeIndex;
    { Return the objects that intersect at the current node index. }
    property Objects: TObjectFileRefArray read FObjects;
    { Return a list of descriptions attached to the current node }
    property Descriptions: TRouteItemDescriptionList read FDescriptions;

    property DatabaseId: TMapDatabaseId read FDatabaseId;
    { Return the path object that connects the current node to the next node. }
    property PathObject: TObjectFileRef read FPathObject;
    { The the index of the target node on the path that is the next node on the route. }
    property TargetNodeIndex: Integer read FTargetNodeIndex;
    { Distance from the start of the route in meters. }
    property Distance: TDistance read FDistance write FDistance;
    { Time from the start of the route }
    property Time: TDateTime read FTime write FTime;
    { Location (latitude,longitude) of the node }
    property Location: TGeoPoint read FLocation write FLocation;
  end;

  { TRouteItemList }

  TRouteItemList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRouteItem;
    function AddItem(ADatabaseId: TMapDatabaseId;
      ACurrentNodeIndex: Integer;
      const AObjects: TObjectFileRefArray;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer): Integer;
  end;

  { TRouteDescription }

  TRouteDescription = class
  private
    FNodes: TRouteItemList;
    FDatabaseMapping: TStringList; // DatabaseId : string

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear();
    function IsEmpty: Boolean;

    procedure AddNode(ADatabaseId: TMapDatabaseId;
      ACurrentNodeIndex: Integer;
      const AObjects: TObjectFileRefArray;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer);

    property Nodes: TRouteItemList read FNodes;
    property DatabaseMapping: TStringList read FDatabaseMapping;
  end;

  { For every unique combination of object attributes that are routing
    relevant we store an ObjectvariantData entry. }

  { TObjectVariantData }

  TObjectVariantData = object
    TypeInfo: TTypeInfo;  // The type of the object
    MaxSpeed: Byte;       // Maximum speed allowed on the way
    Grade: Byte;          // Quality of road/track 1 (good)...5 (bad)

    { Read data from the given FileScanner }
    procedure Read(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
    { Write the data to the given FileWriter. }
    procedure Write(AWriter: TFileWriter);
  end;

  { DataFile class for loading the object variant data, which is part of the
    routing graph. The object variat data contains a lookup table for path
    attributes. Since the number of attribute value combinations is much
    smaller then the actual number of elements in the route graph it makes
    sense to store all possible combinations in a lookup table and just
    index them from the routing graph paths. }

  { TObjectVariantDataFile }

  TObjectVariantDataFile = object
    Data: array of TObjectVariantData;
    IsLoaded: Boolean;
    Filename: string;

    procedure Init();
    function Load(ATypeConfig: TTypeConfig; AFilename: string): Boolean;
  end;

  { Information for an object referenced by a path. }
  TRouteNodeObjectData = record
    Obj: TObjectFileRef;       // Reference to the object
    ObjVariantIndex: Integer;  // Index into the lookup table, holding object specific routing data
  end;

  { Exclude regarding use of paths. You cannot use the path with the index "targetPath" if you come
    from the source object. }
  TRouteNodeExclude = record
    Source: TObjectFileRef; // The source object
    TargetIndex: Integer;   // The index of the target path
  end;

  { A single path that starts at the given route node. A path contains a number of information
    that are relevant for the router. }
  TRouteNodePath = record
    Distance: TDistance;  // Distance from the current route node to the target route node
    Id: TId;              // id of the targeting route node
    ObjectIndex: Integer; // The index of the way to use from this route node to the target route node
    Flags: TVehicleTypes; // Certain flags
  end;

  { TRouteNode }

  TRouteNode = object
  private
    FFileOffset: TFileOffset;
    FPoint: TGeoPoint;
    FSerial: Byte;
  public
    Objects: array of TRouteNodeObjectData;
    Paths: array of TRouteNodePath;
    Excludes: array of TRouteNodeExclude;

    procedure Init(const AFileOffset: TFileOffset; const APoint: TGeoPoint);

    function AddObject(const AObj: TObjectFileRef; AObjVariantIndex: Integer): Integer;

    procedure Read(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
    procedure Write(AWriter: TFileWriter);

    // FileOffset of the route node
    property FileOffset: TFileOffset read FFileOffset;
    // Coordinate and id of the route node
    property Point: TGeoPoint read FPoint;
    property Serial: Byte read FSerial;
  end;

  { TRouteEntry }

  TRouteEntry = object
    DatabaseId: TMapDatabaseId;
    CurNodeId: TId;
    CurNodeIndex: Integer;
    Objects: TObjectFileRefArray;
    PathObject: TObjectFileRef;
    TargetNodeIndex: Integer;

    procedure Init(ADatabaseId: TMapDatabaseId;
      ACurNodeId: TId;
      ACurNodeIndex: Integer;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer);

    function GetDBFileOffset(): TMapDBFileOffset;
  end;

  TRouteEntryArray = array of TRouteEntry;

  { TRouteData }

  TRouteData = object
    Entries: TRouteEntryArray;

    procedure Clear();

    procedure AddEntry(ADatabaseId: TMapDatabaseId;
      ACurNodeId: TId;
      ACurNodeIndex: Integer;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer);

    procedure Append(ARoutePart: TRouteData);
    procedure PopEntry();

    function IsEmpty(): Boolean;
  end;

implementation

uses Math;

{ TMapDBId }

procedure TMapDBId.Init(ADatabaseId: TMapDatabaseId; AId: TId);
begin
  DatabaseId := ADatabaseId;
  Id := AId;
end;

{ TMapDBFileOffset }

procedure TMapDBFileOffset.Init(ADatabaseId: TMapDatabaseId;
  AOffset: TFileOffset);
begin
  DatabaseId := ADatabaseId;
  Offset := AOffset;
end;

{ TRouteItemDescription }

function TRouteItemDescription.GetDebugString(): string;
var
  i: Integer;
begin
  case ItemType of
    riStart:
      Result := 'Start: ' + GetValue(0);
    riTarget:
      Result := 'Target: ' + GetValue(0);
    riName:
      Result := 'Name: ' + GetValue(0);
    riNameChanged:
      Result := 'Name Change: ' + GetValue(0) + ' -> ' + GetValue(1);
    riCrossingWays:
    begin
      Result := 'Crossing: from ' + GetValue(0) + ' to ' + GetValue(1)
              + ' exits (' + GetValue(2) + '): ';
      for i := 3 to Length(Values)-1 do
      begin
        if i > 3 then
          Result := Result + ', ';
        Result := Result + GetValue(i);
      end;
    end;
    riDirection:
    begin
      Result := 'Direction: '
              + GetValue(2) + ' (' + GetValue(0) + ' deg), curve '
              + GetValue(3) + ' (' + GetValue(1) + ' deg), ';
    end;
    riTurn:
      Result := 'Turn';
    riRoundaboutEnter:
      Result := 'Enter roundabout';
    riRoundaboutLeave:
      Result := 'Leave roundabout';
    riMotorwayEnter:
      Result := 'Enter motorway: ' + GetValue(0);
    riMotorwayChange:
      Result := 'Change motorway';
    riMotorwayLeave:
      Result := 'Leave motorway';
    riMotorwayJunction:
      Result := 'Motorway junction';
    riDestination:
      Result := 'Destination: ' + GetValue(0);
    riMaxSpeed:
      Result := 'Max. speed: ' + GetValue(0);
    riTypeName:
      Result := 'Type name: ' + GetValue(0);
    riPOIAtRoute:
      Result := 'POI: ' + GetValue(0) + ' (' + GetValue(1) + ' m)';
  else
    Result := '';
    for i := Low(Values) to High(Values) do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Values[i];
    end;
  end;
end;

procedure TRouteItemDescription.CombineNameRef(out AValue: string; const AName,
  ARef: string);
begin
  AValue := AName;
  if ARef <> '' then
    AValue := AValue + '|' + ARef;
end;

procedure TRouteItemDescription.ExtractNameRef(const AValue: string; out AName,
  ARef: string);
var
  n: Integer;
begin
  n := Pos('|', AValue);
  if n = 0 then
  begin
    AName := AValue;
    ARef := '';
  end
  else
  begin
    AName := Copy(AValue, 1, n-1);
    ARef := Copy(AValue, n+1, MaxInt);
  end;
end;

function TRouteItemDescription.GetValue(AIndex: Integer): string;
begin
  if Length(Values) >= (AIndex + 1) then
    Result := Values[AIndex]
  else
    Result := '';
end;

function TRouteItemDescription.AngleToDirectionStr(AAngle: Double): string;
begin
  if (AAngle >= -10.0) and (AAngle <= 10.0) then
    Result := '^'
  else
  if (AAngle >= -45.0) then
    Result := '<'
  else
  if (AAngle >= -120.0) then
    Result := '<<'
  else
  if (AAngle >= -180.0) then
    Result := '<<<'
  else
  if (AAngle <= 45.0) then
    Result := '>'
  else
  if (AAngle <= 120.0) then
    Result := '>>'
  else
  if (AAngle <= 180.0) then
    Result := '>>>'
  else
    Result := '???'
end;

function TRouteItemDescription.GetDescription(): string;
begin
  if Length(Values) > 0 then
    Result := Values[0]
  else
    Result := '';
end;

procedure TRouteItemDescription.SetDescription(const AValue: string;
  AItemType: TRouteItemType);
begin
  ItemType := AItemType;
  if Length(Values) = 0 then
    SetLength(Values, 1);
  Values[0] := AValue;
end;

procedure TRouteItemDescription.SetNameChanged(const AOrigin, ATarget: string);
begin
  ItemType := riNameChanged;
  if Length(Values) < 2 then
    SetLength(Values, 2);
  Values[0] := AOrigin;
  Values[1] := ATarget;
end;

procedure TRouteItemDescription.SetCrossingWays(AExitCount: Integer;
  const AOrigin, ATarget: string);
begin
  ItemType := riCrossingWays;
  if Length(Values) < 3 then
    SetLength(Values, 3);
  Values[0] := IntToStr(AExitCount);
  Values[1] := AOrigin;
  Values[2] := ATarget;
end;

procedure TRouteItemDescription.AddCrossingWay(const ADescription: string);
var
  n: Integer;
begin
  Assert(ItemType = riCrossingWays);
  if Length(Values) < 3 then
    SetLength(Values, 3);
  n := Length(Values);
  SetLength(Values, n+1);
  Values[n] := ADescription;
end;

function TRouteItemDescription.GetExitCount(): Integer;
begin
  Assert(ItemType in [riCrossingWays, riRoundaboutLeave]);
  if Length(Values) > 0 then
    Result := StrTointDef(Values[0], 0)
  else
    Result := 0;
end;

procedure TRouteItemDescription.SetDirection(ATurnAngle, ACurveAngle: Double);
begin
  ItemType := riDirection;
  if Length(Values) < 4 then
    SetLength(Values, 4);
  Values[0] := FloatToStr(ATurnAngle);
  Values[1] := FloatToStr(ACurveAngle);
  Values[2] := AngleToDirectionStr(ATurnAngle);
  Values[3] := AngleToDirectionStr(ACurveAngle);
end;

function TRouteItemDescription.GetTurnAngle(): Double;
begin
  Result := StrToFloatDef(GetValue(0), 0);
end;

function TRouteItemDescription.GetCurveAngle(): Double;
begin
  Result := StrToFloatDef(GetValue(1), 0);
end;

function TRouteItemDescription.GetTurn(): string;
begin
  Result := GetValue(2);
end;

function TRouteItemDescription.GetCurve(): string;
begin
  Result := GetValue(3);
end;

procedure TRouteItemDescription.SetRoundaboutLeave(AExitCount: Integer);
begin
  ItemType := riRoundaboutLeave;
  if Length(Values) < 1 then
    SetLength(Values, 1);
  Values[0] := IntToStr(AExitCount);
end;

procedure TRouteItemDescription.SetMotorwayChange(const AOrigin,
  ATarget: string);
begin
  ItemType := riMotorwayChange;
  if Length(Values) < 2 then
    SetLength(Values, 2);
  Values[0] := AOrigin;
  Values[1] := ATarget;
end;

procedure TRouteItemDescription.SetMaxSpeed(AValue: Integer);
begin
  ItemType := riMaxSpeed;
  if Length(Values) < 1 then
    SetLength(Values, 1);
  Values[0] := IntToStr(AValue);
end;

procedure TRouteItemDescription.SetPOIAtRoute(ADatabaseID: TMapDatabaseId;
  AObject: TObjectFileRef; AName: string; ADistance: TDistance);
begin
  ItemType := riPOIAtRoute;
  if Length(Values) < 4 then
    SetLength(Values, 4);
  Values[0] := IntToStr(ADatabaseID);
  Values[1] := AObject.GetName();
  Values[2] := AName;
  Values[3] := FloatToStr(ADistance);
end;

{ TRouteItemDescriptionList }

procedure TRouteItemDescriptionList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TRouteItemDescription(Ptr).Free();
end;

procedure TRouteItemDescriptionList.AfterConstruction;
begin
  inherited AfterConstruction;
  FStringHash.Init(16);
end;

procedure TRouteItemDescriptionList.BeforeDestruction;
begin
  FStringHash.Clear();
  inherited BeforeDestruction;
end;

procedure TRouteItemDescriptionList.AddItem(const AName: string;
  AItem: TRouteItemDescription);
var
  n: Integer;
begin
  Assert(not FStringHash.FindValue(AName, n));
  n := Add(AItem);
  FStringHash.Add(AName, n);
end;

function TRouteItemDescriptionList.GetItem(AIndex: Integer): TRouteItemDescription;
begin
  Result := TRouteItemDescription(Get(AIndex));
end;

function TRouteItemDescriptionList.GetItemByName(const AName: string): TRouteItemDescription;
var
  n: Integer;
begin
  n := FStringHash.ValueOf(AName);
  if n <> -1 then
    Result := GetItem(n)
  else
    Result := nil;
end;

{ TRouteItem }

constructor TRouteItem.Create(ADatabaseId: TMapDatabaseId;
  ACurrentNodeIndex: Integer; const AObjects: TObjectFileRefArray;
  const APathObject: TObjectFileRef; ATargetNodeIndex: Integer);
begin
  inherited Create;
  FDatabaseId := ADatabaseId;
  FCurrentNodeIndex := ACurrentNodeIndex;
  FObjects := AObjects;
  FPathObject := APathObject;
  FTargetNodeIndex := ATargetNodeIndex;
end;

function TRouteItem.HasPathObject(): Boolean;
begin
  Result := FPathObject.IsValid();
end;

function TRouteItem.GetDBFileOffset(): TMapDBFileOffset;
begin
  Result.Init(FDatabaseId, FPathObject.Offset);
end;

function TRouteItem.HasDescription(const AName: string): Boolean;
begin
  Result := (Descriptions.GetItemByName(AName) <> nil);
end;

function TRouteItem.GetDescription(const AName: string): TRouteItemDescription;
begin
  Result := Descriptions.GetItemByName(AName);
end;

procedure TRouteItem.AddDescription(const AName: string;
  ADescription: TRouteItemDescription);
begin
  Descriptions.AddItem(AName, ADescription);
end;

{ TRouteItemList }

procedure TRouteItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TRouteItem(Ptr).Free();
end;

function TRouteItemList.GetItem(AIndex: Integer): TRouteItem;
begin
  Result := TRouteItem(Get(AIndex));
end;

function TRouteItemList.AddItem(ADatabaseId: TMapDatabaseId;
  ACurrentNodeIndex: Integer; const AObjects: TObjectFileRefArray;
  const APathObject: TObjectFileRef; ATargetNodeIndex: Integer): Integer;
var
  TmpItem: TRouteItem;
begin
  TmpItem := TRouteItem.Create(ADatabaseId, ACurrentNodeIndex, AObjects, APathObject, ATargetNodeIndex);
  Result := Add(TmpItem);
end;

{ TRouteDescription }

procedure TRouteDescription.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TRouteItemList.Create();
  FDatabaseMapping := TStringList.Create(); // DatabaseId : string
end;

procedure TRouteDescription.BeforeDestruction;
begin
  FreeAndNil(FDatabaseMapping);
  FreeAndNil(FNodes);
  inherited BeforeDestruction;
end;

procedure TRouteDescription.Clear();
begin
  FNodes.Clear();
end;

function TRouteDescription.IsEmpty: Boolean;
begin
  Result := (FNodes.Count = 0);
end;

procedure TRouteDescription.AddNode(ADatabaseId: TMapDatabaseId;
  ACurrentNodeIndex: Integer; const AObjects: TObjectFileRefArray;
  const APathObject: TObjectFileRef; ATargetNodeIndex: Integer);
begin
  FNodes.AddItem(ADatabaseId, ACurrentNodeIndex, AObjects, APathObject, ATargetNodeIndex);
end;

{ TRouteNode }

procedure TRouteNode.Init(const AFileOffset: TFileOffset;
  const APoint: TGeoPoint);
begin
  FFileOffset := AFileOffset;
  FPoint.Assign(APoint);
end;

function TRouteNode.AddObject(const AObj: TObjectFileRef;
  AObjVariantIndex: Integer): Integer;
begin
  Result := Length(Objects);
  SetLength(Objects, Result+1);
  Objects[Result].Obj := AObj;
  Objects[Result].ObjVariantIndex := AObjVariantIndex;
end;

procedure TRouteNode.Read(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
var
  ObjectCount, PathCount, ExcludesCount, btMask: Byte;
  PrevFileOffset: TId;
  i: Integer;
  RefType: TRefType;
  iFileOffset: TFileOffset;
  iDistanceValue: LongWord;
begin
  FFileOffset := AScanner.Stream.Position;

  AScanner.Read(FSerial);
  FPoint.ReadFromStream(AScanner.Stream);

  AScanner.Read(ObjectCount);
  AScanner.Read(PathCount);
  AScanner.Read(ExcludesCount);

  SetLength(Objects, ObjectCount);

  PrevFileOffset := 0;

  for i := 0 to ObjectCount-1 do
  begin
    AScanner.ReadNumber(iFileOffset);

    if (iFileOffset mod 2) = 0 then
      RefType := refWay
    else
      RefType := refArea;

    iFileOffset := iFileOffset div 2;
    Inc(iFileOffset, PrevFileOffset);

    Objects[i].Obj.SetValue(iFileOffset, RefType);

    AScanner.Read(Objects[i].ObjVariantIndex);

    PrevFileOffset := iFileOffset;
  end;

  SetLength(Paths, PathCount);

  for i := 0 to PathCount-1 do
  begin
    AScanner.Read(Paths[i].Id);
    AScanner.Read(Paths[i].ObjectIndex);
    //AScanner.Read(Paths[i].bearing);
    AScanner.Read(btMask);
    AScanner.ReadNumber(iDistanceValue);
    Paths[i].Flags := VehicleMaskToTypes(btMask);
    Paths[i].Distance := (iDistanceValue div 10);
  end;

  SetLength(Excludes, ExcludesCount);
  for i := 0 to ExcludesCount-1 do
  begin
    AScanner.Read(Excludes[i].Source);
    AScanner.Read(Excludes[i].TargetIndex);
  end;
end;

procedure TRouteNode.Write(AWriter: TFileWriter);
var
  LastFileOffset: TFileOffset;
  i: Integer;
  offset: TFileOffset;
begin
  AWriter.Write(Serial);
  Point.WriteToStream(AWriter.Stream);

  Assert(Length(Paths) <= High(Byte));
  Assert(Length(Excludes) <= High(Byte));

  AWriter.Write(Byte(Length(Objects)));
  AWriter.Write(Byte(Length(Paths)));
  AWriter.Write(Byte(Length(Excludes)));

  LastFileOffset := 0;

  for i := 0 to Length(Objects)-1 do
  begin
    offset := Objects[i].Obj.Offset - LastFileOffset;

    if (Objects[i].Obj.RefType = refWay) then
      offset := offset * 2
    else if (Objects[i].Obj.RefType = refArea) then
      offset := offset * 2 + 1
    else
      Assert(False);

    AWriter.WriteNumber(offset);
    AWriter.Write(Objects[i].ObjVariantIndex);

    LastFileOffset := Objects[i].Obj.Offset;
  end;

  for i := 0 to Length(Paths)-1 do
  begin
    AWriter.Write(Paths[i].Id);
    AWriter.Write(Paths[i].ObjectIndex);
    //AWriter.Write(Paths[i].bearing);
    AWriter.Write(VehicleTypesToMask(Paths[i].Flags));
    AWriter.WriteNumber(LongWord(Floor(Paths[i].Distance / 10 + 0.5)));
  end;

  for i := 0 to Length(Excludes)-1 do
  begin
    AWriter.Write(Excludes[i].Source);
    AWriter.Write(Excludes[i].TargetIndex);
  end;
end;

{ TRouteEntry }

procedure TRouteEntry.Init(ADatabaseId: TMapDatabaseId; ACurNodeId: TId;
  ACurNodeIndex: Integer; const APathObject: TObjectFileRef;
  ATargetNodeIndex: Integer);
begin
  DatabaseId := ADatabaseId;
  CurNodeId := ACurNodeId;
  CurNodeIndex := ACurNodeIndex;
  PathObject := APathObject;
  TargetNodeIndex := ATargetNodeIndex;
end;

function TRouteEntry.GetDBFileOffset(): TMapDBFileOffset;
begin
  Result.Init(DatabaseId, PathObject.Offset);
end;

{ TRouteData }

procedure TRouteData.Clear();
begin
  SetLength(Entries, 0);
end;

procedure TRouteData.AddEntry(ADatabaseId: TMapDatabaseId; ACurNodeId: TId;
  ACurNodeIndex: Integer; const APathObject: TObjectFileRef;
  ATargetNodeIndex: Integer);
var
  n: Integer;
begin
  n := Length(Entries);
  SetLength(Entries, n+1);
  Entries[n].Init(ADatabaseId, ACurNodeId, ACurNodeIndex, APathObject, ATargetNodeIndex);
end;

procedure TRouteData.Append(ARoutePart: TRouteData);
var
  i, n: Integer;
begin
  n := Length(ARoutePart.Entries);
  for i := 0 to n-1 do
  begin
    AddEntry(
      ARoutePart.Entries[i].DatabaseId,
      ARoutePart.Entries[i].CurNodeId,
      ARoutePart.Entries[i].CurNodeIndex,
      ARoutePart.Entries[i].PathObject,
      ARoutePart.Entries[i].TargetNodeIndex);
  end;
end;

procedure TRouteData.PopEntry();
var
  n: Integer;
begin
  n := Length(Entries);
  if n > 0 then
    SetLength(Entries, n-1);
end;

function TRouteData.IsEmpty(): Boolean;
begin
  Result := (Length(Entries) > 0);
end;

{ TObjectVariantDataFile }

procedure TObjectVariantDataFile.Init();
begin
  SetLength(Data, 0);
  IsLoaded := False;
  Filename := '';
end;

function TObjectVariantDataFile.Load(ATypeConfig: TTypeConfig;
  AFilename: string): Boolean;
var
  Scanner: TFileScanner;
  i, iDataCount: Integer;
begin
  Init();
  Filename := AFilename;

  Scanner := TFileScanner.Create();
  try
    Scanner.Open(Filename, fsmSequential, True);

    Scanner.Read(iDataCount);
    SetLength(Data, iDataCount);
    for i := 0 to iDataCount-1 do
    begin
      Data[i].Read(ATypeConfig, Scanner);
    end;
    Scanner.Close();
    IsLoaded := True;
  finally
    Scanner.Free();
  end;
  Result := IsLoaded;
end;

procedure TObjectVariantData.Read(ATypeConfig: TTypeConfig;
  AScanner: TFileScanner);
var
  //typeId: TTypeId;
  typeId: LongWord;
begin
  AScanner.ReadNumber(typeId);
  TypeInfo := ATypeConfig.GetTypeInfo(TTypeId(typeId));

  AScanner.Read(MaxSpeed);
  AScanner.Read(Grade);
end;

procedure TObjectVariantData.Write(AWriter: TFileWriter);
begin
  AWriter.WriteNumber(LongWord(TypeInfo.Index));
  AWriter.Write(MaxSpeed);
  AWriter.Write(Grade);
end;

end.

