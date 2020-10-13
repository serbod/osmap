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
  Intersection
  JunctionRef -> TIntersection

routing/DBFileOffset
  DBId -> TMapDBId
  DBFileOffset -> TMapDBFileOffset

routing/route
  Description -> TRouteItemDescription
  Node -> TRouteItem
  RouteDescription -> TRouteDescription

routing/RouteNode
  ObjectVariantData
  ObjectData -> TRouteNodeObjectData
  Exclude -> TRouteNodeExclude
  Path -> TRouteNodePath
  RouteNode -> TRouteNode

routing/RouteData
  RouteData
  RouteEntry

routing/RoutingProfile
  RoutingProfile -> TAbstractRoutingProfile
  AbstractRoutingProfile -> TRoutingProfile
  ShortestPathRoutingProfile -> TRoutingProfile
  FastestPathRoutingProfile

*)
unit OsMapRouting;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fgl,
  {$endif}
  OsMapTypes, OsMapGeometry, OsMapUtils, OsMapObjTypes, OsMapObjects,
  OsMapObjFeatures, OsMapFiles;

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

    procedure Init(ADatabaseId: TMapDatabaseId = 0; AId: TId = 0);
    function IsValid(): Boolean;
    function IsEqual(const AValue: TMapDBId): Boolean;
    function AsStr(): string;
  end;

  TMapDBIdArray = array of TMapDBId;

  TMapDatabaseIdMap = specialize TFPGMap<TMapDatabaseId, string>; // DatabaseId : string

  { TMapDBFileOffset }

  TMapDBFileOffset = object
    DatabaseId: TMapDatabaseId;
    Offset: TFileOffset;

    procedure Init(ADatabaseId: TMapDatabaseId; AOffset: TFileOffset);
    function AsStr(): string;
    function IsEqual(const AValue: TMapDBFileOffset): Boolean;
  end;

  TMapDBFileOffsetArray = array of TMapDBFileOffset;

  { A Intersection is a node, where multiple routeable ways or areas
    meet. }

  { TIntersection }

  TIntersection = class(TMapObject)
  public
    // The id/file offset of the node where the ways meet
    NodeId: TId;
    // The objects that meet at the given node
    Objects: TObjectFileRefArray;

    procedure Init();

    function ReadData(AScanner: TFileScanner): Boolean;
    procedure Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner); override;
  end;

  TIntersectionArray = array of TIntersection;
  TIntersectionMapById = specialize TFPGMap<TId, TIntersection>; // Id : JunctionRef

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
    function AngleToDirectionStr(AAngle: TReal): string;

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
    procedure SetDirection(ATurnAngle, ACurveAngle: TReal);
    function GetTurnAngle(): TReal;
    function GetCurveAngle(): TReal;
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

  protected
    // assigned
    FDatabaseMapping: TMapDatabaseIdMap; // DatabaseId : string

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
    property DatabaseMapping: TMapDatabaseIdMap read FDatabaseMapping write FDatabaseMapping;
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

  TObjectVariantDataArray = array of TObjectVariantData;

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

  { TRouteNodePath }

  TRouteNodePath = object
    Distance: TDistance;  // Distance from the current route node to the target route node
    Id: TId;              // id of the targeting route node
    ObjectIndex: Integer; // The index of the way to use from this route node to the target route node
    Flags: TVehicleTypes; // Certain flags

    // AVehicle in Flags
    function IsRestricted(AVehicle: TVehicleType): Boolean;
  end;

  { TRouteNode }

  TRouteNode = class
  private
    FFileOffset: TFileOffset;
    FPoint: TGeoPoint;
    FSerial: Byte;
  public
    Objects: array of TRouteNodeObjectData;
    Paths: array of TRouteNodePath;
    Excludes: array of TRouteNodeExclude;

    procedure Create(const AFileOffset: TFileOffset; const APoint: TGeoPoint);

    function AddObject(const AObj: TObjectFileRef; AObjVariantIndex: Integer): Integer;
    function GetId(): TId; // Point.GetId()
    //function GetCoord(): TGeoPoint; // Point.GetCoord()

    function IsRelevant(): Boolean;  // Point.IsRelevant()

    procedure Read(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
    procedure Write(AWriter: TFileWriter);

    // FileOffset of the route node
    property FileOffset: TFileOffset read FFileOffset;
    // Coordinate and id of the route node
    property Point: TGeoPoint read FPoint;
    property Serial: Byte read FSerial;
  end;

  TRouteNodeList =  specialize TFPGList<TRouteNode>;
  TRouteNodeMapById = specialize TFPGMap<TId, TRouteNode>; // Id : RouteNode

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

  { Abstract interface for a routing profile. A routing profile decides about the costs
    of taking a certain way. It thus may hold information about how fast ways can be used,
    maximum speed of the traveling device etc... }
  TAbstractRoutingProfile = class
  public
    function GetVehicle(): TVehicleType; virtual; abstract;
    function GetCostLimitDistance(): TDistance; virtual; abstract;
    function GetCostLimitFactor(): TReal; virtual; abstract;

    function CanUse(const ACurrentNode: TRouteNode;
      const AObjectVariantDataArr: TObjectVariantDataArray;
      APathIndex: Integer): Boolean; virtual; abstract;
    function CanUse(AArea: TMapArea): Boolean; virtual; abstract; overload;
    function CanUse(AWay: TMapWay): Boolean; virtual; abstract; overload;
    function CanUseForward(AWay: TMapWay): Boolean; virtual; abstract;
    function CanUseBackward(AWay: TMapWay): Boolean; virtual; abstract;

    function GetCosts(const ACurrentNode: TRouteNode;
      const AObjectVariantDataArr: TObjectVariantDataArray;
      APathIndex: Integer): TReal; virtual; abstract; overload;
    function GetCosts(AArea: TMapArea; const ADistance: TDistance): TReal; virtual; abstract; overload;
    function GetCosts(AWay: TMapWay; const ADistance: TDistance): TReal; virtual; abstract; overload;
    function GetCosts(const ADistance: TDistance): TReal; virtual; abstract; overload;

    function GetTime(AArea: TMapArea; const ADistance: TDistance): TDateTime; virtual; abstract;
    function GetTime(AWay: TMapWay; const ADistance: TDistance): TDateTime; virtual; abstract; overload;
  end;

  TSpeedMap = specialize TFPGMap<string, TReal>;

  { Common base class for our concrete profile instantiations. Offers a number of profile
    type independent interface implementations and helper methods. }

  { TRoutingProfile }

  TRoutingProfile = class(TAbstractRoutingProfile)
  protected
    FTypeConfig: TTypeConfig;
    //FAccessReader: TFeatureValueReader;
    //FMaxSpeedReader: TFeatureValueReader;
    FVehicle: TVehicleType;
    FVehicleRouteNodeMask: TVehicleMask;
    FCostLimitDistance: TDistance;
    FCostLimitFactor: TReal;
    FSpeeds: array of TReal;
    FMinSpeed: TReal;
    FMaxSpeed: TReal;
    FVehicleMaxSpeed: TReal;

    procedure SetVehicle(const AValue: TVehicleType);
  public
    constructor Create(ATypeConfig: TTypeConfig);

    function GetVehicle(): TVehicleType; override;
    function GetCostLimitDistance(): TDistance; override;
    function GetCostLimitFactor(): TReal; override;

    procedure ParametrizeForFoot(ATypeConfig: TTypeConfig; AMaxSpeed: TReal);
    procedure ParametrizeForBicycle(ATypeConfig: TTypeConfig; AMaxSpeed: TReal);
    function ParametrizeForCar(ATypeConfig: TTypeConfig;
      const ASpeedMap: TSpeedMap;
      AMaxSpeed: TReal): Boolean;

    procedure AddType(const AType: TTypeInfo; ASpeed: TReal);

    function CanUse(const ACurrentNode: TRouteNode;
      const AObjectVariantDataArr: TObjectVariantDataArray;
      APathIndex: Integer): Boolean; override;
    function CanUse(AArea: TMapArea): Boolean; override; overload;
    function CanUse(AWay: TMapWay): Boolean; override; overload;
    function CanUseForward(AWay: TMapWay): Boolean; override;
    function CanUseBackward(AWay: TMapWay): Boolean; override;

    function GetTime(AArea: TMapArea; const ADistance: TDistance): TDateTime; override;
    function GetTime(AWay: TMapWay; const ADistance: TDistance): TDateTime; override; overload;

    { for shortest path by default }
    function GetCosts(const ACurrentNode: TRouteNode;
      const AObjectVariantDataArr: TObjectVariantDataArray;
      APathIndex: Integer): TReal; override;
    function GetCosts(AArea: TMapArea; const ADistance: TDistance): TReal; override; overload;
    function GetCosts(AWay: TMapWay; const ADistance: TDistance): TReal; override; overload;
    function GetCosts(const ADistance: TDistance): TReal; override; overload;

    property Vehicle: TVehicleType read GetVehicle write SetVehicle;
    property VehicleMaxSpeed: TReal read FVehicleMaxSpeed write FVehicleMaxSpeed;
    { static distance value added to the maximum cost }
    property CostLimitDistance: TDistance read GetCostLimitDistance write FCostLimitDistance;

    { The router tries to minimize the actual costs of the route. There is a lower limit
      defined by GetCosts(double distance). Applying the given factor to the minimal cost
      results in a upper limit for the costs.

      Increasing the factor results in the router trying harder to find a route by looking for
      bigger and even bigger detours, decreasing the factor result in the router either finding a rather direct
      route or none. Setting the factor below 1.0 should result in the router not finding any route at all.

      If there is a router the current router will find it and the router will look for the optimal route first.
      So, if there is a route the limit could be set to std::limits<double>::max(). If there is no route though
      the limit will stop the router to search for all possible detours, walking the whole graph in the end.
      Since this might take for ever the limit should be reasonable high.

      The actual maximum cost limit is calculated based on a constant limit distance (default 10.0 Km)
      and a cost factor applied to the minimum costs 8default 5.0).

      So the resulting maxium cost are profile.GetCosts(profile.GetCostLimitDistance())+
      profile.GetCosts(distance)*profile.GetCostLimitFactor(). }
    property CostLimitFactor: TReal read GetCostLimitFactor write FCostLimitFactor;
  end;

  { Profile that defines costs base of the time the traveling device needs
    for a certain way resulting in the fastest path chosen (cost=distance/speedForWayType). }

  { TFastestPathRoutingProfile }

  TFastestPathRoutingProfile = class(TRoutingProfile)
  public
    function GetCosts(const ACurrentNode: TRouteNode;
      const AObjectVariantDataArr: TObjectVariantDataArray;
      APathIndex: Integer): TReal; override;
    function GetCosts(AArea: TMapArea; const ADistance: TDistance): TReal; override; overload;
    function GetCosts(AWay: TMapWay; const ADistance: TDistance): TReal; override; overload;
    function GetCosts(const ADistance: TDistance): TReal; override; overload;
  end;


  function MapDBId(ADatabaseId: TMapDatabaseId; AId: TId): TMapDBId;

  function MapDBFileOffset(ADatabaseId: TMapDatabaseId; AOffset: TFileOffset): TMapDBFileOffset;

  function MapDBIdArrayAppend(var AArray: TMapDBIdArray; const AValue: TMapDBId): Integer;

  function MapDBFileOffsetArrayAppend(var AArray: TMapDBFileOffsetArray; const AValue: TMapDBFileOffset): Integer;

implementation

uses Math;

function MapDBId(ADatabaseId: TMapDatabaseId; AId: TId): TMapDBId;
begin
  Result.DatabaseId := ADatabaseId;
  Result.Id := AId;
end;

function MapDBFileOffset(ADatabaseId: TMapDatabaseId; AOffset: TFileOffset): TMapDBFileOffset;
begin
  Result.DatabaseId := ADatabaseId;
  Result.Offset := AOffset;
end;

function MapDBIdArrayAppend(var AArray: TMapDBIdArray; const AValue: TMapDBId): Integer;
begin
  Result := Length(AArray);
  SetLength(AArray, Result + 1);
  AArray[Result] := AValue;
end;

function MapDBFileOffsetArrayAppend(var AArray: TMapDBFileOffsetArray; const AValue: TMapDBFileOffset): Integer;
begin
  Result := Length(AArray);
  SetLength(AArray, Result + 1);
  AArray[Result] := AValue;
end;

{ TMapDBId }

procedure TMapDBId.Init(ADatabaseId: TMapDatabaseId; AId: TId);
begin
  DatabaseId := ADatabaseId;
  Id := AId;
end;

function TMapDBId.IsValid(): Boolean;
begin
  Result := (Id <> 0);
end;

function TMapDBId.IsEqual(const AValue: TMapDBId): Boolean;
begin
  Result := (AValue.Id = Id) and (AValue.DatabaseId = DatabaseId);
end;

function TMapDBId.AsStr(): string;
begin
  Result := Format('%d:%d', [DatabaseId, Id]);
end;

{ TMapDBFileOffset }

procedure TMapDBFileOffset.Init(ADatabaseId: TMapDatabaseId;
  AOffset: TFileOffset);
begin
  DatabaseId := ADatabaseId;
  Offset := AOffset;
end;

function TMapDBFileOffset.AsStr(): string;
begin
  Result := IntToHex(Offset, 2) + ':' + IntToHex(DatabaseId, 2);
end;

function TMapDBFileOffset.IsEqual(const AValue: TMapDBFileOffset): Boolean;
begin
  Result := (AValue.Offset = Offset) and (AValue.DatabaseId = DatabaseId);
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

function TRouteItemDescription.AngleToDirectionStr(AAngle: TReal): string;
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

procedure TRouteItemDescription.SetDirection(ATurnAngle, ACurveAngle: TReal);
begin
  ItemType := riDirection;
  if Length(Values) < 4 then
    SetLength(Values, 4);
  Values[0] := FloatToStr(ATurnAngle);
  Values[1] := FloatToStr(ACurveAngle);
  Values[2] := AngleToDirectionStr(ATurnAngle);
  Values[3] := AngleToDirectionStr(ACurveAngle);
end;

function TRouteItemDescription.GetTurnAngle(): TReal;
begin
  Result := StrToFloatDef(GetValue(0), 0);
end;

function TRouteItemDescription.GetCurveAngle(): TReal;
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
  //FDatabaseMapping := TMapDatabaseIdMap.Create(); // DatabaseId : string
end;

procedure TRouteDescription.BeforeDestruction;
begin
  //FreeAndNil(FDatabaseMapping);
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

procedure TRouteNode.Create(const AFileOffset: TFileOffset;
  const APoint: TGeoPoint);
begin
  inherited Create();
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

function TRouteNode.GetId(): TId;
begin
  Result := FPoint.GetId();
end;

function TRouteNode.IsRelevant(): Boolean;
begin
  Result := (Serial <> 0);
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

{ TObjectVariantData }

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

{ TRouteNodePath }

function TRouteNodePath.IsRestricted(AVehicle: TVehicleType): Boolean;
begin
  Result := AVehicle in Flags;
end;

{ TRoutingProfile }

function TRoutingProfile.GetCostLimitDistance(): TDistance;
begin
  Result := FCostLimitDistance;
end;

function TRoutingProfile.GetCostLimitFactor(): TReal;
begin
  Result := FCostLimitFactor;
end;

function TRoutingProfile.GetVehicle(): TVehicleType;
begin
  Result := FVehicle;
end;

procedure TRoutingProfile.SetVehicle(const AValue: TVehicleType);
begin
  FVehicle := AValue;
  case FVehicle of
    vehicleFoot: FVehicleRouteNodeMask := VEHICLE_FOOT;
    vehicleBicycle: FVehicleRouteNodeMask := VEHICLE_BICYCLE;
    vehicleCar: FVehicleRouteNodeMask := VEHICLE_CAR;
  end;
end;

constructor TRoutingProfile.Create(ATypeConfig: TTypeConfig);
begin
  FTypeConfig := ATypeConfig;
  //FAccessReader.;
  //FMaxSpeedReader;
  FVehicle := vehicleCar;
  FVehicleRouteNodeMask := VEHICLE_CAR;
  FCostLimitDistance := 10000.0; // 10 km
  FCostLimitFactor := 5.0;
  FMinSpeed := 0.0;
  FMaxSpeed := 0.0;
  FVehicleMaxSpeed := MaxDouble;
end;

procedure TRoutingProfile.ParametrizeForFoot(ATypeConfig: TTypeConfig;
  AMaxSpeed: TReal);
var
  ti: TTypeInfo;
begin
  SetLength(FSpeeds, 0);
  Vehicle := vehicleFoot;
  VehicleMaxSpeed := AMaxSpeed;
  for ti in FTypeConfig.Types do
  begin
    if (not ti.IsIgnore) and (ti.CanRouteFoot) then
      AddType(ti, AMaxSpeed);
  end;
end;

procedure TRoutingProfile.ParametrizeForBicycle(ATypeConfig: TTypeConfig;
  AMaxSpeed: TReal);
var
  ti: TTypeInfo;
begin
  SetLength(FSpeeds, 0);
  Vehicle := vehicleBicycle;
  VehicleMaxSpeed := AMaxSpeed;
  for ti in FTypeConfig.Types do
  begin
    if (not ti.IsIgnore) and (ti.CanRouteBicycle) then
      AddType(ti, AMaxSpeed);
  end;
end;

function TRoutingProfile.ParametrizeForCar(ATypeConfig: TTypeConfig;
  const ASpeedMap: TSpeedMap; AMaxSpeed: TReal): Boolean;
var
  ti: TTypeInfo;
  speed: TReal;
begin
  Result := True;
  SetLength(FSpeeds, 0);
  Vehicle := vehicleCar;
  VehicleMaxSpeed := AMaxSpeed;
  for ti in FTypeConfig.Types do
  begin
    if (not ti.IsIgnore) and (ti.CanRouteCar) then
    begin
      if ASpeedMap.TryGetData(ti.TypeName, speed) then
        AddType(ti, speed)
      else
      begin
        Result := False;
        WriteLn(LogFile, 'ERROR: No speed for type=', ti.TypeName, 'defined!');
        Continue;
      end;
    end;
  end;
end;

procedure TRoutingProfile.AddType(const AType: TTypeInfo; ASpeed: TReal);
var
  n, i: Integer;
begin
  n := Length(FSpeeds);
  if n = 0 then
  begin
    FMinSpeed := ASpeed;
    FMaxSpeed := ASpeed;
  end
  else
  begin
    FMinSpeed := Min(FMinSpeed, ASpeed);
    FMaxSpeed := Max(FMaxSpeed, ASpeed);
  end;

  if (AType.Index >= n) then
  begin
    SetLength(FSpeeds, AType.Index + 1);
    for i := n to Length(FSpeeds)-1 do
      FSpeeds[i] := 0.0;
  end;

  FSpeeds[AType.Index] := ASpeed;
end;

function TRoutingProfile.CanUse(const ACurrentNode: TRouteNode;
  const AObjectVariantDataArr: TObjectVariantDataArray;
  APathIndex: Integer): Boolean;
var
  objIndex, typeIndex: Integer;
  ti: TTypeInfo;
begin

  if (Vehicle in ACurrentNode.Paths[APathIndex].Flags) then
  begin
    objIndex := ACurrentNode.Paths[APathIndex].ObjectIndex;
    ti := AObjectVariantDataArr[ACurrentNode.Objects[objIndex].ObjVariantIndex].TypeInfo;
    typeIndex := ti.Index;

    Result := (typeIndex < Length(FSpeeds)) and (FSpeeds[typeIndex] > 0.0);
  end
  else
    Result := False
end;

function TRoutingProfile.CanUse(AArea: TMapArea): Boolean;
var
  typeIndex: Integer;
begin
  if Length(AArea.Rings) = 1 then
  begin
    typeIndex := AArea.rings[0].GetType().Index;

    Result := (typeIndex < Length(FSpeeds)) and (FSpeeds[typeIndex] > 0.0);
  end
  else
    Result := False;
end;

function TRoutingProfile.CanUse(AWay: TMapWay): Boolean;
var
  typeIndex: Integer;
  accessValue: Byte;
begin
  typeIndex := AWay.GetType().Index;

  Result := (typeIndex < Length(FSpeeds)) and (FSpeeds[typeIndex] > 0.0);
  if not Result then
    Exit;

  accessValue := StrToIntDef(AWay.FeatureValueBuffer.GetFeatureValue(ftAccess), 0);

  if (accessValue <> 0) then
    Result := TAccessFeature.CanRoute(accessValue, Vehicle)
  else
    Result := AWay.GetType().CanRouteBy(Vehicle);
end;

function TRoutingProfile.CanUseForward(AWay: TMapWay): Boolean;
var
  typeIndex: Integer;
  accessValue: Byte;
begin
  typeIndex := AWay.GetType().Index;

  Result := (typeIndex < Length(FSpeeds)) and (FSpeeds[typeIndex] > 0.0);
  if not Result then
    Exit;

  accessValue := StrToIntDef(AWay.FeatureValueBuffer.GetFeatureValue(ftAccess), 0);

  if (accessValue <> 0) then
    Result := TAccessFeature.CanRouteForward(accessValue, Vehicle)
  else
    Result := AWay.GetType().CanRouteBy(Vehicle);
end;

function TRoutingProfile.CanUseBackward(AWay: TMapWay): Boolean;
var
  typeIndex: Integer;
  accessValue: Byte;
begin
  typeIndex := AWay.GetType().Index;

  Result := (typeIndex < Length(FSpeeds)) and (FSpeeds[typeIndex] > 0.0);
  if not Result then
    Exit;

  accessValue := StrToIntDef(AWay.FeatureValueBuffer.GetFeatureValue(ftAccess), 0);

  if (accessValue <> 0) then
    Result := TAccessFeature.CanRouteBackward(accessValue, Vehicle)
  else
    Result := AWay.GetType().CanRouteBy(Vehicle);
end;

function TRoutingProfile.GetTime(AArea: TMapArea;
  const ADistance: TDistance): TDateTime;
var
  speed: TReal;
begin
  speed := FSpeeds[AArea.GetType().Index];

  speed := Min(FVehicleMaxSpeed, speed);

  if speed <> 0.0 then
    Result := ((ADistance / 1000) / speed) / HoursPerDay
  else
    Result := 0.0;
end;

function TRoutingProfile.GetTime(AWay: TMapWay;
  const ADistance: TDistance): TDateTime;
var
  speed: TReal;
  maxSpeedValue: Byte;
begin
  maxSpeedValue := StrToIntDef(AWay.FeatureValueBuffer.GetFeatureValue(ftMaxSpeed), 0);
  if maxSpeedValue <> 0 then
    speed := maxSpeedValue
  else
    speed := FSpeeds[AWay.GetType().Index];

  speed := Min(FVehicleMaxSpeed, speed);

  if speed <> 0.0 then
    Result := ((ADistance / 1000) / speed) / HoursPerDay
  else
    Result := 0.0;
end;

function TRoutingProfile.GetCosts(const ACurrentNode: TRouteNode;
  const AObjectVariantDataArr: TObjectVariantDataArray;
  APathIndex: Integer): TReal;
begin
  Result := ACurrentNode.Paths[APathIndex].Distance / 1000;
end;

function TRoutingProfile.GetCosts(AArea: TMapArea;
  const ADistance: TDistance): TReal;
begin
  Result := ADistance / 1000;
end;

function TRoutingProfile.GetCosts(AWay: TMapWay;
  const ADistance: TDistance): TReal;
begin
  Result := ADistance / 1000;
end;

function TRoutingProfile.GetCosts(const ADistance: TDistance): TReal;
begin
  Result := ADistance / 1000;
end;

{ TFastestPathRoutingProfile }

function TFastestPathRoutingProfile.GetCosts(const ACurrentNode: TRouteNode;
  const AObjectVariantDataArr: TObjectVariantDataArray;
  APathIndex: Integer): TReal;
var
  speed: TReal;
  objIndex: Integer;
  ti: TTypeInfo;
begin
  //Result := inherited GetCosts(ACurrentNode, AObjectVariantDataArr, APathIndex);
  objIndex := ACurrentNode.Paths[APathIndex].ObjectIndex;

  if (AObjectVariantDataArr[ACurrentNode.Objects[objIndex].ObjVariantIndex].MaxSpeed > 0) then
    speed := AObjectVariantDataArr[ACurrentNode.Objects[objIndex].ObjVariantIndex].MaxSpeed
  else
  begin
    ti := AObjectVariantDataArr[ACurrentNode.Objects[objIndex].ObjVariantIndex].TypeInfo;

    speed := FSpeeds[ti.Index];
  end;

  speed := Min(VehicleMaxSpeed, speed);

  if speed <> 0.0 then
    Result := ((ACurrentNode.Paths[APathIndex].Distance / 1000) / speed)
  else
    Result := 0.0;
end;

function TFastestPathRoutingProfile.GetCosts(AArea: TMapArea;
  const ADistance: TDistance): TReal;
var
  speed: TReal;
begin
  speed := FSpeeds[AArea.GetType().Index];

  speed := Min(FVehicleMaxSpeed, speed);

  if speed <> 0.0 then
    Result := ((ADistance / 1000) / speed)
  else
    Result := 0.0;
end;

function TFastestPathRoutingProfile.GetCosts(AWay: TMapWay;
  const ADistance: TDistance): TReal;
var
  speed: TReal;
  maxSpeedValue: Byte;
begin
  maxSpeedValue := StrToIntDef(AWay.FeatureValueBuffer.GetFeatureValue(ftMaxSpeed), 0);
  if maxSpeedValue <> 0 then
    speed := maxSpeedValue
  else
    speed := FSpeeds[AWay.GetType().Index];

  speed := Min(FVehicleMaxSpeed, speed);

  if speed <> 0.0 then
    Result := ((ADistance / 1000) / speed)
  else
    Result := 0.0;
end;

function TFastestPathRoutingProfile.GetCosts(const ADistance: TDistance): TReal;
var
  speed: TReal;
begin
  speed := Min(FVehicleMaxSpeed, FMaxSpeed);

  if speed <> 0.0 then
    Result := ((ADistance / 1000) / speed)
  else
    Result := 0.0;
end;

{ TIntersection }

procedure TIntersection.Init();
begin
  NodeId := 0;
  SetLength(Objects, 0);
end;

function TIntersection.ReadData(AScanner: TFileScanner): Boolean;
var
  i: Integer;
  objectCount: LongWord;
  objectFileRefReader: TObjectFileRefStreamReader;
begin
  try
    AScanner.ReadNumber(NodeId);
    AScanner.ReadNumber(objectCount);
    SetLength(Objects, objectCount);

    objectFileRefReader.Init(AScanner);

    for i := 0 to objectCount-1 do
      objectFileRefReader.Read(Objects[i]);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'ERROR: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TIntersection.Read(const ATypeConfig: TTypeConfig;
  AScanner: TFileScanner);
begin
  ReadData(AScanner);
end;

end.

