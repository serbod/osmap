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
routing\route
  Description -> TRouteItemDescription
  Node -> TRouteItem
  RouteDescription -> TRouteDescription

*)
unit OsMapRouting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsMapTypes, OsMapGeometry;

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
    procedure SetPOIAtRoute(ADatabaseID: Int64; AObject: Pointer; AName: string);
  end;

  TRouteItemDescriptionList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRouteItemDescription;
  end;

  TRouteItem = class
  private
    FDatabaseId: TDatabaseId;
    FCurrentNodeIndex: Integer;
    FObjects: array of TObjectFileRef;
    FPathObject: TObjectFileRef;
    FTargetNodeIndex: Integer;
    FDistance: TDistance;  // distance from route start (meters)
    FTime: TDateTime;   // time from route start
    FLocation: TGeoPoint;
    //FDescriptionMap: std::unordered_map<std::string,DescriptionRef> ;
    FDescriptions: TRouteItemDescriptionList;
  public
    constructor Create(ADatabaseId: TDatabaseId;
      ACurrentNodeIndex: Integer;
      const AObjects: array of TObjectFileRef;
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
    property Objects: array of TObjectFileRef read FObjects;
    { Return a list of descriptions attached to the current node }
    property Descriptions: TRouteItemDescriptionList read FDescriptions;

    property DatabaseId: TDatabaseId read FDatabaseId;
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

  TRouteItemList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRouteItem;
  end;

  TRouteDescription = class
  private
    FNodes: TRouteItemList;
    FDatabaseMapping: TStringList; // DatabaseId : string

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear();
    function IsEmpty: Boolean;

    procedure AddNode(ADatabaseId: TDatabaseId;
      ACurrentNodeIndex: Integer;
      const AObjects: array of TObjectFileRef;
      const APathObject: TObjectFileRef;
      ATargetNodeIndex: Integer);

    property Nodes: TRouteItemList read FNodes;
    property DatabaseMapping: TStringList read FDatabaseMapping;
  end;


implementation

uses OsMapUtils;

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
  Result := '';
  for i := Low(Values) to High(Values) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Values[i];
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
  //Values[2] := ATarget;
  //Values[3] := ATarget;
end;

function TRouteItemDescription.GetTurnAngle(): Double;
begin

end;

function TRouteItemDescription.GetCurveAngle(): Double;
begin

end;

function TRouteItemDescription.GetTurn(): string;
begin

end;

function TRouteItemDescription.GetCurve(): string;
begin

end;

procedure TRouteItemDescription.SetRoundaboutLeave(AExitCount: Integer);
begin

end;

procedure TRouteItemDescription.SetMotorwayChange(const AOrigin,
  ATarget: string);
begin

end;

procedure TRouteItemDescription.SetMaxSpeed(AValue: Integer);
begin

end;

procedure TRouteItemDescription.SetPOIAtRoute(ADatabaseID: Int64;
  AObject: Pointer; AName: string);
begin

end;

end.

