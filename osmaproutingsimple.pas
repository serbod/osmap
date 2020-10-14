(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2020  Sergey Bodrov

  This source is ported from libosmscout library
  Copyright (C) 2012  Tim Teulings
  Copyright (C) 2017  Lukas Karas

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
  OsMap simple routing service
routing\SimpleRoutingService
  ClosestRoutableObjectResult
  SimpleRoutingService
*)

unit OsMapRoutingSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OsMapTypes, OsMapObjTypes, OsMapGeometry, OsMapObjFeatures,
  OsMapRouting, OsMapRoutingService, OsMapRoutingDatabase, OsMapDatabase,
  OsMapDataFiles, OsMapObjects;

type
  TClosestRoutableObjectResult = record
    ObjectFileRef: TObjectFileRef;
    Distance: TDistance;
    WayRef: TMapWay;
    AreaRef: TMapArea;
    Name: string;
  end;

  { The RoutingService implements functionality in the context of routing.
  The following functions are available:
  - Calculation of a route from a start node to a target node
  - Transformation of the resulting route to a Way
  - Transformation of the resulting route to a simple list of points
  - Transformation of the resulting route to a routing description with is the base
  for further transformations to a textual or visual description of the route
  - Returning the closest routeable node to given geolocation }

  { TSimpleRoutingService }

  TSimpleRoutingService = class(TAbstractRoutingService)
  private
    FDatabase: TMapDatabase;        // Database object, holding all index and data files
    FFileNameBase: string;          // Common base name for all router files
    FAccessReader: TFeatureValueReader;   // Read access information from objects
    FIsOpen: Boolean;               // true, if opened
    FPath: string;                  // Path to the directory containing all files

    FRoutingDatabase: TRoutingDatabase; // Access to routing data and index files

    function HasNodeWithId(const ANodes: TGeoPointArray): Boolean;
  protected
    function GetVehicle(AProfile: TRoutingProfile): TVehicleType; override;

    function CanUse(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID; const ARouteNode: TRouteNode; APathIndex: Integer): Boolean; override;
    function CanUseForward(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID; AWay: TMapWay): Boolean; override;
    function CanUseBackward(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID; AWay: TMapWay): Boolean; override;

    function GetCosts(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      const ARouteNode: TRouteNode; APathIndex: Integer): TReal; overload; override;
    function GetCosts(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      AWay: TMapWay; AWayLength: TDistance): TReal; overload; override;

    function GetEstimateCosts(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      ATargetDistance: TDistance): TReal; override;
    function GetCostLimit(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      ATargetDistance: TDistance): TReal; override;

    function GetRouteNodes(const ARouteNodeIds: TMapDBIdArray; ARouteNodeMap: TRouteNodeMap): Boolean; override;
    { Return the route node for the given database offset }
    function GetRouteNode(const AId: TMapDBId; out ANode: TRouteNode): Boolean; override;

    { !! offsets only for data files
    function GetWayByOffset(const AOffset: TMapDBFileOffset): TMapWay; override;
    function GetWaysByOffset(const AOffsetList: TMapDBFileOffsetArray; AWayMap: TMapWayDict): Boolean; override;

    function GetAreaByOffset(const AOffset: TMapDBFileOffset): TMapArea; override;
    function GetAreasByOffset(const AOffsetList: TMapDBFileOffsetArray; AAreaMap: TMapAreaDict): Boolean; override;}

    function ResolveRouteDataJunctions(var ARoute: TRouteData): Boolean; override;

    { add twin nodes to nextNode from other databases to open list }
    function GetNodeTwins(AProfile: TRoutingProfile;
      const ADatabase: TMapDatabaseID;
      AId: TId): TMapDBIdArray; override;
  public
    { Create a new instance of the routing service.
      ADatabase - A valid reference to a database instance
      AParameter - An instance to the parameter object holding further paramterization }
    constructor Create(const ADatabase: TMapDatabaseID;
      const AParameter: TRouterParameter;
      const AFileNameBase: string);
    destructor Destroy(); override;
    { Opens the routing service. This loads the routing graph for the given vehicle
      Return false on error, else true }
    function Open(): Boolean;
    { Returns true, if the routing service has been successfully opened, else false. }
    function IsOpen(): Boolean;
    { Close the routing service }
    procedure Close();

    { Returns the type configuration of the underlying database instance }
    function GetTypeConfig(): TTypeConfig;

    { Calculate a route going through all the via points
      AProfile - Profile to use
      AVia - A vector of via points
      ARadius - The maximum radius to search in from the search center
      AParameter - A RoutingParamater object
      Result - A RoutingResult object }
    function CalculateRouteViaCoords(AProfile: TRoutingProfile;
      AVia: TGeoPointArray;
      const ARadius: TDistance;
      const AParameter: TRouterParameter): TRoutingResult;

    { Returns the closest routeable object (area or way) relative
      to the given coordinate.

      The result should be use as imput for the router to define
      routing start or end point.

      @note The returned node may in fact not be routable, it is just
      the closest node to the given position on a routable way or area.

      @note The actual object may not be within the given radius
      due to internal search index resolution.

      ACoord - coordinate of the search center
      AProfile - Routing profile to use. It defines Vehicle to use
      ARadius - The maximum radius to search in from the search center

      Return a reference to a node on a way or area that is routable (if returned
      route position is valid) }
    function GetClosestRoutableNode(const ACoord: TGeoPoint;
      AProfile: TRoutingProfile;
      const ARadius: TDistance): TRoutePosition;

    { Returns the closest routeable object (area or way) relative
      to the given coordinate.

     The result should be use for typical "object you are traveling on"
     information as used by routing applications.

     @note The returned node may in fact not be routable, it is just
     the closest node to the given position on a routable way or area.

     @note The actual object may not be within the given radius
     due to internal search index resolution.

     @note This is a simple solution that does not track any spast state.
     A better implementation should hold on recently travels coordinates and
     ways or areas and do some tolerance error handling in case of GPS
     jitter effects.

     ALocation - coordinate of the search center
     AVehicle - The vehicle to use
     AMaxRadius - The maximum radius to search in from the search center

     Return a convinient description of the clostest routable object (if valid) }
    function GetClosestRoutableObject(const ALocation: TGeoPoint;
      AVehicle: TVehicleType;
      const AMaxRadius: TDistance): TClosestRoutableObjectResult;

    function GetDatabaseMapping(): TMapDatabaseIdMap; override;

    procedure DumpStatistics();
  end;

implementation

{ TSimpleRoutingService }

constructor TSimpleRoutingService.Create(const ADatabase: TMapDatabaseID;
  const AParameter: TRouterParameter; const AFileNameBase: string);
begin
  inherited Create(AParameter);
  FDatabase := ADatabase;
  FFileNameBase := AFileNameBase;
  FAccessReader.Init(GetTypeConfig(), ftAccess);
  FIsOpen := False;
  //Assert(FDatabase <> 0);
end;

destructor TSimpleRoutingService.Destroy();
begin
  if IsOpen() then
    Close();

  inherited Destroy();
end;

function TSimpleRoutingService.HasNodeWithId(const ANodes: TGeoPointArray): Boolean;
var
  TmpNode: TGeoPoint;
begin
  Assert(False);
  for (TmpNode in ANodes) do
  begin
    { !!!
    if TmpNode.IsRelevant() then // TmpNode.Serial <> 0
    begin
      Result := True;
      Break;
    end;}
  end;
  Result := False;
end;

function TSimpleRoutingService.GetVehicle(AProfile: TRoutingProfile): TVehicleType;
begin
  Result := AProfile.Vehicle;
end;

function TSimpleRoutingService.CanUse(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; const ARouteNode: TRouteNode;
  APathIndex: Integer): Boolean;
begin
  Result := AProfile.CanUse(ARouteNode, FRoutingDatabase.GetObjectVariantData(), APathIndex);
end;

function TSimpleRoutingService.CanUseForward(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; AWay: TMapWay): Boolean;
begin
  Result := AProfile.CanUseForward(AWay);
end;

function TSimpleRoutingService.CanUseBackward(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; AWay: TMapWay): Boolean;
begin
  Result := AProfile.CanUseBackward(AWay);
end;

function TSimpleRoutingService.GetCosts(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; const ARouteNode: TRouteNode;
  APathIndex: Integer): TReal;
begin
  Result := AProfile.GetCosts(ARouteNode, FRoutingDatabase.GetObjectVariantData(), APathIndex);
end;

function TSimpleRoutingService.GetCosts(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; AWay: TMapWay; AWayLength: TDistance): TReal;
begin
  Result := AProfile.GetCosts(AWay, AWayLength);
end;

function TSimpleRoutingService.GetEstimateCosts(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; ATargetDistance: TDistance): TReal;
begin
  Result := AProfile.GetCosts(ATargetDistance);
end;

function TSimpleRoutingService.GetCostLimit(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; ATargetDistance: TDistance): TReal;
begin
  Result := AProfile.GetCosts(AProfile.GetCostLimitDistance())
          + (AProfile.GetCosts(ATargetDistance) * AProfile.GetCostLimitFactor());
end;

function TSimpleRoutingService.GetRouteNodes(const ARouteNodeIds: TMapDBIdArray;
  ARouteNodeMap: TRouteNodeMap): Boolean;
var
  Ids: TMapDBIdArray;
  i: Integer;
  nodeMap: TRouteNodeMap;
  dbId: TMapDatabaseId;
begin
  if Length(ARouteNodeIds) = 0 then
  begin
    Result := True;
    Exit;
  end;

  SetLength(Ids, Length(ARouteNodeIds));

  for i := Low(ARouteNodeIds) to Hihg(ARouteNodeIds) do
  begin
    Ids[i] := ARouteNodeIds[i].;
  end;

  //std::unordered_map<Id,RouteNodeRef> nodeMap;

  if (not FRoutingDatabase.GetRouteNodes(Low(Ids), High(Ids), Length(Ids), nodeMap)) then
  begin
    Result := False;
    Exit;
  end;

  dbId := ARouteNodeIds[0].DatabaseId;

  for i := 0 to nodeMap.Count-1 do
  begin
    ARouteNodeMap.AddOrSetData(MapDBId(dbId, nodeMap.Keys[i]), nodeMap.Data[i]);
  end;

  Result := True;
end;

function TSimpleRoutingService.GetRouteNode(const AId: TMapDBId; out
  ANode: TRouteNode): Boolean;
begin
  Result := FRoutingDatabase.GetRouteNode(AId.Id, ANode);
end;

{function TSimpleRoutingService.GetWayByOffset(const AOffset: TMapDBFileOffset): TMapWay;
begin
  FDatabase.WayDataFile.GetByOffset(AOffset, Result);
end;

function TSimpleRoutingService.GetWaysByOffset(const AOffsetList: TMapDBFileOffsetArray;
  AWayMap: TMapWayDict): Boolean;
begin
end;

function TSimpleRoutingService.GetAreaByOffset(const AOffset: TMapDBFileOffset): TMapArea;
begin

end;

function TSimpleRoutingService.GetAreasByOffset(const AOffsetList: TMapDBFileOffsetArray;
  AAreaMap: TMapAreaDict): Boolean;
begin

end;}

function TSimpleRoutingService.ResolveRouteDataJunctions(var ARoute: TRouteData): Boolean;
var
  nodeIds: TIdArray;
  junction: TIntersection;
  junctions: TIntersectionArray;
  routeEntry: TRouteEntry;
  junctionMap: TIntersectionMapById;
  n: Integer;
begin
  Result := False;
  for (routeEntry in ARoute.Entries) do
  begin
    if (routeEntry.CurNodeId <> 0) then
      nodeIds := Concat(nodeIds, [routeEntry.CurNodeId]);
  end;

  if (not FRoutingDatabase.GetJunctions(nodeIds, junctions)) then
    Exit;

  SetLength(nodeIds, 0);

  for (junction in junctions) do
    junctionMap.AddOrSetData(junction.NodeId, junction);

  SetLength(junctions, 0);

  for (routeEntry in ARoute.Entries) do
  begin
    if (routeEntry.CurNodeId <> 0) then
      if junctionMap.Find(routeEntry.CurNodeId, n) then
      begin
        junction := junctionMap.Data[n];
        routeEntry.Objects := junction.Objects;
      end;
  end;

  Result := True;

end;

function TSimpleRoutingService.GetNodeTwins(AProfile: TRoutingProfile;
  const ADatabase: TMapDatabaseID; AId: TId): TMapDBIdArray;
begin
  SetLength(Result, 0);
end;

function TSimpleRoutingService.Open(): Boolean;
begin
  FPath := FDatabase.Path;

  Assert(FPath <> '');

  Result := FRoutingDatabase.Open(FDatabase);
  if Result then
    FIsOpen := True;
end;

function TSimpleRoutingService.IsOpen(): Boolean;
begin
  Result := FIsOpen;
end;

procedure TSimpleRoutingService.Close();
begin
  FRoutingDatabase.Close();

  FIsOpen := False;
end;

function TSimpleRoutingService.GetTypeConfig(): TTypeConfig;
begin
  Result := FDatabase.TypeConfig;
end;

function TSimpleRoutingService.CalculateRouteViaCoords(AProfile: TRoutingProfile;
  AVia: TGeoPointArray; const ARadius: TDistance;
  const AParameter: TRouterParameter): TRoutingResult;
var
  nodeIndexes: array of Integer;
  objects: TObjectFileRefArray;
  fromObject, toObject: TObjectFileRef;
  etap: TGeoPoint;
  r: TDistance;
  target: TRoutePosition;
  index, fromNodeIndex, toNodeIndex: Integer;
  partialResult: TRoutingResult;
begin
  Assert(Length(via) > 0);

  Result.Init();

  for (etap in via) do
  begin
    r := ARadius;
    target := GetClosestRoutableNode(etap, AProfile, r);

    if (not target.IsValid()) then
      Exit;

    nodeIndexes := Concat(nodeIndexes, [target.NodeIndex]);
    objects := Concat(objects, [target.Obj]);
  end;

  for (index := 0 to Length(nodeIndexes)-2 do
  begin
    fromNodeIndex := nodeIndexes[index];
    fromObject := objects[index];
    toNodeIndex := nodeIndexes[index+1];
    toObject := objects[index+1];

    partialResult := CalculateRoute(AProfile,
      RoutePosition(fromObject, fromNodeIndex, 0),
      RoutePosition(toObject, toNodeIndex, 0),
      AParameter);

    if (not partialResult.Success()) then
    begin
      Result.Route.Clear();
      Exit;
    end;

    { In intermediary via points the end of the previous part is the start of the }
    { next part, we need to remove the duplicate point in the calculated route }
    if (index < Length(nodeIndexes)-2) then
      partialResult.Route.PopEntry();

    Result.Route.Append(partialResult.Route);
  end;
end;

function TSimpleRoutingService.GetClosestRoutableNode(const ACoord: TGeoPoint;
  AProfile: TRoutingProfile; const ARadius: TDistance): TRoutePosition;
var
  position: TRoutePosition;
  boundingBox: TGeoBox;
  wayRoutableTypes,
  areaRoutableTypes,
  wayLoadedTypes,
  areaLoadedTypes: TTypeInfoSet;
  TypeInfo: TTypeInfo;
  wayWayOffsets: TFileOffsetArray;
  wayAreaSpans: TDataBlockSpanArray;
  areas: array of TMapArea; // TMapAreaList
  area: TMapArea;
  ways: array of TMapWay; // TMapWayList
  way: TMapWay;
  minDistance, distance: TReal;
  TmpPoint: TGeoPoint;
  r, intersectLon, intersectLat: TReal;
  i: Integer;
begin
  Result.Init(0, 0, 0);

  if (not Assigned(FDatabase))
  or (not Assigned(FDatabase.TypeConfig))
  or (not Assigned(FDatabase.AreaAreaIndex))
  or (not Assigned(FDatabase.AreaWayIndex))
  or (not Assigned(FDatabase.AreaDataFile))
  or (not Assigned(FDatabase.WayDataFile))
  then
  begin
    WriteLn(log, 'Error: At least one index file is invalid!');
  end;

  boundingBox.BoxByCenterAndRadius(ACoord, ARadius);

  for (TypeInfo in FDatabase.TypeConfig.Types) do
  begin
    if (not TypeInfo.IsIgnore) and TypeInfo.CanRoute(AProfile.GetVehicle()) then
    begin
      if TypeInfo.CanBeWay then
        wayRoutableTypes.Add(TypeInfo);

      // TODO: Currently disabled, since router cannot handle areas as start or target node
      { if TypeInfo.CanBeArea then
        areaRoutableTypes.Add(TypeInfo); }
    end;
  end;

  if (not FDatabase.AreaWayIndex.GetOffsets(boundingBox, wayRoutableTypes, wayWayOffsets, wayLoadedTypes)) then
  begin
    WriteLn(log, 'Error: Error getting ways from area way index!');
  end;

  if (not FDatabase.AreaAreaIndex.GetAreasInArea(FDatabase.TypeConfig,
    boundingBox, MaxInt, areaRoutableTypes, wayAreaSpans, areaLoadedTypes)) then
  begin
    WriteLn(log, 'Error: Error getting areas from area area index!');
  end;

  Sort(wayWayOffsets);

  if (not FDatabase.WayDataFile.GetByOffset(wayWayOffsets, ways)) then
  begin
    WriteLn(log, 'Error: Error reading ways in area!');
    Exit;
  end;

  if (not FDatabase.AreaDataFile.GetByBlockSpans(wayAreaSpans, areas)) then
  begin
    WriteLn(log, 'Error: Error reading areas in area!');
    Exit;
  end;

  minDistance := infinity;

  for (area in areas) do
  begin
    if (not AProfile.CanUse(area)) then
      Continue;

    if (not HasNodeWithId(area.Rings[0].Nodes)) then
      Continue;

    for i := 0 to Length(area.Rings[0].Nodes)-1 do
    begin
      TmpPoint := area.Rings[0].Nodes[i];
      distance = Sqrt((TmpPoint.Lat - ACoord.Lat) * (TmpPoint.Lat - ACoord.Lat)
                    + (TmpPoint.Lon - ACoord.Lon) * (TmpPoint.Lon - ACoord.Lon));

      if (minDistance > distance) then
      begin
        minDistance := distance;
        Result := RoutePosition(area.GetObjectFileRef(), i, 0);
      end;
    end;
  end;

  for (way in ways) do
  begin
    if (not AProfile.CanUse(way)) then
      Continue;

    if (not HasNodeWithId(way.Nodes)) then
      Continue;

    for i := 0 to Length(way.Nodes)-2 do
    begin
      distance := DistanceToSegment(ACoord, way.Nodes[i], way.Nodes[i+1], r, TmpPoint);
      if (distance < minDistance) then
      begin
        minDistance := distance;
        if (r < 0.5) then
        begin
          Result := RoutePosition(way.GetObjectFileRef(), i, {database} 0);
        end
        else
        begin
          Result := RoutePosition(way.GetObjectFileRef(), i+1, {database} 0);
        end;
      end;
    end;
  end;
end;

function TSimpleRoutingService.GetClosestRoutableObject(const ALocation: TGeoPoint;
  AVehicle: TVehicleType;
  const AMaxRadius: TDistance): TClosestRoutableObjectResult;
var
  routeableWayTypes: TTypeInfoSet;
  routeableAreaTypes: TTypeInfoSet;
  TypeInfo: TTypeInfo;
  closestDistance: TDistance;
  closestWay: TMapWay;
  closestArea: TMapArea;
begin

  for (TypeInfo in FDatabase.TypeConfig.Types) do
  begin
    if (not TypeInfo.IsIgnore) and TypeInfo.CanBeWay and TypeInfo.CanRoute(AVehicle) then
      routeableWayTypes.Add(TypeInfo);

    if (not TypeInfo.IsIgnore) and TypeInfo.CanBeArea and TypeInfo.CanRoute(AVehicle) then
      routeableAreaTypes.Add(TypeInfo);
  end;

  closestDistance := Infinity;

  if (not routeableWayTypes.Empty()) {
    WayRegionSearchResult waySearchResult=database->LoadWaysInRadius(location,
                                                                     routeableWayTypes,
                                                                     maxRadius);

    for (const auto& entry : waySearchResult.GetWayResults()) {
      if (entry.GetDistance()<closestDistance) {
        closestDistance=entry.GetDistance();
        closestWay=entry.GetWay();
        closestArea.reset();
      }
    }
  }

  if (!routeableAreaTypes.Empty()) {
    AreaRegionSearchResult areaSearchResult=database->LoadAreasInRadius(location,
                                                                        routeableAreaTypes,
                                                                        maxRadius);
    for (const auto& entry : areaSearchResult.GetAreaResults()) {
      if (entry.GetDistance()<closestDistance) {
        closestDistance=entry.GetDistance();
        closestWay.reset();
        closestArea=entry.GetArea();
      }
    }
  }

  if (!closestWay && !closestArea) {
    result.distance=Distance::Max();
    return result;
  }
  else {
    NameFeatureLabelReader nameFeatureLabelReader(*database->GetTypeConfig());

    result.distance=closestDistance;
    if (closestWay) {
      result.way=closestWay;
      result.object=result.way->GetObjectFileRef();
      result.name=nameFeatureLabelReader.GetLabel(result.way->GetFeatureValueBuffer());
    }
    else {
      result.area=closestArea;
      result.object=result.area->GetObjectFileRef();
      result.name=nameFeatureLabelReader.GetLabel(result.area->GetFeatureValueBuffer());
    }
  }

  return result;
end;

function TSimpleRoutingService.GetDatabaseMapping(): TMapDatabaseIdMap;
begin
  Result.AddOrSetData(0, ADatabase.Path);
end;

procedure TSimpleRoutingService.DumpStatistics();
begin
  if Assigned(ADatabase) then
    ADatabase.DumpStatistics();
end;

end.

