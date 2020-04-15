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
  OsMapRouting, OsMapRoutingService, OsMapRoutingDatabase;

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
    FDatabase: TMapDatabaseID;      // Database object, holding all index and data files
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
      const ARouteNode: TRouteNode; APathIndex: Integer): Double; overload; override;
    function GetCosts(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      AWay: TMapWay; AWayLength: TDistance): Double; overload; override;

    function GetEstimateCosts(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      ATargetDistance: TDistance): Double; override;
    function GetCostLimit(AProfile: TRoutingProfile; ADatabase: TMapDatabaseID;
      ATargetDistance: TDistance): Double; override;

    function GetRouteNodes(const ARouteNodeIds: TMapDBIdArray; ARouteNodeMap: TRouteNodeMap): Boolean; override;
    { Return the route node for the given database offset }
    function GetRouteNode(const AId: TMapDBId; out ANode: TRouteNode): Boolean; override;

    function GetWayByOffset(const AOffset: TMapDBFileOffset): TMapWay; override;
    function GetWaysByOffset(const AOffsetList: TMapDBFileOffsetArray; AWayMap: TMapWayDict): Boolean; override;

    function GetAreaByOffset(const AOffset: TMapDBFileOffset): TMapArea; override;
    function GetAreasByOffset(const AOffsetList: TMapDBFileOffsetArray; AAreaMap: TMapAreaDict): Boolean; override;

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

    function Open(): Boolean;
    function IsOpen(): Boolean;
    procedure Close();

    function GetTypeConfig(): TTypeConfig;

    function CalculateRouteViaCoords(AProfile: TRoutingProfile;
      AVia: TGeoPointArray;
      const ARadius: TDistance;
      const AParameter: TRouterParameter): TRoutingResult;

    function GetClosestRoutableNode(const ACoord: TGeoPoint;
      AProfile: TRoutingProfile;
      const ARadius: TDistance): TRoutePosition;

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
  APathIndex: Integer): Double;
begin
  Result := AProfile.GetCosts(ARouteNode, FRoutingDatabase.GetObjectVariantData(), APathIndex);
end;

function TSimpleRoutingService.GetCosts(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; AWay: TMapWay; AWayLength: TDistance): Double;
begin
  Result := AProfile.GetCosts(AWay, AWayLength);
end;

function TSimpleRoutingService.GetEstimateCosts(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; ATargetDistance: TDistance): Double;
begin
  Result := AProfile.GetCosts(ATargetDistance);
end;

function TSimpleRoutingService.GetCostLimit(AProfile: TRoutingProfile;
  ADatabase: TMapDatabaseID; ATargetDistance: TDistance): Double;
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

function TSimpleRoutingService.GetWayByOffset(const AOffset: TMapDBFileOffset): TMapWay;
var
  wayDataFile: TWayDataFile;
begin
  wayDataFile := FDatabase. ->GetWayDataFile());

  if (!wayDataFile) {
    return false;
  }

  return wayDataFile->GetByOffset(offset.offset,way);

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

end;

function TSimpleRoutingService.ResolveRouteDataJunctions(var ARoute: TRouteData): Boolean;
begin

end;

function TSimpleRoutingService.GetNodeTwins(AProfile: TRoutingProfile;
  const ADatabase: TMapDatabaseID; AId: TId): TMapDBIdArray;
begin

end;

function TSimpleRoutingService.Open(): Boolean;
begin

end;

function TSimpleRoutingService.IsOpen(): Boolean;
begin

end;

procedure TSimpleRoutingService.Close();
begin

end;

function TSimpleRoutingService.GetTypeConfig(): TTypeConfig;
begin

end;

function TSimpleRoutingService.CalculateRouteViaCoords(AProfile: TRoutingProfile;
  AVia: TGeoPointArray; const ARadius: TDistance;
  const AParameter: TRouterParameter): TRoutingResult;
begin

end;

function TSimpleRoutingService.GetClosestRoutableNode(const ACoord: TGeoPoint;
  AProfile: TRoutingProfile; const ARadius: TDistance): TRoutePosition;
begin

end;

function TSimpleRoutingService.GetClosestRoutableObject(const ALocation: TGeoPoint;
  AVehicle: TVehicleType;
  const AMaxRadius: TDistance): TClosestRoutableObjectResult;
begin

end;

function TSimpleRoutingService.GetDatabaseMapping(): TMapDatabaseIdMap;
begin

end;

procedure TSimpleRoutingService.DumpStatistics();
begin

end;

end.

