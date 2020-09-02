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
OsMap OSM objects

Node:
  Node -> TMapNode
Area:
  AreaRing
  Area -> TMapArea
Way:
  Way -> TMapWay
GroundTile:
  GroundTile -> TGroundTile
  GroundTileCoord -> TGroundTileCoord

MapPainter:
  MapData - TMapData
*)
unit OsMapObjects;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fgl,
  {$else}
  System.Generics.Collections,
  {$endif}
  OsMapObjTypes, OsMapGeometry, OsMapTypes, OsMapFiles;

const
  MASTER_RING_ID = 0;
  OUTER_RING_ID = 1;
  GROUND_TILE_CELL_MAX = 32767;   // GroundTile::Coord

type

  { Cached options for drawable item
    prevent repeated calculations for invisible objects
    Actual for same zoom level. If zoom level changed, must be recalculated }
  TMapItemDrawOptions = packed record
    ZoomLevel: Byte;
    IsStyleVisible: Boolean;
    IsTextLabelVisible: Boolean;
    IsBorderLabelVisible: Boolean;
  end;
  PMapItemDrawOptions = ^TMapItemDrawOptions;

  TMapObject = class
  public
    { File offset in the data file, use as unique id }
    FileOffset: TFileOffset;
    { Offset after this node }
    NextFileOffset: TFileOffset;

    function GetObjectFileRef(): TObjectFileRef; virtual; abstract;
    function GetType(): TTypeInfo; virtual; abstract;

    { Returns true if the nodes is in the given bounding box }
    function Intersects(const ABoundingBox: TGeoBox): Boolean; virtual; abstract;

    { Read the object data from the given FileScanner. }
    procedure Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner); virtual; abstract;
    { Write the object data to the given FileWriter }
    procedure Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter); virtual; abstract;
  end;
  TMapObjectClass = class of TMapObject;

  {$ifdef FPC}
  TMapObjectList = specialize TFPGList<TMapObject>;
  {$else}
  TMapObjectList = TList<TMapObject>;
  {$endif}

  { TMapNode }

  TMapNode = class(TMapObject)
  public
    { List of features }
    FeatureValueBuffer: TFeatureValueBuffer;
    { Coordinates of node }
    Coord: TGeoPoint;

    function GetObjectFileRef(): TObjectFileRef; override;
    function GetType(): TTypeInfo; override; // FeatureValueBuffer.GetType()

    { Returns true if the nodes is in the given bounding box }
    function Intersects(const ABoundingBox: TGeoBox): Boolean; override;

    function GetFeatureCount(): Integer;
    function HasFeature(AIndex: Integer): Boolean;
    function GetFeature(AIndex: Integer): TFeatureInfo;
    procedure UnsetFeature(AIndex: Integer);

    { Read the node data from the given FileScanner. }
    procedure Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner); override;
    { Write the node data to the given FileWriter }
    procedure Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter); override;
  end;

  {$ifdef FPC}
  TMapNodeList = specialize TFPGList<TMapNode>;
  {$else}
  TMapNodeList = TList<TMapNode>;
  {$endif}

  { TMapAreaRing }

  TMapAreaRing = object
  public
    // List of features
    FeatureValueBuffer: TFeatureValueBuffer;
    // The ring hierarchy number (0...n)
    Ring: Byte;
    { Note that ring nodes, bbox and segments fields are public for simple manipulation.
      User that modify it is responsible to keep these values in sync!
      You should not rely on segments and bbox, it is just a cache used some algorithms.
      It may be empty/invalid! }

    { The array of coordinates }
    Nodes: TGeoPointArray;
    { Precomputed (cache) segment bounding boxes for optimisation }
    Segments: TSegmentGeoBoxArray;
    { Precomputed (cache) bounding box }
    BBox: TGeoBox;
    { Cached options for same zoom level }
    DrawOptions: TMapItemDrawOptions;

    procedure Init();

    function GetType(): TTypeInfo;
    procedure SetType(const AValue: TTypeInfo);

    function HasAnyFeaturesSet(): Boolean;

    function IsMasterRing(): Boolean;
    function IsOuterRing(): Boolean;

    function GetNodeIndexByNodeId(const AId: TId; var AIndex: Integer): Boolean;

    function GetCenter(var ACenter: TGeoPoint): Boolean;

    procedure FillBoundingBox(out ABoundingBox: TGeoBox);
    function GetBoundingBox(): TGeoBox;

    { Return True if given point inside this Ring polygon }
    function IsPointInside(const APoint: TGeoPoint): Boolean;
  end;

  { Node data read/write mode for TArea.Read() and TArea.Write()
    ndmAuto - Node ids will only be used if not thought to be required for this area.
    ndmAll  - All data available will be used.
    ndmNone - No node ids will be used. }
  TNodeDataMode = (ndmAuto, ndmAll, ndmNone);

  { Representation of an (complex/multipolygon) area }
  TMapArea = class(TMapObject)
  public
    Rings: array of TMapAreaRing;

    procedure AfterConstruction(); override;
    procedure Init();

    function GetObjectFileRef(): TObjectFileRef; override;
    function GetType(): TTypeInfo; override;
    function GetFeatureValueBuffer(): TFeatureValueBuffer;

    function IsSimple(): Boolean;

    function GetCenter(var ACenter: TGeoPoint): Boolean;

    function GetBoundingBox(): TGeoBox;
    { Returns true if the bounding box of the object intersects the given
      bounding box }
    function Intersects(const ABoundingBox: TGeoBox): Boolean; override;

    { Read the area as written by Write().
      ADataMode:
       ndmAuto - Node ids will only be read if not thought to be required for this area.
       ndmAll  - All data available will be read.
       ndmNone - No node ids will be read. }
    procedure ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner; ADataMode: TNodeDataMode = ndmAuto);
    procedure Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner); override;
    { Read the area as written by WriteImport() All data available will be read. }
    procedure ReadImport(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
    { Read the area as written by WriteOptimized() No node ids will be read. }
    procedure ReadOptimized(ATypeConfig: TTypeConfig; AScanner: TFileScanner);

    { Write the area with all data required in the standard database }
    procedure Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter); override;
    { Write the area with all data required in the standard database }
    procedure WriteData(ATypeConfig: TTypeConfig; AWriter: TFileWriter; ADataMode: TNodeDataMode = ndmAuto);
    { Write the area with all data required during import,
      certain optimizations done on the final data
      are not done here to not loose information }
    procedure WriteImport(ATypeConfig: TTypeConfig; AWriter: TFileWriter);
    { Write the area with all data required by the OptimizeLowZoom
      index, dropping all ids }
    procedure WriteOptimized(ATypeConfig: TTypeConfig; AWriter: TFileWriter);

  end;

  {$ifdef FPC}
  TMapAreaList = specialize TFPGList<TMapArea>;
  {$else}
  TMapAreaList = TList<TMapArea>;
  {$endif}

  { TMapWay }

  TMapWay = class(TMapObject)
    { Precomputed (cache) bounding box }
    FBBox: TGeoBox;
  public
    { List of features }
    FeatureValueBuffer: TFeatureValueBuffer;

    { Cached options for same zoom level }
    DrawOptions: TMapItemDrawOptions;
    { The array of coordinates }
    Nodes: TGeoPointArray;
    { Precomputed (cache) segment bounding boxes for optimisation }
    Segments: TSegmentGeoBoxArray;

    function GetObjectFileRef(): TObjectFileRef; override;
    function GetType(): TTypeInfo; override;

    function GetFeatureCount(): Integer;
    function HasFeature(AIndex: Integer): Boolean;
    function GetFeature(AIndex: Integer): TFeatureInfo;
    procedure UnsetFeature(AIndex: Integer);

    function IsCircular(): Boolean;

    // Nodes[AIndex].Serial
    function GetSerial(AIndex: Integer): TId;
    // Nodes[AIndex].GetId()
    function GetId(AIndex: Integer): TId;
    // Nodes[0].GetId()
    function GetFrontId(): TId;
    // Nodes[Count-1].GetId()
    function GetBackId(): TId;
    // Nodes[AIndex]
    function GetPoint(AIndex: Integer): TGeoPoint;
    // Nodes[AIndex].Coord
    function GetCoord(AIndex: Integer): TGeoPoint;

    function GetBoundingBox(): TGeoBox;
    { Returns true if the bounding box of the object intersects the given
      bounding box }
    function Intersects(const ABoundingBox: TGeoBox): Boolean; override;
    function GetCenter(var ACenter: TGeoPoint): Boolean;

    function GetNodeIndexByNodeId(AId: TId; out AIndex: Integer): Boolean;

    procedure SetType(const AValue: TTypeInfo);

    { Read the data as written by Write().
      ADataMode:
       ndmAuto - Node ids will only be read if not thought to be required.
       ndmAll  - All data available will be read.
       ndmNone - No node ids will be read. }
    procedure ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner; ADataMode: TNodeDataMode = ndmAuto);
    procedure Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner); override;
    { Read the data as written by WriteOptimized() No node ids will be read. }
    procedure ReadOptimized(ATypeConfig: TTypeConfig; AScanner: TFileScanner);

    { Write the area with all data required in the standard database }
    procedure WriteData(ATypeConfig: TTypeConfig; AWriter: TFileWriter; ADataMode: TNodeDataMode = ndmAuto);
    procedure Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter); override;
    { Write the area with all data required by the OptimizeLowZoom
      index, dropping all ids }
    procedure WriteOptimized(ATypeConfig: TTypeConfig; AWriter: TFileWriter);
  end;

  {$ifdef FPC}
  TMapWayList = specialize TFPGList<TMapWay>;
  TMapWayDict = specialize TFPGMap<string, TMapWay>;
  TMapAreaDict = specialize TFPGMap<string, TMapArea>;
  {$else}
  TMapWayList = TList<TMapWay>;
  {$endif}


  { Ground tiles }

  TGroundTileType = (gtUnknown,
                     gtLand,      // left side of the coast
                     gtWater,     // right side of the coast
                     gtCoast);

  {  A Coordinate for a point in a ground tile path. }
  TGroundTileCoord = object
  public
    X: Word;
    Y: Word;
    IsCoast: Boolean;

    procedure SetValue(AX, AY: Word; AIsCoast: Boolean);
    function IsEqual(const AValue: TGroundTileCoord): Boolean;
  end;

  TGroundTileCoordArray = array of TGroundTileCoord;

  { A single ground tile cell. The ground tile defines an area
    of the given type.

    If the coords array is empty, the area is the complete cell.
    If the coords array is not empty it is defining a polygon which
    is of the given type.

    A cell can either have no GroundTile, one GroundTile that fills
    the complete cell area or multiple GroundTiles that only fill
    parts of the cell area.

    The polygon can consist (partly) of a coastline (Coord.coast=true) or
    of cell boundary lines (Coord.cell=false). }
  TGroundTile = object
  public
    TileType: TGroundTileType;  // The type of the cell
    XAbs: Integer;              // Absolute x coordinate of the cell in relation to level and cell size
    YAbs: Integer;              // Absolute y coordinate of the cell in relation to level and cell size
    XRel: Integer;              // X coordinate of cell in relation to cell index of this level
    YRel: Integer;              // Y coordinate of cell in relation to cell index of this level
    CellWidth: TReal;          // Width of cell
    CellHeight: TReal;         // Height of cell
    Coords: TGroundTileCoordArray;  // Optional coordinates for coastline
  end;

  //TGroundTileList = specialize TFPGList<TGroundTile>;
  TGroundTileList = array of TGroundTile;

  { This is the data structure holding all to be rendered data. }

  { TMapData }

  TMapData = class
  private
    FNodeList: TMapNodeList;
    FAreaList: TMapAreaList;
    FWayList: TMapWayList;
    FPoiNodeList: TMapNodeList;
    FPoiAreaList: TMapAreaList;
    FPoiWayList: TMapWayList;
    FGroundTileList: TGroundTileList;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    procedure ClearDbData();
    // Nodes as retrieved from database
    property NodeList: TMapNodeList read FNodeList;
    // Areas as retrieved from database
    property AreaList: TMapAreaList read FAreaList;
    // Ways as retrieved from database
    property WayList: TMapWayList read FWayList;
    // List of manually added nodes (not managed or changed by the database)
    property PoiNodeList: TMapNodeList read FPoiNodeList;
    // List of manually added areas (not managed or changed by the database)
    property PoiAreaList: TMapAreaList read FPoiAreaList;
    // List of manually added ways (not managed or changed by the database)
    property PoiWayList: TMapWayList read FPoiWayList;
    // List of ground tiles (optional)
    property GroundTileList: TGroundTileList read FGroundTileList;
  end;

  {$ifdef FPC}
  TMapDataList = specialize TFPGList<TMapData>;
  {$else}
  TMapDataList = TList<TMapData>;
  {$endif}

//function MapNode();

  procedure ResetMapItemDrawOptions(var AValue: TMapItemDrawOptions);

implementation

uses Math; // eliminate "end of source not found"

procedure ResetMapItemDrawOptions(var AValue: TMapItemDrawOptions);
begin
  AValue.ZoomLevel := 0;
  AValue.IsStyleVisible := True;
  AValue.IsTextLabelVisible := True;
  AValue.IsBorderLabelVisible := True;
end;

{ TMapNode }

function TMapNode.GetObjectFileRef(): TObjectFileRef;
begin
  Result := ObjectFileRef(FileOffset, refNode);
end;

function TMapNode.Intersects(const ABoundingBox: TGeoBox): Boolean;
begin
  // ??? Intersects or Includes?
  Result := ABoundingBox.IsIncludes(Coord);
end;

function TMapNode.GetFeatureCount(): Integer;
begin
  Result := FeatureValueBuffer.FeatureCount;
end;

function TMapNode.HasFeature(AIndex: Integer): Boolean;
begin
  Result := FeatureValueBuffer.HasFeatureValue(AIndex);
end;

function TMapNode.GetFeature(AIndex: Integer): TFeatureInfo;
begin
  Result := FeatureValueBuffer.TypeInfo.GetFeatureInfo(AIndex);
end;

procedure TMapNode.UnsetFeature(AIndex: Integer);
begin
  FeatureValueBuffer.FreeValue(AIndex);
end;

function TMapNode.GetType(): TTypeInfo;
begin
  Result := FeatureValueBuffer.TypeInfo;
end;

procedure TMapNode.Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner);
var
  TypeId: TTypeId;
begin
  FileOffset := AScanner.Stream.Position;
  AScanner.ReadTypeId(TypeId, ATypeConfig.NodeTypeIdBytes);

  FeatureValueBuffer.SetType(ATypeConfig.GetNodeTypeInfo(TypeId));
  FeatureValueBuffer.Read(AScanner);

  Coord.ReadFromStream(AScanner.Stream);
  NextFileOffset := AScanner.Stream.Position;
end;

procedure TMapNode.Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  AWriter.WriteTypeId(FeatureValueBuffer.GetType().NodeId,
                      ATypeConfig.NodeTypeIdBytes);

  FeatureValueBuffer.Write(AWriter);

  Coord.WriteToStream(AWriter.Stream);
end;

{ TMapAreaRing }

procedure TMapAreaRing.Init();
begin
  Ring := OUTER_RING_ID;
  BBox.Invalidate();
end;

function TMapAreaRing.GetType(): TTypeInfo;
begin
  Result := FeatureValueBuffer.GetType()
end;

procedure TMapAreaRing.SetType(const AValue: TTypeInfo);
begin
  FeatureValueBuffer.SetType(AValue);
end;

function TMapAreaRing.HasAnyFeaturesSet(): Boolean;
var
  i: Integer;
begin
  for i := 0 to FeatureValueBuffer.FeatureCount do
  begin
    if (FeatureValueBuffer.HasFeatureValue(i)) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TMapAreaRing.IsMasterRing(): Boolean;
begin
  Result := (Ring = MASTER_RING_ID);
end;

function TMapAreaRing.IsOuterRing(): Boolean;
begin
  Result := (Ring = OUTER_RING_ID);
end;

function TMapAreaRing.GetNodeIndexByNodeId(const AId: TId; var AIndex: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(Nodes)-1 do
  begin
    if (Nodes[i].GetId() = AId) then
    begin
      AIndex := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TMapAreaRing.GetCenter(var ACenter: TGeoPoint): Boolean;
var
  MinCoord, MaxCoord: TGeoPoint;
  IsStart: Boolean;
  i: Integer;
begin
  Result := False;
  MinCoord.Init(0.0, 0.0);
  MaxCoord.Init(0.0, 0.0);
  IsStart := True;

  for i := 0 to Length(Nodes)-1 do
  begin
    if IsStart then
    begin
      MinCoord.Assign(Nodes[i]);
      MaxCoord.Assign(Nodes[i]);
      IsStart := False;
    end
    else
    begin
      MinCoord.AssignMin(Nodes[i]);
      MaxCoord.AssignMax(Nodes[i]);
    end;
  end;

  if IsStart then
    Exit;

  ACenter.Init(MinCoord.Lat + (MaxCoord.Lat - MinCoord.Lat) / 2,
               MinCoord.Lon + (MaxCoord.Lon - MinCoord.Lon) / 2);

  Result := True;
end;

procedure TMapAreaRing.FillBoundingBox(out ABoundingBox: TGeoBox);
var
  i: Integer;
  MinCoord, MaxCoord: TGeoPoint;
begin
  Assert(Length(Nodes) <> 0);
  if BBox.Valid then
  begin
    ABoundingBox.Assign(BBox);
    Exit;
  end;

  MinCoord.Assign(Nodes[0]);
  MaxCoord.Assign(MinCoord);

  for i := 1 to Length(Nodes)-1 do
  begin
    MinCoord.AssignMin(Nodes[i]);
    MaxCoord.AssignMax(Nodes[i]);
  end;

  ABoundingBox.SetValue(MinCoord, MaxCoord);
end;

function TMapAreaRing.GetBoundingBox(): TGeoBox;
begin
  FillBoundingBox(Result);
end;

function TMapAreaRing.IsPointInside(const APoint: TGeoPoint): Boolean;
var
  ZeroPt, TmpPt: TGeoPoint;
  i, n: Integer;
begin
  Result := GetBoundingBox().IsIncludes(APoint);
  if Result then
  begin
    ZeroPt.Init(0, 0);
    n := 0;
    for i := 1 to Length(Nodes)-1 do
    begin
      if GetLineIntersection(ZeroPt, APoint, Nodes[i-1], Nodes[i], TmpPt) then
        Inc(n);
    end;
    Result := ((n mod 2) <> 0);
  end;
end;

{ TMapArea }

procedure TMapArea.AfterConstruction();
begin
  inherited AfterConstruction();
  Init();
end;

procedure TMapArea.Init();
begin
  FileOffset := 0;
  NextFileOffset := 0;
  SetLength(Rings, 1);
  Rings[0].Init();
end;

function TMapArea.GetObjectFileRef(): TObjectFileRef;
begin
  Result.Offset := FileOffset;
  Result.RefType := refArea;
end;

function TMapArea.GetType(): TTypeInfo;
begin
  Assert(Length(Rings) > 0);
  Result := Rings[0].GetType();
end;

function TMapArea.GetFeatureValueBuffer(): TFeatureValueBuffer;
begin
  Assert(Length(Rings) > 0);
  Result := Rings[0].FeatureValueBuffer;
end;

function TMapArea.IsSimple(): Boolean;
begin
  Result := Length(Rings) = 1;
end;

function TMapArea.GetCenter(var ACenter: TGeoPoint): Boolean;
var
  MinCoord, MaxCoord: TGeoPoint;
  IsStart: Boolean;
  i, ii: Integer;
begin
  Result := False;
  Assert(Length(Rings) > 0);
  MinCoord.Init(0.0, 0.0);
  MaxCoord.Init(0.0, 0.0);
  IsStart := False;
  for i := 0 to Length(Rings)-1 do
  begin
    if Rings[i].IsOuterRing() then
    begin
      for ii := 0 to Length(Rings[i].Nodes)-1 do
      begin
        if IsStart then
        begin
          MinCoord.Assign(Rings[i].Nodes[ii]);
          MaxCoord.Assign(MinCoord);
          IsStart := False;
        end
        else
        begin
          MinCoord.AssignMin(Rings[i].Nodes[ii]);
          MaxCoord.AssignMax(Rings[i].Nodes[ii]);
        end;
      end;
    end;
  end;

  if IsStart then
    Exit;

  ACenter.Init(MinCoord.Lat + (MaxCoord.Lat - MinCoord.Lat) / 2,
                   MinCoord.Lon + (MaxCoord.Lon - MinCoord.Lon) / 2);

  Result := True;
end;

function TMapArea.GetBoundingBox(): TGeoBox;
var
  i: Integer;
begin
  Result.Invalidate();
  Assert(Length(Rings) > 0);
  for i := 0 to Length(Rings)-1 do
  begin
    if Rings[i].IsOuterRing() then
    begin
      Result.Include(Rings[i].GetBoundingBox());
    end;
  end;
end;

function TMapArea.Intersects(const ABoundingBox: TGeoBox): Boolean;
begin
  Result := GetBoundingBox().IsIntersects(ABoundingBox);
end;

procedure TMapArea.ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner;
  ADataMode: TNodeDataMode);
var
  RingType: TTypeId;
  RingTypeInfo: TTypeInfo; // type
  IsMultipleRings, HasMaster, IsReadIds: Boolean;
  RingCount: Integer;
  i: Integer;
  pRing: ^TMapAreaRing;
begin
  Assert(Assigned(AScanner));
  RingCount := 1;
  FileOffset := AScanner.Stream.Position;
  AScanner.ReadTypeId(RingType, ATypeConfig.AreaTypeIdBytes);
  RingTypeInfo := ATypeConfig.GetAreaTypeInfo(RingType);

  SetLength(Rings, RingCount);
  Rings[0].FeatureValueBuffer.SetType(RingTypeInfo);
  Rings[0].FeatureValueBuffer.Read(AScanner, IsMultipleRings, HasMaster);

  if IsMultipleRings then
  begin
    AScanner.ReadNumber(RingCount);
    Inc(RingCount);
    SetLength(Rings, RingCount);
  end;

  if HasMaster then
    Rings[0].Ring := MASTER_RING_ID
  else
    Rings[0].Ring := OUTER_RING_ID;

  case ADataMode of
    ndmAuto: IsReadIds := Rings[0].GetType().CanRoute;
    ndmAll:  IsReadIds := True;
  else
    //ndmNone:
    IsReadIds := False;
  end;

  AScanner.ReadMapPoints(Rings[0].Nodes, Rings[0].Segments, Rings[0].BBox, IsReadIds);

  for i := 1 to RingCount-1 do
  begin
    pRing := @Rings[i];
    AScanner.ReadTypeId(RingType, ATypeConfig.AreaTypeIdBytes);
    RingTypeInfo := ATypeConfig.GetAreaTypeInfo(RingType);
    pRing^.SetType(RingTypeInfo);

    if RingTypeInfo.AreaId <> ObjTypeIgnore then
      pRing^.FeatureValueBuffer.Read(AScanner);

    AScanner.Read(pRing^.Ring);

    case ADataMode of
      ndmAuto: IsReadIds := (RingTypeInfo.AreaId <> ObjTypeIgnore) and RingTypeInfo.CanRoute;
      ndmAll:  IsReadIds := (RingTypeInfo.AreaId <> ObjTypeIgnore) or (pRing^.Ring = OUTER_RING_ID);
    else
      // ndmNone:
      IsReadIds := False;
    end;

    AScanner.ReadMapPoints(pRing^.Nodes, pRing^.Segments, pRing^.BBox, IsReadIds);
  end;
  NextFileOffset := AScanner.Stream.Position;
end;

procedure TMapArea.Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner);
begin
  ReadData(ATypeConfig, AScanner, ndmAuto);
end;

procedure TMapArea.ReadImport(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
begin
  ReadData(ATypeConfig, AScanner, ndmAll);
end;

procedure TMapArea.ReadOptimized(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
begin
  ReadData(ATypeConfig, AScanner, ndmNone);
end;

procedure TMapArea.WriteData(ATypeConfig: TTypeConfig; AWriter: TFileWriter;
  ADataMode: TNodeDataMode);
var
  RingTypeInfo: TTypeInfo; // type
  IsMultipleRings, HasMaster, IsUseIds: Boolean;
  RingCount: Integer;
  i: Integer;
  DebugPos: Int64;
  pRing: ^TMapAreaRing;
begin
  RingCount := Length(Rings);
  Assert(Assigned(AWriter));
  Assert(RingCount > 0);

  IsMultipleRings := (RingCount > 1);
  HasMaster := Rings[0].IsMasterRing();

  // TODO: We would like to have a bit flag here, if we have a simple area,
  // an area with one master (and multiple rings) or an area with
  // multiple outer but no master
  //
  // Also for each ring we would like to have a bit flag, if
  // we stor eids or not

  // Master/Outer ring
  pRing := @Rings[0];
  DebugPos := AWriter.Stream.Position;

  AWriter.WriteTypeId(pRing^.GetType().AreaId,
                      ATypeConfig.AreaTypeIdBytes);

  pRing^.FeatureValueBuffer.Write(AWriter, IsMultipleRings, HasMaster);

  if IsMultipleRings then
    AWriter.WriteNumber(RingCount-1);

  case ADataMode of
    ndmAuto: IsUseIds := Rings[0].GetType().CanRoute;
    ndmAll:  IsUseIds := True;
  else
    // ndmNone:
    IsUseIds := False;
  end;

  AWriter.WriteMapPoints(pRing^.Nodes, IsUseIds);

  // Potential additional rings
  for i := 1 to RingCount-1 do
  begin
    pRing := @Rings[i];
    RingTypeInfo := Rings[i].GetType();
    case ADataMode of
      ndmAuto: IsUseIds := (RingTypeInfo.AreaId <> ObjTypeIgnore) and RingTypeInfo.CanRoute;
      ndmAll:  IsUseIds := (RingTypeInfo.AreaId <> ObjTypeIgnore) or (pRing^.Ring = OUTER_RING_ID);
    else
      // ndmNone:
      IsUseIds := False;
    end;

    AWriter.WriteTypeId(RingTypeInfo.AreaId,
                        ATypeConfig.AreaTypeIdBytes);

    if (RingTypeInfo.AreaId <> ObjTypeIgnore) then
      pRing^.FeatureValueBuffer.Write(AWriter);

    AWriter.Write(pRing^.Ring);

    AWriter.WriteMapPoints(pRing^.Nodes, IsUseIds);
  end;
end;

procedure TMapArea.Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  WriteData(ATypeConfig, AWriter, ndmAuto);
end;

procedure TMapArea.WriteImport(ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  WriteData(ATypeConfig, AWriter, ndmAll);
end;

procedure TMapArea.WriteOptimized(ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  WriteData(ATypeConfig, AWriter, ndmNone);
end;

{ TMapWay }

function TMapWay.GetObjectFileRef(): TObjectFileRef;
begin
  Result.Offset := FileOffset;
  Result.RefType := refWay;
end;

function TMapWay.GetType(): TTypeInfo;
begin
  Result := FeatureValueBuffer.GetType();
end;

function TMapWay.GetFeatureCount(): Integer;
begin
  Result := FeatureValueBuffer.TypeInfo.FeatureCount;
end;

function TMapWay.HasFeature(AIndex: Integer): Boolean;
begin
  Result := FeatureValueBuffer.HasFeatureValue(AIndex);
end;

function TMapWay.GetFeature(AIndex: Integer): TFeatureInfo;
begin
  Result := FeatureValueBuffer.TypeInfo.GetFeatureInfo(AIndex);
end;

procedure TMapWay.UnsetFeature(AIndex: Integer);
begin
  FeatureValueBuffer.FreeValue(AIndex);
end;

function TMapWay.IsCircular(): Boolean;
begin
  Result := (GetBackId() <> 0) and (GetBackId() = GetFrontId());
end;

function TMapWay.GetSerial(AIndex: Integer): TId;
begin
  //Result := Nodes[AIndex].Serial;
  Result := 0;
end;

function TMapWay.GetId(AIndex: Integer): TId;
begin
  Result := Nodes[AIndex].GetId();
end;

function TMapWay.GetFrontId(): TId;
begin
  if Length(Nodes) > 0 then
    Result := Nodes[0].GetId()
  else
    Result := 0;
end;

function TMapWay.GetBackId(): TId;
begin
  if Length(Nodes) > 0 then
    Result := Nodes[Length(Nodes)-1].GetId()
  else
    Result := 0;
end;

function TMapWay.GetPoint(AIndex: Integer): TGeoPoint;
begin
  Result := Nodes[AIndex];
end;

function TMapWay.GetCoord(AIndex: Integer): TGeoPoint;
begin
  Result := Nodes[AIndex];
end;

function TMapWay.GetBoundingBox(): TGeoBox;
begin
  if (not FBBox.Valid) and (Length(Nodes) <> 0) then
    FBBox.InitForPoints(Nodes);
  Result.Assign(FBBox)
end;

function TMapWay.Intersects(const ABoundingBox: TGeoBox): Boolean;
begin
  Result := GetBoundingBox().IsIntersects(ABoundingBox);
end;

function TMapWay.GetCenter(var ACenter: TGeoPoint): Boolean;
var
  MinCoord, MaxCoord: TGeoPoint;
  i: Integer;
begin
  Result := False;
  if Length(Nodes) = 0 then
    Exit;

  MinCoord.Assign(Nodes[0]);
  MaxCoord.Assign(Nodes[0]);

  for i := 1 to Length(Nodes)-1 do
  begin
    MinCoord.AssignMin(Nodes[i]);
    MaxCoord.AssignMax(Nodes[i]);
  end;

  ACenter.Init(MinCoord.Lat + (MaxCoord.Lat - MinCoord.Lat) / 2,
               MinCoord.Lon + (MaxCoord.Lon - MinCoord.Lon) / 2);

  Result := True;
end;

function TMapWay.GetNodeIndexByNodeId(AId: TId; out AIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(Nodes)-1 do
  begin
    if Nodes[i].GetId() = AId then
    begin
      AIndex := i;
      Result := True;
      Break;
    end;
  end;
end;

procedure TMapWay.SetType(const AValue: TTypeInfo);
begin
  ResetMapItemDrawOptions(DrawOptions);
  FeatureValueBuffer.SetType(AValue);
end;

procedure TMapWay.ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner;
  ADataMode: TNodeDataMode);
var
  TypeId: TTypeId;
  TypeInfo: TTypeInfo;
  IsUseIds: Boolean;
  DebugPos: Int64;
begin
  FileOffset := AScanner.Stream.Position;
  AScanner.ReadTypeId(TypeId, ATypeConfig.WayTypeIdBytes);
  TypeInfo := ATypeConfig.GetWayTypeInfo(TypeId);
  FeatureValueBuffer.SetType(TypeInfo);
  FeatureValueBuffer.Read(AScanner);

  DebugPos := AScanner.Stream.Position;
  case ADataMode of
    ndmAuto: IsUseIds := (TypeInfo.CanRoute or TypeInfo.OptimizeLowZoom);
    ndmAll:  IsUseIds := True;
  else
    // ndmNone:
    IsUseIds := False;
  end;

  AScanner.ReadMapPoints(Nodes, Segments, FBBox, IsUseIds);

  NextFileOffset := AScanner.Stream.Position;
end;

procedure TMapWay.Read(const ATypeConfig: TTypeConfig; AScanner: TFileScanner);
begin
  ReadData(ATypeConfig, AScanner, ndmAuto);
end;

procedure TMapWay.ReadOptimized(ATypeConfig: TTypeConfig; AScanner: TFileScanner);
begin
  ReadData(ATypeConfig, AScanner, ndmNone);
end;

procedure TMapWay.WriteData(ATypeConfig: TTypeConfig; AWriter: TFileWriter;
  ADataMode: TNodeDataMode);
var
  IsUseIds: Boolean;
  //DebugPos: Int64;
begin
  Assert(Length(Nodes) > 0);

  //DebugPos := AWriter.Stream.Position;
  AWriter.WriteTypeId(FeatureValueBuffer.TypeInfo.WayId,
                      ATypeConfig.WayTypeIdBytes);

  FeatureValueBuffer.Write(AWriter);
  //DebugPos := AWriter.Stream.Position;

  case ADataMode of
    ndmAuto: IsUseIds := (FeatureValueBuffer.TypeInfo.CanRoute or FeatureValueBuffer.TypeInfo.OptimizeLowZoom);
    ndmAll:  IsUseIds := True;
  else
    // ndmNone:
    IsUseIds := False;
  end;

  AWriter.WriteMapPoints(Nodes, IsUseIds);
end;

procedure TMapWay.Write(const ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  WriteData(ATypeConfig, AWriter, ndmAuto);
end;

procedure TMapWay.WriteOptimized(ATypeConfig: TTypeConfig; AWriter: TFileWriter);
begin
  WriteData(ATypeConfig, AWriter, ndmNone);
end;

{ TGroundTileCoord }

procedure TGroundTileCoord.SetValue(AX, AY: Word; AIsCoast: Boolean);
begin
  X := AX;
  Y := AY;
  IsCoast := AIsCoast;
end;

function TGroundTileCoord.IsEqual(const AValue: TGroundTileCoord): Boolean;
begin
  Result := (X = AValue.X) and (Y = AValue.Y) and (IsCoast = AValue.IsCoast);
end;

{ TMapData }

procedure TMapData.AfterConstruction();
begin
  inherited AfterConstruction();
  FNodeList := TMapNodeList.Create();
  FAreaList := TMapAreaList.Create();
  FWayList := TMapWayList.Create();
  FPoiNodeList := TMapNodeList.Create();
  FPoiAreaList := TMapAreaList.Create();
  FPoiWayList := TMapWayList.Create();
  //FGroundTileList := TGroundTileList.Create();
end;

procedure TMapData.BeforeDestruction();
begin
  //FreeAndNil(FGroundTileList);
  FreeAndNil(FPoiWayList);
  FreeAndNil(FPoiAreaList);
  FreeAndNil(FPoiNodeList);
  FreeAndNil(FWayList);
  FreeAndNil(FAreaList);
  FreeAndNil(FNodeList);
  inherited BeforeDestruction();
end;

procedure TMapData.ClearDbData();
begin
  FNodeList.Clear();
  FAreaList.Clear();
  FWayList.Clear();
  FPoiNodeList.Clear();
  FPoiAreaList.Clear();
  FPoiWayList.Clear();
end;

end.

