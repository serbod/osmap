(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2020  Sergey Bodrov

  This source is ported from libosmscout library
  Copyright (C) 2015  Tim Teulings

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
Classes and functions for storing and indexing of OSM sets of data 
of the same type in a tile.

TileId:
  TileId -> TTileId, GetTileId()
  TileIdBoxConstIterator -> TTileIdBoxConstIterator
  TileIdBox -> TTileIdBox

DataTileCache:
  TileData -> TTileData ...


*)
unit OsMapDataTile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  OsMapTypes,  OsMapGeometry;

type
  { A Tile id, uniquely identifing a tile by its level and (unique for the given level) tile coordinates.

    Note that this are libosmscout tiles, that evenly split the whole world into - for each level -
    equaly sized (regarding their latitude and longitude interval) rectangles.

    Classic OSM tiles are calculated differently. }

  { TTileId }

  TTileId = object
    X: UInt32;  // The x coordinate of the tile in relation to the zoom level
    Y: UInt32;  // The y coordinate of the tile in relation to the zoom level

    function AsPixel(): TPixel;
    function IsEqual(AOther: TTileId): Boolean;

    { Return a short human readable description of the tile id }
    function GetDisplayText(): string;

    { Return the top left coordinate of the tile
      AMagnification - Magnification to complete the definition of the tile id (these are relative
      to a magnification) }
    function GetTopLeftCoord(const AMagnification: TMagnification): TGeoPoint;

    { Return the bounding box of the given tile for a given magnification level }
    function GetBoundingBox(const ALevel: TMagnificationLevel): TGeoBox;
    function GetBoundingBox(const AMagnification: TMagnification): TGeoBox;
  end;

function GetTileId(AX, AY: UInt32): TTileId;

{ Return the libosmscout-specific tile id for the given magnification that contains the given
  coordinates.
  AMagnification - Magnification to use
  ACoord - Coordinate that should be covered by the tile }
function GetTileId(const AMagnification: TMagnification; const ACoord: TGeoPoint): TTileId;
{ Return the libosmscout-specific tile id for the given magnification that contains the given
  coordinates.
  ALevel - Level to use (magnification.GetLevel())
  ACoord - Coordinate that should be covered by the tile }
function GetTileId(const ALevel: TMagnificationLevel; const ACoord: TGeoPoint): TTileId;


type

  { TTileIdBoxConstIterator }

  TTileIdBoxConstIterator = object
    CurrentTile: TTileId;
    MinTile: TTileId;
    MaxTile: TTileId;

    procedure Init(const ACur, AMin, AMax: TTileId);
    { Set next tile as CurrentTile, return False if there is no next tile }
    function Next(): Boolean;
    { Return CurrentTile and set next tile as CurrentTile, return False if there is no next tile }
    function GetNext(var ACurrentTile: TTileId): Boolean;
  end;

  { A bounding box defined by two tile ids that span a rectangular region (in
    tile coordinate system) }
  TTileIdBox = object
    MinTile: TTileId;
    MaxTile: TTileId;

    property MinX: UInt32 read MinTile.X;
    property MinY: UInt32 read MinTile.Y;
    property MaxX: UInt32 read MaxTile.X;
    property MaxY: UInt32 read MaxTile.Y;

    function GetWidth(): Integer;
    function GetHeight(): Integer;
    function GetCount(): Integer;

    function GetDisplayText(): string;
  end;


function GetTileIdBox(const A, B: TTileId): TTileIdBox;

implementation

function GetTileId(AX, AY: UInt32): TTileId;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function GetTileId(const AMagnification: TMagnification; const ACoord: TGeoPoint): TTileId;
begin
  Result.X := Trunc((ACoord.Lon + 180.0) / CellDimensions[AMagnification.Level].Width);
  Result.Y := Trunc((ACoord.Lat + 90.0) / CellDimensions[AMagnification.Level].Height);
end;

function GetTileId(const ALevel: TMagnificationLevel; const ACoord: TGeoPoint): TTileId;
begin
  Result.X := Trunc((ACoord.Lon + 180.0) / CellDimensions[ALevel].Width);
  Result.Y := Trunc((ACoord.Lat + 90.0) / CellDimensions[ALevel].Height);
end;

function GetTileIdBox(const A, B: TTileId): TTileIdBox;
begin
  Result.MinTile.X := Min(A.X, B.X);
  Result.MinTile.Y := Min(A.Y, B.Y);
  Result.MaxTile.X := Max(A.X, B.X);
  Result.MaxTile.Y := Max(A.Y, B.Y);
end;

{ TTileId }

function TTileId.AsPixel(): TPixel;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TTileId.IsEqual(AOther: TTileId): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y);
end;

function TTileId.GetDisplayText(): string;
begin
  Result := Format('%d.%d', [y, x]);
end;

function TTileId.GetTopLeftCoord(const AMagnification: TMagnification): TGeoPoint;
begin
  Result.Lat := y * CellDimensions[AMagnification.Level].Height - 90.0;
  Result.Lon := x * CellDimensions[AMagnification.Level].Width - 180.0;
end;

function TTileId.GetBoundingBox(const ALevel: TMagnificationLevel): TGeoBox;
begin
  Result.MinCoord.Lat := y * CellDimensions[ALevel].Height - 90.0;
  Result.MinCoord.Lon := x * CellDimensions[ALevel].Width - 180.0;

  Result.MaxCoord.Lat := (y+1) * CellDimensions[ALevel].Height - 90.0;
  Result.MaxCoord.Lon := (x+1) * CellDimensions[ALevel].Width - 180.0;
end;

function TTileId.GetBoundingBox(const AMagnification: TMagnification): TGeoBox;
begin
  Result := GetBoundingBox(AMagnification.Level);
end;

{ TTileIdBoxConstIterator }

procedure TTileIdBoxConstIterator.Init(const ACur, AMin, AMax: TTileId);
begin
  CurrentTile := ACur;
  MinTile := AMin;
  MaxTile := AMax;
end;

function TTileIdBoxConstIterator.Next(): Boolean;
begin
  Result := (not CurrentTile.IsEqual(MaxTile));
  if Result then
  begin
    if (CurrentTile.X >= MaxTile.X) then
      CurrentTile := GetTileId(minTile.X, CurrentTile.Y+1)
    else
      CurrentTile := GetTileId(CurrentTile.X+1, CurrentTile.Y);
  end;
end;

function TTileIdBoxConstIterator.GetNext(var ACurrentTile: TTileId): Boolean;
begin
  ACurrentTile := CurrentTile;
  Result := Next();
end;

function TTileIdBox.GetWidth(): Integer;
begin
  Result := MaxTile.X - MinTile.X + 1;
end;

function TTileIdBox.GetHeight(): Integer;
begin
  Result := MaxTile.Y - MinTile.Y + 1;
end;

function TTileIdBox.GetCount(): Integer;
begin
  Result := GetWidth() * GetHeight();
end;

function TTileIdBox.GetDisplayText(): string;
begin
  Result := Format('[%s - %s]', [MinTile.GetDisplayText(), MaxTile.GetDisplayText]);
end;


end.

