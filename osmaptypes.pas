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
  OsMap types
system\OSMScoutTypes
util\Distance
util\Magnification
ObjectRef

Pixel:
  Pixel
  Vertex2D
  Vertex3D

*)
unit OsMapTypes;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Math;

const
  { Magnification levels }
  MAG_LEVEL_WORLD      = 0;
  MAG_LEVEL_CONTINENT  = 4;
  MAG_LEVEL_STATE      = 5;
  MAG_LEVEL_STATE_OVER = 6;
  MAG_LEVEL_COUNTY     = 7;
  MAG_LEVEL_REGION     = 8;
  MAG_LEVEL_PROXIMITY  = 9;
  MAG_LEVEL_CITY_OVER  = 10;
  MAG_LEVEL_CITY       = 11;
  MAG_LEVEL_SUBURB     = 12;
  MAG_LEVEL_DETAIL     = 13;
  MAG_LEVEL_CLOSE      = 14;
  MAG_LEVEL_CLOSER     = 15;
  MAG_LEVEL_VERY_CLOSE = 16;
  MAG_LEVEL_BLOCK      = 18;
  MAG_LEVEL_STREET     = 19;
  MAG_LEVEL_HOUSE      = 20;

  { Vehicle types }
  VEHICLE_FOOT    = 1 shl 0;
  VEHICLE_BICYCLE = 1 shl 1;
  VEHICLE_CAR     = 1 shl 2;

type
  // Type to be used for OSM ids (signed numbers with 64 bit size).
  TOsmId = Int64;
  // Type to be used for libosmscout internal ids (unsigned numbers with 64 bit size).
  TId = UInt64;
  TIdArray = array of TId;

  TPageId = UInt64;
  // Type for describing the position of data within a file.
  TFileOffset = Int64;
  TFileOffsetArray = array of TFileOffset;
  // Type for describing a type of an way, area or node.
  TTypeId = Word;
  // Feature value ID (string ID)
  TFeatureValueId = Integer;

  TFeatureValue = string;
  TFeatureValueList = array of TFeatureValueId;

  TFeatureType = (
    ftNone,
    ftName,               // name, label
    ftNameAlt,            // alternative name
    ftRef,                // tags: ref
    ftLocation,           // tags: addr:housenumber, addr:street, addr:place
    ftAddress,            // tags: addr:housenumber, addr:street, addr:place
    ftAccess,
    ftAccessRestricted,
    ftLayer,              // tags: layer
    ftWidth,              // tags: width
    ftMaxSpeed,           // tags: maxspeed
    ftGrade,              // tags: surface, tracktype
    ftAdminLevel,         // tags: admin_level, is_in
    ftPostalCode,         // tags: postal_code, addr:postcode
    ftBridge,             // tags: bridge
    ftTunnel,             // tags: tunnel
    ftEmbankment,         // tags: embankment
    ftRoundabout,         // tags: junction
    ftEle,                // tags: ele
    ftDestination,        // tags: destination, destination:ref, destination:forward
    ftBuilding,           // tags: building (yes, residential, hospital, etc..)
    ftWebsite,            // tags: website
    ftPhone,              // tags: phone
    ftIsIn,               // tags: is_in
    ftConstructionYear,
    ftSideway,
    ftLanes
  );

  TVehicleType = (vehicleFoot, vehicleBicycle, vehicleCar);
  TVehicleTypes = set of TVehicleType;

  { from VEHICLE_ constants }
  TVehicleMask = Byte;

  TReal = Real;
  TSingle = Single;

  // distance in meters
  TDistance = TReal;

  TMagnificationLevel = LongWord;

  { TMagnification }

  TMagnification = object
  private
    FLevel: TMagnificationLevel;
    FMagnification: TReal;
    procedure SetLevel(ALevel: TMagnificationLevel);
    procedure SetMagnification(AMagnification: TReal);
  public
    { Magnification level MAG_LEVEL_ }
    property Level: TMagnificationLevel read FLevel write SetLevel;
    property Magnification: TReal read FMagnification write SetMagnification;

    function IsEqual(const AValue: TMagnification): Boolean;
    function IsGreater(const AValue: TMagnification): Boolean;
    procedure Assign(const AValue: TMagnification);

    procedure Inc();
  end;

  { TMagnificationConverter }

  TMagnificationConverter = object
  public
    function Convert(const AName: string; var AMagnification: TMagnification): Boolean; overload;
    function Convert(const ALevel: TMagnificationLevel; var AName: string): Boolean; overload;
  end;

  TOsmRefType = (osmRefNone, osmRefNode, osmRefWay, osmRefRelation);

  TRefType = (refNone, refNode, refArea, refWay);

  { Reference to an OSM object by its type (Node, Way, Relation) and its
    OSM object id. }
  TObjectOSMRef = object
  public
    Id: TOsmId;
    RefType: TOsmRefType;

    procedure Assign(const AValue: TObjectOSMRef);
    procedure SetValue(AId: TOsmId; ARefType: TOsmRefType);
    procedure Invalidate();
    function IsValid(): Boolean;

    function GetName(): string;
    function GetTypeName(): string;
  end;

  { Reference to an libosmscout internal object by its type (area, way, node)
    and by its file offset within its data file. }

  { TObjectFileRef }

  TObjectFileRef = object
  public
    Offset: TFileOffset;
    RefType: TRefType;

    procedure Assign(const AValue: TObjectFileRef);
    procedure SetValue(AOffset: TFileOffset; ARefType: TRefType);
    procedure Invalidate();
    function IsValid(): Boolean;
    function IsEqual(const AValue: TObjectFileRef): Boolean;

    function GetName(): string;
    function GetTypeName(): string;
  end;

  TObjectFileRefArray = array of TObjectFileRef;

  { Representation of a pixel on a display or a plane.
    Coordinates are non-negative, values are decimal. }
  TPixel = object
    X: LongWord;
    Y: LongWord;

    procedure SetValue(AX, AY: LongWord);
    procedure Assign(AOther: TPixel);
    function IsEqual(AOther: TPixel): Boolean;
    { Returns a unique number based on the coordinates of the pixel. The bits of the coordinates
      are projected onto one number by interleaving the bits of the coordinates. Coordinates close
      in 2D space are thus likely clos ein one dimensional space, too. }
    function GetId(): TId;

    function  GetDisplayText(): string;
  end;

  { Two dimensional coordinate (floating point values,
    negative coordinates possible). }
  TVertex2D = object
    X: TReal;
    Y: TReal;

    procedure SetValue(AX, AY: TReal);
    procedure Assign(AOther: TVertex2D);
    function IsEqual(AOther: TVertex2D): Boolean;

    function DistanceTo(AOther: TVertex2D): TReal;
  end;

  TVertex2DArray = array of TVertex2D;

  { Three dimensional coordinate (floating point values,
    negative coordinates possible). }
  TVertex3D = object
    X: TReal;
    Y: TReal;
    Z: TReal;

    procedure SetValue(AX, AY, AZ: TReal);
    procedure Assign(AOther: TVertex3D);
    function IsEqual(AOther: TVertex3D): Boolean;
  end;

  { Returns TMagnification for given level (from MAG_LEVEL_ ) }
  function Magnification(ALevel: TMagnificationLevel): TMagnification;

  { Returns TObjectOSMRef for given OSM object ID and ref type (Node, Way, Relation) }
  function ObjectOSMRef(AId: TOsmId; ARefType: TOsmRefType): TObjectOSMRef;

  { Returns TObjectFileRef for given file offset and object type (area, way, node) }
  function ObjectFileRef(AOffset: TFileOffset; ARefType: TRefType): TObjectFileRef;

  function Vertex2D(AX, AY: TReal): TVertex2D; inline;

  function VehicleTypesToMask(AValue: TVehicleTypes): TVehicleMask;
  function VehicleMaskToTypes(AValue: TVehicleMask): TVehicleTypes;

implementation

uses OsMapUtils;

function Magnification(ALevel: TMagnificationLevel): TMagnification;
begin
  Result.SetLevel(ALevel);
end;

function ObjectOSMRef(AId: TOsmId; ARefType: TOsmRefType): TObjectOSMRef;
begin
  Result.SetValue(AId, ARefType);
end;

function ObjectFileRef(AOffset: TFileOffset; ARefType: TRefType): TObjectFileRef;
begin
  Result.SetValue(AOffset, ARefType);
end;

function Vertex2D(AX, AY: TReal): TVertex2D;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function VehicleTypesToMask(AValue: TVehicleTypes): TVehicleMask;
begin
  Result := 0;
  if vehicleFoot in AValue then
    Result := Result or VEHICLE_FOOT;
  if vehicleBicycle in AValue then
    Result := Result or VEHICLE_BICYCLE;
  if vehicleCar in AValue then
    Result := Result or VEHICLE_CAR;
end;

function VehicleMaskToTypes(AValue: TVehicleMask): TVehicleTypes;
begin
  Result := [];
  if (AValue and VEHICLE_FOOT) <> 0 then
    Result := Result + [vehicleFoot];
  if (AValue and VEHICLE_BICYCLE) <> 0 then
    Result := Result + [vehicleBicycle];
  if (AValue and VEHICLE_CAR) <> 0 then
    Result := Result + [vehicleCar];
end;

{ TMagnificationConverter }

function TMagnificationConverter.Convert(const AName: string;
  var AMagnification: TMagnification): Boolean;
begin
  Result := True;
  // 'case <string> of' not supported by Delphi
  if AName = 'world' then
    AMagnification.SetLevel(MAG_LEVEL_WORLD)
  else if AName = 'continent' then
    AMagnification.SetLevel(MAG_LEVEL_CONTINENT)
  else if AName = 'state' then
    AMagnification.SetLevel(MAG_LEVEL_STATE)
  else if AName = 'stateOver' then
    AMagnification.SetLevel(MAG_LEVEL_STATE_OVER)
  else if AName = 'county' then
    AMagnification.SetLevel(MAG_LEVEL_COUNTY)
  else if AName = 'region' then
    AMagnification.SetLevel(MAG_LEVEL_REGION)
  else if AName = 'proximity' then
    AMagnification.SetLevel(MAG_LEVEL_PROXIMITY)
  else if AName = 'cityOver' then
    AMagnification.SetLevel(MAG_LEVEL_CITY_OVER)
  else if AName = 'city' then
    AMagnification.SetLevel(MAG_LEVEL_CITY)
  else if AName = 'suburb' then
    AMagnification.SetLevel(MAG_LEVEL_SUBURB)
  else if AName = 'detail' then
    AMagnification.SetLevel(MAG_LEVEL_DETAIL)
  else if AName = 'close' then
    AMagnification.SetLevel(MAG_LEVEL_CLOSE)
  else if AName = 'closer' then
    AMagnification.SetLevel(MAG_LEVEL_CLOSER)
  else if AName = 'veryClose' then
    AMagnification.SetLevel(MAG_LEVEL_VERY_CLOSE)
  else if AName = 'block' then
    AMagnification.SetLevel(MAG_LEVEL_BLOCK)
  else if AName = 'street' then
    AMagnification.SetLevel(MAG_LEVEL_STREET)
  else if AName = 'house' then
    AMagnification.SetLevel(MAG_LEVEL_HOUSE)
  else
    Result := False;
end;

function TMagnificationConverter.Convert(const ALevel: TMagnificationLevel;
  var AName: string): Boolean;
begin
  Result := True;
  case ALevel of
    MAG_LEVEL_WORLD:      AName := 'world';
    MAG_LEVEL_CONTINENT:  AName := 'continent';
    MAG_LEVEL_STATE:      AName := 'state';
    MAG_LEVEL_STATE_OVER: AName := 'stateOver';
    MAG_LEVEL_COUNTY:     AName := 'county';
    MAG_LEVEL_REGION:     AName := 'region';
    MAG_LEVEL_PROXIMITY:  AName := 'proximity';
    MAG_LEVEL_CITY_OVER:  AName := 'cityOver';
    MAG_LEVEL_CITY:       AName := 'city';
    MAG_LEVEL_SUBURB:     AName := 'suburb';
    MAG_LEVEL_DETAIL:     AName := 'detail';
    MAG_LEVEL_CLOSE:      AName := 'close';
    MAG_LEVEL_CLOSER:     AName := 'closer';
    MAG_LEVEL_VERY_CLOSE: AName := 'veryClose';
    MAG_LEVEL_BLOCK:      AName := 'block';
    MAG_LEVEL_STREET:     AName := 'street';
    MAG_LEVEL_HOUSE:      AName := 'house';
  else
    Result := False;
  end;
end;

{ TMagnification }

function TMagnification.IsEqual(const AValue: TMagnification): Boolean;
begin
  Result := (AValue.Magnification = FMagnification) and (AValue.Level = FLevel);
end;

function TMagnification.IsGreater(const AValue: TMagnification): Boolean;
begin
  Result := (FMagnification > AValue.Magnification) or (FLevel > AValue.Level);
end;

procedure TMagnification.Assign(const AValue: TMagnification);
begin
  FMagnification := AValue.Magnification;
  FLevel := AValue.Level;
end;

procedure TMagnification.SetMagnification(AMagnification: TReal);
begin
  FMagnification := AMagnification;
  FLevel := Trunc(log2(AMagnification));
end;

procedure TMagnification.SetLevel(ALevel: TMagnificationLevel);
begin
  FMagnification := power(2.0, ALevel);
  FLevel := ALevel;
end;

procedure TMagnification.Inc();
begin
  FMagnification := FMagnification * 2.0;
  FLevel := FLevel + 1;
end;

{ TObjectFileRef }

procedure TObjectFileRef.Assign(const AValue: TObjectFileRef);
begin
  Offset := AValue.Offset;
  RefType := AValue.RefType;
end;

procedure TObjectFileRef.SetValue(AOffset: TFileOffset; ARefType: TRefType);
begin
  Offset := AOffset;
  RefType := ARefType;
end;

procedure TObjectFileRef.Invalidate();
begin
  Offset := 0;
  RefType := refNone;
end;

function TObjectFileRef.IsValid(): Boolean;
begin
  Result := (RefType <> refNone);
end;

function TObjectFileRef.IsEqual(const AValue: TObjectFileRef): Boolean;
begin
  Result := (AValue.Offset = Offset) and (AValue.RefType = RefType);
end;

function TObjectFileRef.GetName(): string;
begin
  Result := GetTypeName() + ' ' + IntToStr(Offset);
end;

function TObjectFileRef.GetTypeName(): string;
begin
  case RefType of
    refNode:     Result := 'Node';
    refArea:     Result := 'Area';
    refWay:      Result := 'Way';
  else
    Result := 'none';
  end;
end;

{ TObjectOsmRef }

procedure TObjectOSMRef.Assign(const AValue: TObjectOSMRef);
begin
  Id := AValue.Id;
  RefType := AValue.RefType;
end;

procedure TObjectOSMRef.SetValue(AId: TOsmId; ARefType: TOsmRefType);
begin
  Id := AId;
  RefType := ARefType;
end;

procedure TObjectOSMRef.Invalidate();
begin
  Id := 0;
  RefType := osmRefNone;
end;

function TObjectOSMRef.IsValid(): Boolean;
begin
  Result := (RefType <> osmRefNone);
end;

function TObjectOSMRef.GetName(): string;
begin
  Result := GetTypeName() + ' ' + IntToStr(Id);
end;

function TObjectOSMRef.GetTypeName(): string;
begin
  case RefType of
    osmRefNode:     Result := 'Node';
    osmRefWay:      Result := 'Way';
    osmRefRelation: Result := 'Relation';
  else
    Result := 'none';
  end;
end;

{ TPixel }

procedure TPixel.SetValue(AX, AY: LongWord);
begin
  X := AX;
  Y := AY;
end;

procedure TPixel.Assign(AOther: TPixel);
begin
  X := AOther.X;
  Y := AOther.Y;
end;

function TPixel.IsEqual(AOther: TPixel): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y);
end;

function TPixel.GetId(): TId;
begin
  Result := InterleaveNumbers(X, Y);
end;

function TPixel.GetDisplayText(): string;
begin
  Result := IntToStr(X) + ',' + IntToStr(Y);
end;

{ TVertex2D }

procedure TVertex2D.SetValue(AX, AY: TReal);
begin
  X := AX;
  Y := AY;
end;

procedure TVertex2D.Assign(AOther: TVertex2D);
begin
  X := AOther.X;
  Y := AOther.Y;
end;

function TVertex2D.IsEqual(AOther: TVertex2D): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y);
end;

function TVertex2D.DistanceTo(AOther: TVertex2D): TReal;
var
  XD, YD: TReal;
begin
  XD := X - AOther.X;
  YD := Y - AOther.Y;
  Result := sqrt((XD * XD) + (YD * YD));
end;

{ TVertex3D }

procedure TVertex3D.SetValue(AX, AY, AZ: TReal);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;

procedure TVertex3D.Assign(AOther: TVertex3D);
begin
  X := AOther.X;
  Y := AOther.Y;
  Z := AOther.Z;
end;

function TVertex3D.IsEqual(AOther: TVertex3D): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y) and (Z = AOther.Z);
end;


end.

