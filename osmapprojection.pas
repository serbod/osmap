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
util\Projection
  Projection
  MercatorProjection
  TileProjection
*)
unit OsMapProjection;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, OsMapTypes, OsMapGeometry;

type
  TProjection = class;

  { This class is used to hide internal complexity concerned with batching GeoToPixel calls }

  { TBatchTransformer }

  TBatchTransformer = object
  public
    Projection: TProjection;
    {$ifdef OSMSCOUT_HAVE_SSE2}
    Count: Integer;
    lat: array[2] of Double;
    lon: array[2] of Double;
    xPointer[2] array[2] of PDouble;
    yPointer[2] array[2] of PDouble;
    {$endif}

    procedure GeoToPixel(ALat: TLatitude; ALon: TLongtitude; var x, y: Double);
  end;

  { The Projection class is an abstract base class for multiple projection implementations.

    The Projection class allows transformation of geo coordinates to screen/image coordinates and
    screen/image coordinates back to geo coordinates. }

  { TProjection }

  TProjection = class
  protected
    FLon: Double;           // Longitude coordinate of the center of the image
    FLat: Double;           // Latitude coordinate of the center of the image
    FAngle: Double;         // Display rotation angle in radians
    FMagnification: TMagnification;  // Current magnification
    FDpi: Double;           // Screen DPI
    FWidth: Integer;        // Width of image
    FHeight: Integer;       // Height of image

    LonMin: Double;         // Longitude of the upper left corner of the image
    LatMin: Double;         // Latitude of the upper left corner of the image
    LonMax: Double;         // Longitude of the lower right corner of the image
    LatMax: Double;         // Latitude of the lower right corner of the image

    FPixelSize: Double;     // Size of a pixel in meter
    FMeterInPixel: Double;  // Number of on screen pixel for one meter on the ground
    FMeterInMM: Double;     // Number of on screen millimeters for one meter on the ground

    procedure GeoToPixelBath(ATransformData: TBatchTransformer); virtual; abstract;
  public
    procedure AfterConstruction(); override;

    function CanBatch(): Boolean; virtual; abstract;
    function IsValid(): Boolean; virtual; abstract;

    { Return True if given coordinate is valid for this projection }
    function IsValidFor(const ACoord: TGeoPoint): Boolean; virtual; abstract;

    function GetCenter(): TGeoPoint;

    { Returns True, if the given geo coordinate is in the bounding box }
    function GeoCoordIsIn(ALat: TLatitude; ALon: TLongtitude): Boolean;
    { Returns true, if the given bounding box is completely within the projection bounding box }
    function GeoBoxIsIn(const AGeoBox: TGeoBox): Boolean; overload;
    { Returns True if pixel with given X.Y is in visible area with ATolerance }
    function IsPixelValid(AX, AY, ATolerance: Double): Boolean;

    { Returns the bounding box of the area covered }
    function GetDimensions(): TGeoBox;
    procedure FillDimensions(var AValue: TGeoBox);

    { Convert a width in mm into the equivalent pixel size based on the given DPI
      AValue - Width in mm
      Result - Width in screen pixel }
    function ConvertWidthToPixel(AValue: Double): Double;
    { Convert a width in pixel into the equivalent mm size based on the given DPI
      AValue - Width in screen pixel
      Result - Width in mm }
    function ConvertPixelToWidth(AValue: Double): Double;

    { Converts a pixel coordinate to a geo coordinate. Return True on success,
      or False if returned coordinate is not valid for this projection. }
    function PixelToGeo(X, Y: Double; out ACoord: TGeoPoint): Boolean; virtual; abstract;
    { Converts a geo coordinate to a pixel coordinate. Return True on success,
      or False if given coordinate is not valid for this projection. }
    function GeoToPixel(const ACoord: TGeoPoint; out X, Y: Double): Boolean; virtual; abstract;

    { longitude coordinate of the region center }
    property Lon: Double read FLon;
    { latitude coordinate of the region center }
    property Lat: Double read FLat;
    { angle in radians ([0..2*PI[) of the display in relation to the north. A degree of 0 means
      north is to the top, a degree of PI, renders with the south to the top of the display) }
    property Angle: Double read FAngle;
    { Current magnification }
    property Magnification: TMagnification read FMagnification;
    { Screen DPI as part of the projection }
    property Dpi: Double read FDpi;
    { width of the screen, in pixels }
    property Width: Integer read FWidth;
    { height of the screen, in pixels }
    property Height: Integer read FHeight;
    { Size of a pixel in meter }
    property PixelSize: Double read FPixelSize;
    { Number of on screen pixel for one meter on the ground }
    property MeterInPixel: Double read FMeterInPixel;
    { Number of on screen millimeters for one meter on the ground }
    property MeterInMM: Double read FMeterInMM;
  end;

  { Mercator projection that tries to render the resulting map in the same
    physical size on all devices. If the physical DPI of the device is
    correctly given, objects on any device has the same size. Bigger devices
    will show "more" map thus.

    Scale is calculated based on the assumption that the original OpenStreetMap
    tiles were designed for 96 DPI displays. }
  TMercatorProjection = class(TProjection)
  protected
    FMaxLat: Double;
    FMinLat: Double;
    FMaxLon: Double;
    FMinLon: Double;

    FIsValid: Boolean; //  projection is valid
    FLatOffset: Double;  // Absolute and untransformed screen position of lat coordinate
    FAngleSin: Double;
    FAngleCos: Double;
    FAngleNegSin: Double;
    FAngleNegCos: Double;

    FScale: Double;
    FScaleGradtorad: Double; // Precalculated scale*Gradtorad

    // precalculated derivation of "latToYPixel" function in projection center scaled by gradtorad * scale
    FScaledLatDeriv: Double;
    FIsLinearInterpolationEnabled: Boolean;

    procedure GeoToPixelBath(ATransformData: TBatchTransformer); override;

  public
    procedure AfterConstruction(); override;

    function CanBatch(): Boolean; override;
    function IsValid(): Boolean; override;
    function IsValidFor(const ACoord: TGeoPoint): Boolean; override;

    function PixelToGeo(X, Y: Double; out ACoord: TGeoPoint): Boolean; override;
    function GeoToPixel(const ACoord: TGeoPoint; out X, Y: Double): Boolean; override;

    { Setup projection parameters. Return True on success,
      or False if arguments are not valid for Mercator projection,
      projection parameters are unchanged in such case.

      Angle is in radians ([0..2*PI[)

      Note that coord (center) have to be valid coordinate
      in Mercator projection. But it is possible setup dimensions
      (width and height) that projection will cover area bigger
      than the one valid for Mercator projection. Bounding box
      is adjusted then to be valid for projection.

      In code:

        Projection.GetDimensions(bbox);
        Projection.GeoToPixel(bbox.GetMinCoord(),x,y)

      may be x >= 0 }
    function Setup(const ACoord: TGeoPoint;
                   AAngle: Double;
                   const AMagnification: TMagnification;
                   ADpi: Double;
                   AWidth, AHeight: Integer): Boolean;

    function Move(AHorizPixel, AVertPixel: Double): Boolean;
    function MoveUp(APixel: Double): Boolean;
    function MoveDown(APixel: Double): Boolean;
    function MoveLeft(APixel: Double): Boolean;
    function MoveRight(APixel: Double): Boolean;

    property MaxLat: Double read FMaxLat;
    property MinLat: Double read FMinLat;
    property MaxLon: Double read FMaxLon;
    property MinLon: Double read FMinLon;

    { Switch to enable/disable linear interpolation of latitude to pixel computation.
      It speedup GeoToPixel calculation with fractional error on small render area. }
    property IsLinearInterpolationEnabled: Boolean read FIsLinearInterpolationEnabled write FIsLinearInterpolationEnabled;
  end;

  { Mercator projection as used by the OpenStreetMap tile rendering code.

    The TileProjection simplifies the general Mercator projection code to
    make sure that there are no effects based on rounding errors or similar. }
  TTileProjection = class(TProjection)
  protected
    FIsValid: Boolean; //  projection is valid
    FLonOffset: Double;
    FLatOffset: Double;

    FScale: Double;
    // Precalculated scale*Gradtorad
    FScaleGradtorad: Double;
    // precalculated derivation of "latToYPixel" function in projection center scaled by gradtorad * scale
    FScaledLatDeriv: Double;
    FIsLinearInterpolationEnabled: Boolean;

    procedure GeoToPixelBath(ATransformData: TBatchTransformer); override;

    function SetupInternal(ALonMin, ALatMin, ALonMax, ALatMax: Double;
                   const AMagnification: TMagnification;
                   ADpi: Double;
                   AWidth, AHeight: Integer): Boolean;

  public
    procedure AfterConstruction(); override;

    function CanBatch(): Boolean; override;
    function IsValid(): Boolean; override;
    function IsValidFor(const ACoord: TGeoPoint): Boolean; override;

    function PixelToGeo(X, Y: Double; out ACoord: TGeoPoint): Boolean; override;
    function GeoToPixel(const ACoord: TGeoPoint; out X, Y: Double): Boolean; override;

    function Setup(const ATile: TOsmTileId;
                   const AMagnification: TMagnification;
                   ADpi: Double;
                   AWidth, AHeight: Integer): Boolean; overload;

    function Setup(const ATileBox: TOsmTileIdBox;
                   const AMagnification: TMagnification;
                   ADpi: Double;
                   AWidth, AHeight: Integer): Boolean; overload;

    { Switch to enable/disable linear interpolation of latitude to pixel computation.
      It speedup GeoToPixel calculation with fractional error on small render area. }
    property IsLinearInterpolationEnabled: Boolean read FIsLinearInterpolationEnabled write FIsLinearInterpolationEnabled;
  end;



implementation

uses Math;

const
  GRAD_TO_RAD = 2 * M_PI / 360;


{ TBatchTransformer }

procedure TBatchTransformer.GeoToPixel(ALat: TLatitude; ALon: TLongtitude;
  var x, y: Double);
begin
  Assert(Assigned(Projection));
{$ifdef OSMSCOUT_HAVE_SSE2}
  if Projection.CanBatch() then
  begin
    Self.lon[count] := ALon;
    Self.lat[count] := ALat;
    xPointer[count] := @x;
    yPointer[count] := @y;
    count++;

    if (count = 2) then
    begin
      count=0;
      Projection.GeoToPixel(Self);
    end;
  end
  else
  begin
    Projection.GeoToPixel(GeoCoord(ALat, ALon), x, y);
  end;
{$else}
  Projection.GeoToPixel(GeoCoord(ALat, ALon), x, y);
{$endif}
end;


{ TProjection }

procedure TProjection.AfterConstruction();
begin
  inherited AfterConstruction();

  FLon := 0.0;
  FLat := 0.0;
  FAngle := 0.0;
  FMagnification.Level := 0;
  FDpi := 0.0;
  FWidth := 0;
  FHeight := 0;
  LonMin := 0.0;
  LonMax := 0.0;
  LatMin := 0.0;
  LatMax := 0.0;
  FPixelSize := 0.0;
  FMeterInPixel := 0.0;
  FMeterInMM := 0.0;
end;

function TProjection.GetCenter(): TGeoPoint;
begin
  Result.Init(Lat, Lon);
end;

function TProjection.GeoCoordIsIn(ALat: TLatitude; ALon: TLongtitude): Boolean;
begin
  Result := (ALon >= LonMin) and (ALon < LonMax) and (ALat >= LatMin) and (ALat <= LatMax);
end;

function TProjection.GeoBoxIsIn(const AGeoBox: TGeoBox): Boolean;
begin
  Result := not ((AGeoBox.MinCoord.Lon > LonMax)
              or (AGeoBox.MaxCoord.Lon < LonMin)
              or (AGeoBox.MinCoord.Lat > LatMax)
              or (AGeoBox.MaxCoord.Lat < LatMin));
end;

function TProjection.IsPixelValid(AX, AY, ATolerance: Double): Boolean;
begin
  Result := (AX >= -ATolerance) and (AX <= (Width + ATolerance))
        and (AY >= -ATolerance) and (AY <= (Height + ATolerance));
end;

function TProjection.GetDimensions(): TGeoBox;
begin
  Result.MinCoord.Init(LatMin, LonMin);
  Result.MaxCoord.Init(LatMax, LonMax);
end;

procedure TProjection.FillDimensions(var AValue: TGeoBox);
begin
  AValue.MinCoord.Init(LatMin, LonMin);
  AValue.MaxCoord.Init(LatMax, LonMax);
end;

function TProjection.ConvertWidthToPixel(AValue: Double): Double;
begin
  Result := AValue * Dpi / 25.4;
end;

function TProjection.ConvertPixelToWidth(AValue: Double): Double;
begin
  Result := AValue * 25.4 / Dpi;
end;

{ TMercatorProjection }

procedure TMercatorProjection.GeoToPixelBath(ATransformData: TBatchTransformer);
begin
  Assert(False); //should not be called
end;

procedure TMercatorProjection.AfterConstruction();
begin
  inherited AfterConstruction();

  FMaxLat := +85.0511;
  FMinLat := -85.0511;
  FMaxLon := +180.0;
  FMinLon := -180.0;

  FIsValid := False;
  FLatOffset := 0.0;
  FScale := 1.0;
  FScaleGradtorad := 0.0;
  FIsLinearInterpolationEnabled := False;
end;

function TMercatorProjection.CanBatch(): Boolean;
begin
  Result := False;
end;

function TMercatorProjection.IsValid(): Boolean;
begin
  Result := FIsValid;
end;

function TMercatorProjection.IsValidFor(const ACoord: TGeoPoint): Boolean;
begin
  Result := (ACoord.Lat >= MinLat) and (ACoord.Lat <= MaxLat)
        and (ACoord.Lon >= MinLon) and (ACoord.Lon <= MaxLon);
end;

function TMercatorProjection.PixelToGeo(X, Y: Double; out ACoord: TGeoPoint): Boolean;
var
  xn, yn: Double;
begin
  Assert(FIsValid);

  // Transform to center-based coordinate
  X := X - Width / 2.0;
  Y := Height / 2.0 - Y;

  if (Angle <> 0.0) then
  begin
    xn := X * FAngleCos - Y * FAngleSin;
    yn := X * FAngleSin + Y * FAngleCos;

    X := xn;
    Y := yn;
  end;

  // Transform to absolute geo coordinate
  ACoord.Lon := Lon + X / FScaleGradtorad;
  ACoord.Lat := arctan(sinh(Y / FScale + FLatOffset)) / GRAD_TO_RAD;

  Result := IsValidFor(ACoord);
end;

function TMercatorProjection.GeoToPixel(const ACoord: TGeoPoint; out X,
  Y: Double): Boolean;
var
  xn, yn: Double;
begin
  Assert(FIsValid);

  // Screen coordinate relative to center of image
  X := (ACoord.Lon - Lon) * FScaleGradtorad;

  if (IsLinearInterpolationEnabled) then
    Y := (ACoord.Lat - Lat) * FScaledLatDeriv
  else
    Y := (arctanh(sin(ACoord.Lat * GRAD_TO_RAD)) - FLatOffset) * FScale;

  if (Angle <> 0.0) then
  begin
    xn := X * FAngleNegCos - Y * FAngleNegSin;
    yn := X * FAngleNegSin + Y * FAngleNegCos;

    X := xn;
    Y := yn;
  end;

  // Transform to canvas coordinate
  Y := Height / 2.0 - Y;
  X := X + Width / 2.0;

  Result := IsValidFor(ACoord);
end;

function TMercatorProjection.Setup(const ACoord: TGeoPoint; AAngle: Double;
  const AMagnification: TMagnification; ADpi: Double; AWidth, AHeight: Integer): Boolean;
var
  fpEquatorTileWidth, fpEquatorTileResolution: Double;
  fpEquatorCorrectedEquatorTileResolution: Double;
  fpGroundWidthEquatorMeter, fpGroundWidthVisibleMeter: Double;
  tl: TGeoPoint; // top left
  tr: TGeoPoint; // top right
  bl: TGeoPoint; // bottom left
  br: TGeoPoint; // bottom right
  latDeriv: Double;
begin
  Result := FIsValid
        and (Lon = ACoord.Lon)
        and (Lat = ACoord.Lat)
        and (FAngle = AAngle)
        and FMagnification.IsEqual(AMagnification)
        and (FDpi = ADpi)
        and (FWidth = AWidth)
        and (FHeight = AHeight);

  if Result then
    Exit;

  if (not IsValidFor(ACoord)) then
    Exit;

  FIsValid := True;

  // Make a copy of the context information
  FLon := ACoord.Lon;
  FLat := ACoord.Lat;
  FAngle := AAngle;
  FMagnification.Assign(AMagnification);
  FDpi := ADpi;
  FWidth := AWidth;
  FHeight := AHeight;

  if (Angle <> 0.0) then
  begin
    FAngleSin := sin(Angle);
    FAngleCos := cos(Angle);
    FAngleNegSin := -FAngleSin;
    FAngleNegCos := FAngleCos;   // ???
  end
  else
  begin
    FAngleSin := 0;
    FAngleNegSin := 0;
    FAngleCos := 1;
    FAngleNegCos := 1;
  end;

  // Width in meter of a tile of the given magnification at the equator
  fpEquatorTileWidth := TileWidthZoom0Aquator / FMagnification.Magnification;

  // Resolution (meter/pixel) of a pixel in a classical 256 pixel tile for the given zoom level at the equator
  fpEquatorTileResolution := fpEquatorTileWidth / 256.0;

  // Modified resolution (meter/pixel) at the equator based on our actual DPI instead of the standard tile DPI
  fpEquatorCorrectedEquatorTileResolution := fpEquatorTileResolution * TileDPI / Dpi;

  // Width of the visible area at the equator
  fpGroundWidthEquatorMeter := Width * fpEquatorCorrectedEquatorTileResolution;

  // Width of the visible area in meter
  fpGroundWidthVisibleMeter := fpGroundWidthEquatorMeter * cos(Lat * GRAD_TO_RAD);

  // Resulting projection scale factor
  FScale := Width / (2 * M_PI * fpGroundWidthEquatorMeter / EarthExtentMeter);
  FScaleGradtorad := FScale * GRAD_TO_RAD;

  // Size of one pixel in meter
  FPixelSize := fpGroundWidthVisibleMeter / Width;

  // How many pixel are one meter?
  FMeterInPixel := 1.0 / FPixelSize;

  // 1 meter on the ground is how many millimeter on display?
  FMeterInMM := FMeterInPixel * 25.4 / Dpi;

  // Absolute Y mercator coordinate for latitude
  FLatOffset := arctanh(sin(ACoord.Lat * GRAD_TO_RAD));

  //std::cout << "Pixel size " << pixelSize << " meterInPixel " << meterInPixel << " meterInMM " << meterInMM << std::endl;

  // top left
  PixelToGeo(0.0, 0.0, tl);

  // top right
  PixelToGeo(Width, 0.0, tr);

  // bottom left
  PixelToGeo(0.0, Height, bl);

  // bottom right
  PixelToGeo(Width,Height, br);

  // evaluate bounding box, crop bounding box to valid Mercator area
  LatMin := max(MinLat, min(min(tl.Lat, tr.Lat), min(bl.Lat, br.Lat)));
  LatMax := min(MaxLat, max(max(tl.Lat, tr.Lat), max(bl.Lat, br.Lat)));

  LonMin := max(MinLon, min(min(tl.Lon, tr.Lon), min(bl.Lon, br.Lon)));
  LonMax := min(MaxLon, max(max(tl.Lon, tr.Lon), max(bl.Lon, br.Lon)));

  // derivation of "latToYPixel" function in projection center
  LatDeriv := 1.0 / sin((2 * Lat * GRAD_TO_RAD + M_PI) / 2);
  FScaledLatDeriv := LatDeriv * GRAD_TO_RAD * FScale;

  {$ifdef DEBUG_MERCATOR}
  LogDebug('Center: ' + GeoCoord(Lat, Lon).GetDisplayText());
  LogDebug(Format('Magnification: %d/%d', [Magnification.Magnification, Magnification.Level]));
  LogDebug(Format('Screen dimension: %d x $d, %d DPI', [Width, Height, Dpi]));

  LogDebug(Format('Box: %s, $d', [GeoBox(GeoCoord(latMin,lonMin), GeoCoord(latMax,lonMax)).GetDisplayText(), fpGroundWidthVisibleMeter]));

  LogDebug(Format('Scale: 1 : %d', [FScale]));
  {$endif}

  Result := True;
end;

function TMercatorProjection.Move(AHorizPixel, AVertPixel: Double): Boolean;
var
  x, y: Double;
  TmpCoord: TGeoPoint;
begin
  Result := False;
  GeoToPixel(GeoCoord(Lat, Lon), x, y);

  if (not PixelToGeo(x + AHorizPixel, y - AVertPixel, TmpCoord)) then
    Exit;

  if (TmpCoord.Lat < -85.0511) or (TmpCoord.Lat > 85.0511) then
    Exit;

  if (TmpCoord.Lon < -180.0) or (TmpCoord.Lon > 180.0) then
    Exit;

  Result := Setup(TmpCoord, Angle, Magnification, Dpi, Width, Height);
end;

function TMercatorProjection.MoveUp(APixel: Double): Boolean;
begin
  Result := Move(0, APixel);
end;

function TMercatorProjection.MoveDown(APixel: Double): Boolean;
begin
  Result := Move(0, -APixel);
end;

function TMercatorProjection.MoveLeft(APixel: Double): Boolean;
begin
  Result := Move(-APixel, 0);
end;

function TMercatorProjection.MoveRight(APixel: Double): Boolean;
begin
  Result := Move(APixel, 0);
end;

{ TTileProjection }

function TTileProjection.SetupInternal(ALonMin, ALatMin, ALonMax,
  ALatMax: Double; const AMagnification: TMagnification; ADpi: Double; AWidth,
  AHeight: Integer): Boolean;
var
  latDeriv: Double;
begin
  Result := (LatMax = ALatMax)
        and (LonMin = ALonMin)
        and (LonMax = ALonMax)
        and FMagnification.IsEqual(AMagnification)
        and (Dpi = ADpi)
        and (Width = AWidth)
        and (Height = AHeight);
  if Result then
    Exit;

  FIsValid := True;

  // Make a copy of the context information
  FMagnification.Assign(AMagnification);
  FDpi := ADpi;
  FWidth := AWidth;
  FHeight := AHeight;

  LatMin := ALatMin;
  LatMax := ALatMax;
  LonMin := ALonMin;
  LonMax := ALonMax;

  FLat := (LatMin + LatMax) / 2;
  FLon := (LonMin + LonMax) / 2;

  FScale := Width / (GRAD_TO_RAD * (LonMax - LonMin));
  FScaleGradtorad := FScale * GRAD_TO_RAD;

  FLonOffset := LonMin * FScaleGradtorad;
  FLatOffset := FScale * arctanh(sin(LatMin * GRAD_TO_RAD));

  FPixelSize := EarthExtentMeter / Magnification.Magnification / Width;
  FMeterInPixel := 1 / FPixelSize;
  FMeterInMM := FMeterInPixel * 25.4 / FPixelSize;

  // derivation of "latToYPixel" function in projection center
  latDeriv := 1.0 / sin((2 * Lat * GRAD_TO_RAD + M_PI) / 2);
  FScaledLatDeriv := latDeriv * GRAD_TO_RAD * FScale;

  {$ifdef OSMSCOUT_HAVE_SSE2}
  sse2LonOffset      = _mm_set1_pd(FLonOffset);
  sse2LatOffset      = _mm_set1_pd(FLatOffset);
  sse2Scale          = _mm_set1_pd(FScale);
  sse2ScaleGradtorad = _mm_set1_pd(FScaleGradtorad);
  sse2Height         = _mm_set1_pd(Height);
  {$endif}

  Result := True;
end;

procedure TTileProjection.AfterConstruction();
begin
  inherited AfterConstruction();

  FIsValid := False;
  FLonOffset := 0.0;
  FLatOffset := 0.0;
  FScale := 0.0;
  FScaleGradtorad := 0.0;
  FIsLinearInterpolationEnabled := False;
end;

function TTileProjection.CanBatch(): Boolean;
begin
  //Result := True;
  Result := False;  // not supported
end;

function TTileProjection.IsValid(): Boolean;
begin
  Result := FIsValid;
end;

function TTileProjection.IsValidFor(const ACoord: TGeoPoint): Boolean;
begin
  Result := (ACoord.Lat >= -85.0511) and (ACoord.Lat <= +85.0511)
        and (ACoord.Lon >= -180.0) and (ACoord.Lon <= +180.0);
end;

function TTileProjection.PixelToGeo(X, Y: Double; out ACoord: TGeoPoint): Boolean;
begin
  ACoord.Lon := (X + FLonOffset) / (FScale * GRAD_TO_RAD);
  ACoord.Lat := arctan(sinh((Height - Y + FLatOffset) / FScale)) / GRAD_TO_RAD;

  Result := IsValidFor(ACoord);
end;

function TTileProjection.GeoToPixel(const ACoord: TGeoPoint; out X, Y: Double): Boolean;
begin
  X := ACoord.Lon * FScaleGradtorad - FLonOffset;

  if IsLinearInterpolationEnabled then
    Y := (Height / 2.0) - ((ACoord.Lat - Lat) * FScaledLatDeriv)
  else
  begin
    {$ifdef OSMSCOUT_HAVE_SSE2}
    Y := Height - (FScale * atanh_sin_pd(ACoord.Lat * GRAD_TO_RAD) - FLatOffset);
    {$else}
    Y := Height - (FScale * arctanh(sin(ACoord.Lat * GRAD_TO_RAD)) - FLatOffset);
    {$endif}
  end;

  Result := IsValidFor(ACoord);
end;

procedure TTileProjection.GeoToPixelBath(ATransformData: TBatchTransformer);
begin
  {$ifdef OSMSCOUT_HAVE_SSE2}
  v2df x := _mm_sub_pd(_mm_mul_pd( ARRAY2V2DF(transformData.lon), sse2ScaleGradtorad), sse2LonOffset);
  __m128d test = ARRAY2V2DF(transformData.lat);
  v2df y := _mm_sub_pd(sse2Height, _mm_sub_pd(_mm_mul_pd(sse2Scale, atanh_sin_pd( _mm_mul_pd( test,  ARRAY2V2DF(sseGradtorad)))), sse2LatOffset));

  //store results:
  _mm_storel_pd (transformData.xPointer[0], x);
  _mm_storeh_pd (transformData.xPointer[1], x);
  _mm_storel_pd (transformData.yPointer[0], y);
  _mm_storeh_pd (transformData.yPointer[1], y);
  {$else}
  Assert(False); //should not be called
  {$endif}
end;

function TTileProjection.Setup(const ATile: TOsmTileId;
  const AMagnification: TMagnification; ADpi: Double; AWidth, AHeight: Integer): Boolean;
var
  BoundingBox: TGeoBox;
begin
  BoundingBox := ATile.GetBoundingBox(AMagnification);
  Result := SetupInternal(BoundingBox.MinCoord.Lon,
                          BoundingBox.MinCoord.Lat,
                          BoundingBox.MaxCoord.Lon,
                          BoundingBox.MaxCoord.Lat,
                          AMagnification,
                          ADpi,
                          AWidth, AHeight);
end;

function TTileProjection.Setup(const ATileBox: TOsmTileIdBox;
  const AMagnification: TMagnification; ADpi: Double; AWidth, AHeight: Integer): Boolean;
var
  BoundingBox: TGeoBox;
begin
  BoundingBox := ATileBox.GetBoundingBox(AMagnification);
  Result := SetupInternal(BoundingBox.MinCoord.Lon,
                          BoundingBox.MinCoord.Lat,
                          BoundingBox.MaxCoord.Lon,
                          BoundingBox.MaxCoord.Lat,
                          AMagnification,
                          ADpi,
                          AWidth, AHeight);
end;

end.

