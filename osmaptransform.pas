(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2019  Sergey Bodrov

  This source is ported from libosmscout library
  Copyright (C) 2011  Tim Teulings

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
util\Transformation
  OptimizeMethod   -> TTransOptimizeMethod
  OutputConstraint -> TTransOutputConstraint
  TransPoint       -> TTransPoint
  TransPolygon
  CoordBuffer      -> TTransBuffer
  TransBuffer      -> TTransBuffer
*)
unit OsMapTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsMapGeometry, OsMapTypes, OsMapProjection;

type
  TTransOptimizeMethod = (tomNone, tomFast, tomQuality);
  TTransOutputConstraint = (tocNoConstraint, tocSimple);

  { TTransPoint }

  TTransPoint = object
  public
    IsVisible: Boolean; // point visible on screen area
    IsDraw: Boolean;    // point visible or connected to visible
    XLon: Double;
    YLat: Double;
    Index: Integer;     // original index, for copy of point

    function IsEqual(AOther: TTransPoint): Boolean; inline;
    function AsCoord(): TGeoPoint; inline;
    function AsVertex2D(): TVertex2D; inline;
  end;
  TTransPointArray = array of TTransPoint;

  { TTransPointList }

  TTransPointList = object
  private
    FCount: Integer;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    Items: array of TTransPoint;

    procedure Clear();
    procedure Delete(AStart, ACount: Integer);

    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { TPolyCell }

  TPolyCell = object
  public
    CellCenter: TVertex2D; // cell center
    HalfSize: Double;      // half the cell size
    Dist: Double;          // distance from cell center to polygon
    MaxDist: Double;       // max distance to polygon within a cell

    procedure Init(AX, AY: Double; AHalfSize: Double; const APoly: TVertex2DArray);
    // set as polygon centroid
    procedure InitCentroid(const APoly: TVertex2DArray);
  end;

  { TPolyCellQueue }

  TPolyCellQueue = object
  private
    FItems: array of TPolyCell;
    FCount: Integer;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FMaxDistMax: Double;
  public
    procedure Init();
    procedure Push(const AItem: TPolyCell);
    function Pull(out AItem: TPolyCell): Boolean;

    property Count: Integer read FCount;
  end;

  { TTransPolygon }

  TTransPolygon = object
  private
    //FPointsSize: Integer;
    FLength: Integer;
    FStart: Integer;
    FEnd: Integer;

    procedure ErasePointsFromArray(var APoints: TTransPointArray; AFrom, ATo: Integer);
    function AddPointToArray(var APoints: TTransPointArray; const AItem: TTransPoint): Integer;

    procedure TransformGeoToPixel(const AProjection: TProjection;
                             const ANodes: TGeoPointArray);
    procedure DropSimilarPoints(AOptimizeErrorTolerance: Double);
    { Drop every point that is (more or less) on direct line between two points A and B }
    procedure DropRedundantPointsFast(AOptimizeErrorTolerance: Double);
    procedure DropRedundantPointsDouglasPeucker(AOptimizeErrorTolerance: Double; AIsArea: Boolean);
    procedure DropEqualPoints();

    { Returns true, if the lines defined by the given coordinates intersect. }
    function IsLinesIntersect(const A1, A2, B1, B2: TTransPoint): Boolean;

    { Helper for FindPathIntersections
      Compute bounding box of path segment from node index
      AFfrom (inclusive) to node index ATo (exclusive) }
    procedure GetSegmentBoundingBox(const APoints: TTransPointArray; AFrom, ATo: Integer; out ABoundingBox: TGeoBox);
    { Helper for FindPathIntersections
      Compute bounding boxes for path segments with length 1000 nodes,
      up to node index bound (exclusive) }
    procedure ComputeSegmentBoxes(const APoints: TTransPointArray; var ASegmentBoxes: TSegmentGeoBoxArray; ABound: Integer; ASegmentSize: Integer = 1000);

    { Find next intersection on way (with itself) from node index AStart1.
      Return true if some intersection was found (way is not simple),
      AStart1i and AStart2 indexes are setup to start possition of intesections lines. }
    function FindIntersection(const APoints: TTransPointArray; var AStart1, AStart2: Integer): Boolean;

    { set IsDraw to false for redundant nodes }
    procedure EnsureSimple(AIsArea: Boolean);

    { Extend Points array length if needed, return False if ALength < AMinLength }
    function CheckCapacity(ALength, AMinLength: Integer): Boolean;
    { Calculate start and end index for points array of given ALength }
    procedure CalculateStartEnd(ALength: Integer);

  public
    { transformed points, in pixel XY }
    Points: TTransPointArray;
    BatchTransformer: TBatchTransformer;

    function IsEmpty(): Boolean;

    procedure TransformArea(const AProjection: TProjection;
                       AOptimize: TTransOptimizeMethod;
                       const ANodes: TGeoPointArray;
                       AOptimizeErrorTolerance: Double;
                       AConstraint: TTransOutputConstraint = tocNoConstraint);

    procedure TransformWay(const AProjection: TProjection;
                       AOptimize: TTransOptimizeMethod;
                       const ANodes: TGeoPointArray;
                       AOptimizeErrorTolerance: Double;
                       AConstraint: TTransOutputConstraint = tocNoConstraint);

    procedure TransformBoundingBox(const AProjection: TProjection;
                       AOptimize: TTransOptimizeMethod;
                       const ABoundingBox: TGeoBox;
                       AOptimizeErrorTolerance: Double;
                       AConstraint: TTransOutputConstraint = tocNoConstraint);

    function GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double): Boolean;

    property PointsCount: Integer read FLength;
    property StartIndex: Integer read FStart;
    property EndIndex: Integer read FEnd;
  end;

  { TTransBuffer }
  { List of all screen points for every visible area and way vertex }
  { TODO : replace by TMapTransform }
  TTransBuffer = object
  private
    FCount: Integer; // used points
    FPolyCellQueue: TPolyCellQueue;
  public
    TransPolygon: TTransPolygon;
    Buffer: array of TVertex2D;

    procedure Init();
    procedure Reset();
    procedure Assign(AOther: TTransBuffer);
    function PushCoord(X, Y: Double): Integer;

    procedure TransformArea(const AProjection: TProjection;
                       AOptimize: TTransOptimizeMethod;
                       const ANodes: TGeoPointArray;
                       out AStart, AEnd: Integer;
                       AOptimizeErrorTolerance: Double);

    function TransformWay(const AProjection: TProjection;
                       AOptimize: TTransOptimizeMethod;
                       const ANodes: TGeoPointArray;
                       out AStart, AEnd: Integer;
                       AOptimizeErrorTolerance: Double): Boolean;

    { Generate parallel way to way stored in this buffer on range orgStart, orgEnd (inclusive)
      Result is stored after the last valid point. Generated way offsets are returned
      in start and end. }
    function GenerateParallelWay(AOrgStart, AOrgEnd: Integer;
                       AOffset: Double;
                       out AStart, AEnd: Integer): Boolean;

    { A fast algorithm for finding polygon pole of inaccessibility, the most
      distant internal point from the polygon outline (not to be confused with
      centroid). Useful for optimal placement of a text label on a polygon.
      https://github.com/mapbox/polylabel }
    function Polylabel(AStart, AEnd: Integer; APrecision: Double = 1.0): TVertex2D;

    procedure FillVertex2DArray(out APolyArr: TVertex2DArray; AStart, AEnd: Integer);

    property Count: Integer read FCount;
  end;

type

  { TTransLineSegment }

  TTransLineSegment = object
    Ref: TTransPoint;
    XDelta: Double;
    YDelta: Double;
    InverseLength: Double;

    procedure Init(const A, B: TTransPoint);

    function IsValid(): Boolean;

    function CalculateDistanceSquared(const p: TTransPoint): Double;
  end;

implementation

uses Math;

const
  SQRT2 = sqrt(2);

function fabs(a: Double): Double; inline;
begin
  Result := abs(a);
end;

{ Calculates the distance between a point p and a line defined by the points a and b.
  p - The point in distance to a line
  a - One point defining the line
  b - Another point defining the line }
function CalculateDistancePointToLineSegment(const p, a, b: TTransPoint): Double;
var
  xdelta, ydelta, u, cx, cy, dx, dy: Double;
begin
  xdelta := b.XLon - a.XLon;
  ydelta := b.YLat - a.YLat;

  if (xdelta = 0) and (ydelta = 0) then
  begin
    Result := Infinity;
    Exit;
  end;

  u := ((p.XLon - a.XLon) * xdelta + (p.YLat - a.YLat) * ydelta) / (xdelta * xdelta + ydelta * ydelta);

  if (u < 0) then
  begin
    cx := a.XLon;
    cy := a.YLat;
  end
  else if (u > 1) then
  begin
    cx := b.XLon;
    cy := b.YLat;
  end
  else
  begin
    cx := a.XLon + (u * xdelta);
    cy := a.YLat + (u * ydelta);
  end;

  dx := cx - p.XLon;
  dy := cy - p.YLat;

  Result := sqrt((dx * dx) + (dy * dy));
end;

function CalculateDistancePointToPoint(const a, b: TTransPoint): Double;
var
  dx, dy: Double;
begin
  dx := b.XLon - a.XLon;
  dy := b.YLat - a.YLat;

  Result := sqrt(dx*dx + dy*dy);
end;

// get squared distance from a point (P) to a segment (A, B)
function CalculateSquaredDistancePointToLineSegment(const P, A, B: TVertex2D): Double;
var
  X, Y, DX, DY, T: Double;
begin
  X := A.X;
  Y := A.Y;
  DX := B.X - X;
  DY := B.Y - Y;

  if (DX <> 0) or (DY <> 0) then
  begin
    T := ((P.X - X) * DX + (P.Y - Y) * DY) / (DX * DX + DY * DY);

    if (T > 1) then
    begin
      X := B.X;
      Y := B.Y;
    end
    else if (T > 0) then
    begin
      X := X + (DX * T);
      Y := Y + (DY * T);
    end;
  end;

  DX := P.X - X;
  DY := P.Y - Y;

  Result := (DX * DX) + (DY * DY);
end;

procedure SimplifyPolyLineDouglasPeucker(var APoints: TTransPointArray;
  ABeginIndex, AEndIndex, AEndValueIndex: Integer;
  AOptimizeErrorToleranceSquared: Double);
var
  LineSegment: TTransLineSegment;
  distanceSquared, maxDistanceSquared: Double;
  i, maxDistanceIndex: Integer;
begin
  LineSegment.Init(APoints[ABeginIndex], APoints[AEndValueIndex]);

  maxDistanceSquared := 0;
  maxDistanceIndex := ABeginIndex;

  for i := ABeginIndex+1 to AEndIndex do
  begin
    if APoints[i].IsDraw then
    begin
      distanceSquared := LineSegment.CalculateDistanceSquared(APoints[i]);

      if (distanceSquared > maxDistanceSquared) then
      begin
        maxDistanceSquared := distanceSquared;
        maxDistanceIndex := i;
      end;
    end;
  end;

  if (maxDistanceSquared <= AOptimizeErrorToleranceSquared) then
  begin
    //we don't need to draw any extra points
    for i := ABeginIndex+1 to AEndIndex do
      APoints[i].IsDraw := False;

    Exit;
  end;

  //we need to split this line in two pieces
  SimplifyPolyLineDouglasPeucker(APoints,
                                 ABeginIndex,
                                 maxDistanceIndex,
                                 maxDistanceIndex,
                                 AOptimizeErrorToleranceSquared);

  SimplifyPolyLineDouglasPeucker(APoints,
                                 maxDistanceIndex,
                                 AEndIndex,
                                 AEndValueIndex,
                                 AOptimizeErrorToleranceSquared);
end;

{ TTransPoint }

function TTransPoint.IsEqual(AOther: TTransPoint): Boolean;
begin
  Result := (IsDraw = AOther.IsDraw) and  (XLon = AOther.XLon) and (YLat = AOther.YLat);
end;

function TTransPoint.AsCoord(): TGeoPoint;
begin
  Result.Lat := YLat;
  Result.Lon := XLon;
end;

function TTransPoint.AsVertex2D(): TVertex2D;
begin
  Result.X := XLon;
  Result.Y := YLat;
end;

{ TTransPointList }

function TTransPointList.GetCapacity: Integer;
begin
  Result := Length(Items);
end;

function TTransPointList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TTransPointList.SetCapacity(AValue: Integer);
begin
  SetLength(Items, AValue);
  if FCount > AValue then
    FCount := AValue;
end;

procedure TTransPointList.Clear();
begin
  FCount := 0;
end;

procedure TTransPointList.Delete(AStart, ACount: Integer);
var
  i, ii, n: Integer;
begin
  if ACount > (FCount - AStart) then
    ACount := (FCount - AStart);
  if ACount <= 0 then
    Exit;
  i := AStart;
  ii := i + ACount;
  n := 0;
  while (n < ACount) do
  begin
    Items[i] := Items[ii];
    Inc(i);
    Inc(ii);
    Inc(n);
  end;
  FCount := FCount - ACount;
end;

{ TTransPolygon }

procedure TTransPolygon.ErasePointsFromArray(var APoints: TTransPointArray;
  AFrom, ATo: Integer);
var
  i, ii, n, iCount: Integer;
begin
  iCount := Length(APoints);
  if ATo >= iCount then
    ATo := iCount-1;
  if (ATo < 0) or (ATo < AFrom) then
    Exit;
  n := ATo - AFrom + 1; // move count
  i := AFrom;
  ii := AFrom + n;
  while n > 0 do
  begin
    APoints[i] := APoints[ii];
    Inc(i);
    Inc(ii);
    Dec(n);
  end;
  SetLength(APoints, iCount - (ATo - AFrom + 1));
end;

function TTransPolygon.AddPointToArray(var APoints: TTransPointArray;
  const AItem: TTransPoint): Integer;
begin
  Result := Length(APoints);
  SetLength(APoints, Result + 1);
  APoints[Result] := AItem;
end;

procedure TTransPolygon.TransformGeoToPixel(const AProjection: TProjection;
  const ANodes: TGeoPointArray);
var
  i: Integer;
begin
  {$ifdef OSMSCOUT_HAVE_SSE2}
  BatchTransformer.Projection := AProjection;
  {$endif}
  if Length(ANodes) > 0 then
  begin
    FStart := 0;
    FLength := Length(ANodes);
    FEnd := FLength-1;
    SetLength(Points, FLength);

    for i := FStart to FEnd do
    begin
      {$ifdef OSMSCOUT_HAVE_SSE2}
      BatchTransformer.GeoToPixel(ANodes[i].Lon, ANodes[i].Lat,
                                  Points[i].XLon, Points[i].YLat);
      {$else}
      AProjection.GeoToPixel(ANodes[i], Points[i].XLon, Points[i].YLat);
      {$endif}
      Points[i].IsDraw := True;
    end;
  end
  else
  begin
    FStart := 0;
    FEnd := 0;
    FLength := 0;
    SetLength(Points, FLength);
  end;
end;

procedure TTransPolygon.DropSimilarPoints(AOptimizeErrorTolerance: Double);
var
  i, j: Integer;
begin
  for i := 0 to FLength - 1 do
  begin
    if Points[i].IsDraw then
    begin
      j := i+1;
      while (j < FLength) do
      begin
        if Points[j].IsDraw then
        begin
          if ((fabs(Points[j].XLon - Points[i].XLon) <= AOptimizeErrorTolerance) and
              (fabs(Points[j].YLat - Points[i].YLat) <= AOptimizeErrorTolerance))
          then
            Points[j].IsDraw := False
          else
            Break;
        end;
        Inc(j);
      end;
    end;
  end;
end;

procedure TTransPolygon.DropRedundantPointsFast(AOptimizeErrorTolerance: Double);
var
  prev, cur, next: Integer;
  distance: Double;
begin
  // Drop every point that is (more or less) on direct line between two points A and B
  prev := 0;
  while (prev < FLength) do
  begin

    while (prev < FLength) and (not Points[prev].IsDraw) do
      Inc(prev);

    if (prev >= FLength) then
      Break;

    cur := prev + 1;

    while (cur < FLength) and (not Points[cur].IsDraw) do
      Inc(cur);

    if (cur >= FLength) then
      Break;

    next := cur + 1;

    while (next < FLength) and (not Points[next].IsDraw) do
      Inc(next);

    if (next >= FLength) then
      Break;

    distance := CalculateDistancePointToLineSegment(Points[cur],
                                                    Points[prev],
                                                    Points[next]);

    if (distance <= AOptimizeErrorTolerance) then
    begin
      Points[cur].IsDraw := False;
      prev := next;
    end
    else
      Inc(prev);
  end;
end;

procedure TTransPolygon.DropRedundantPointsDouglasPeucker(
  AOptimizeErrorTolerance: Double; AIsArea: Boolean);
var
  optimizeErrorToleranceSquared: Double;
  i, iBeg, iEnd, maxDistIndex: Integer;
  maxDist, dist: Double;
begin
  // An implementation of Douglas-Peuker algorithm http://softsurfer.com/Archive/algorithm_0205/algorithm_0205.htm

  optimizeErrorToleranceSquared := AOptimizeErrorTolerance * AOptimizeErrorTolerance;
  iBeg := 0;

  while (iBeg < FLength) and (not Points[iBeg].IsDraw) do
    Inc(iBeg);

  if (iBeg >= FLength) then
    Exit; //we found no single point that is drawn.

  //if this polyon is an area, we start by finding the largest distance from begin point to all other points
  if (AIsArea) then
  begin

    maxDist := 0.0;
    maxDistIndex := iBeg;

    for i := iBeg to FLength-1 do
    begin
      if Points[i].IsDraw then
      begin
        dist := CalculateDistancePointToPoint(Points[iBeg], Points[i]);

        if (dist > maxDist) then
        begin
          maxDist := dist;
          maxDistIndex := i;
        end;
      end;
    end;

    if (maxDistIndex = iBeg) then
      Exit; //we only found 1 point to draw

    SimplifyPolyLineDouglasPeucker(Points,
                                   iBeg,
                                   maxDistIndex,
                                   maxDistIndex,
                                   optimizeErrorToleranceSquared);
    SimplifyPolyLineDouglasPeucker(Points,
                                   maxDistIndex,
                                   FLength,
                                   iBeg,
                                   optimizeErrorToleranceSquared);
  end
  else
  begin
    //not an area but polyline
    //find last drawable point;
    iEnd := FLength-1;
    while (iEnd > iBeg) and (not Points[iEnd].IsDraw) do
      Dec(iEnd);

    if (iEnd <= iBeg) then
      Exit; //we only found 1 drawable point;

    SimplifyPolyLineDouglasPeucker(Points,
                                   iBeg,
                                   iEnd,
                                   iEnd,
                                   optimizeErrorToleranceSquared);
  end;
end;

procedure TTransPolygon.DropEqualPoints();
var
  iCurrent, iNext: Integer;
begin
  iCurrent := 0;
  while (iCurrent < FLength) do
  begin
    if (not Points[iCurrent].IsDraw) then
    begin
      Inc(iCurrent);
      Continue;
    end;

    iNext := iCurrent + 1;
    while (iNext < FLength) and (not Points[iNext].IsDraw) do
      Inc(iNext);

    if (iNext >= FLength) then
      Exit;

    if  (Points[iCurrent].XLon = Points[iNext].XLon)
    and (Points[iCurrent].YLat = Points[iNext].YLat)
    then
      Points[iNext].IsDraw := False;

    iCurrent := iNext;
  end;
end;

function TTransPolygon.IsLinesIntersect(const A1, A2, B1, B2: TTransPoint): Boolean;
var
  denr, ua_numr, ub_numr, ua, ub: Double;
  aBox, bBox: TGeoBox;
begin
  if A1.IsEqual(B1)
  or A1.IsEqual(B2)
  or A2.IsEqual(B1)
  or A2.IsEqual(B2)
  then
    Exit(True);

  if (A1.IsEqual(A2) and B1.IsEqual(B2)) then
  begin
    // two different zero size vectors can't intersects
    Exit(False);
  end;

  // denominator, see GetLineIntersection for more details
  denr := (B2.YLat - B1.YLat) * (A2.XLon - A1.XLon)
        - (B2.XLon - B1.XLon) * (A2.YLat - A1.YLat);

  ua_numr := (B2.XLon - B1.XLon) * (A1.YLat - B1.YLat)
           - (B2.YLat - B1.YLat) * (A1.XLon - B1.XLon);

  ub_numr := (A2.XLon - A1.XLon) * (A1.YLat - B1.YLat)
           - (A2.YLat - A1.YLat) * (A1.XLon - B1.XLon);

  if (denr = 0.0) then
  begin
    // parallels
    if (ua_numr = 0.0) and (ub_numr = 0.0) then
    begin
      // two lines are on one straight line, check the bounds
      aBox.SetValue(GeoCoord(A1.YLat, A1.XLon),
                    GeoCoord(A2.YLat, A2.XLon));

      bBox.SetValue(GeoCoord(B1.YLat, B1.XLon),
                    GeoCoord(B2.YLat, B2.XLon));

      Result := bBox.IsIncludes(A1.AsCoord(), False)
             or bBox.IsIncludes(A2.AsCoord(), False)
             or aBox.IsIncludes(B1.AsCoord(), False)
             or aBox.IsIncludes(B2.AsCoord(), False);
    end
    else
      Result := False;

    Exit;
  end;

  ua := ua_numr / denr;
  ub := ub_numr / denr;

  Result := (ua >= 0.0) and (ua <= 1.0) and (ub >= 0.0) and (ub <= 1.0);
end;

procedure TTransPolygon.GetSegmentBoundingBox(const APoints: TTransPointArray;
  AFrom, ATo: Integer; out ABoundingBox: TGeoBox);
var
  i, iCount: Integer;
  MinLon, MinLat, MaxLon, MaxLat: Double;
begin
  iCount := Length(APoints);
  if (iCount = 0) or (AFrom >= ATo) then
    ABoundingBox.Invalidate();

  MinLon := APoints[AFrom mod iCount].XLon;
  MinLat := APoints[AFrom mod iCount].YLat;
  MaxLon := MinLon;
  MaxLat := MinLat;

  for i := AFrom to ATo-1 do
  begin
    MinLon := min(MinLon, APoints[i mod iCount].XLon);
    MinLat := min(MinLat, APoints[i mod iCount].YLat);
    MaxLon := max(MaxLon, APoints[i mod iCount].XLon);
    MaxLat := max(MaxLat, APoints[i mod iCount].YLat);
  end;

  ABoundingBox.SetValue(GeoCoord(MinLat, MinLon),
                        GeoCoord(MaxLat, MaxLon));
end;

procedure TTransPolygon.ComputeSegmentBoxes(const APoints: TTransPointArray;
  var ASegmentBoxes: TSegmentGeoBoxArray; ABound: Integer; ASegmentSize: Integer);
var
  i, n: Integer;
  box: TSegmentGeoBox;
begin
  Assert(ASegmentSize > 0);
  i := 0;
  while i < ABound do
  begin
    box.FromIndex := i;
    box.ToIndex := min(i + ASegmentSize, ABound);
    GetSegmentBoundingBox(APoints, box.FromIndex, box.ToIndex, box.BBox);
    { TODO : optimize }
    n := Length(ASegmentBoxes);
    SetLength(ASegmentBoxes, n+1);
    ASegmentBoxes[n] := box;
    Inc(i, ASegmentSize);
  end;
end;

function TTransPolygon.FindIntersection(const APoints: TTransPointArray;
  var AStart1, AStart2: Integer): Boolean;
var
  LineBox: TGeoBox;
  SegmentBoxes: TSegmentGeoBoxArray;
  i, j, iCount: Integer;
  i1, i2: TTransPoint;
begin
  // compute b-boxes for path, each 1000 point-long segment
  SetLength(SegmentBoxes, 0);
  iCount := Length(APoints);
  ComputeSegmentBoxes(APoints, SegmentBoxes, iCount, 1000);

  for i := AStart1 to iCount-1 do
  begin
    i1 := APoints[i];
    i2 := APoints[i+1];
    LineBox.SetValue(GeoCoord(i1.YLat, i1.XLon),
                     GeoCoord(i2.YLat, i2.XLon));

    j := i+1;
    while j < iCount do
    begin
      if  (not SegmentBoxes[j div 1000].BBox.IsIntersects(LineBox, False))
      and (not SegmentBoxes[(j+1) div 1000].BBox.IsIntersects(LineBox, False))
      then
      begin
        // round up
        j := j + max(0, 998-(j mod 1000));
        Continue;
      end;
      if (IsLinesIntersect(APoints[i], APoints[i+1], APoints[j], APoints[j+1]))
      then
      begin
        if (APoints[i].IsEqual(APoints[j]) or APoints[i].IsEqual(APoints[j+1])
        or  APoints[i+1].IsEqual(APoints[j]) or APoints[i+1].IsEqual(APoints[j+1]))
        then
          Continue; // ignore sibling edges

        AStart1 := i;
        AStart2 := j;
        Result := True;
        Exit;
      end;
      Inc(j);
    end;
  end;
  Result := False;
end;

procedure TTransPolygon.EnsureSimple(AIsArea: Boolean);
var
  optimised: TTransPointArray;
  i, j, n: Integer;
  IsModified, IsSimple, IsUpdated: Boolean;
begin
  // copy points to vector of TransPointRef for easy manipulation
  SetLength(optimised, FLength+1);
  n := 0;
  for i := 0 to FLength-1 do
  begin
    if Points[i].IsDraw then
    begin
      optimised[n] := Points[i];
      optimised[n].Index := i;
      Inc(n);
    end;
  end;

  if n <= 3 then
    Exit;


  if AIsArea then
  begin
    optimised[n] := optimised[0];
    Inc(n);
  end;
  SetLength(optimised, n);

  IsModified := False;
  IsSimple := False;
  while (not IsSimple) do
  begin
    IsSimple := True;

    i := 0;
    j := 0;
    IsUpdated := True;
    // following while is just performance optimisation,
    // we don't start searching intersections from start again
    while IsUpdated do
    begin
      if FindIntersection(optimised, i, j) then
      begin
        IsSimple := False;
        IsModified := True;
        if AIsArea and (j-i > i + Length(optimised)-j) then
        begin
          { TODO : optimize, replace erase }
          ErasePointsFromArray(optimised, j+1, MaxInt);
          ErasePointsFromArray(optimised, 0, i);
          AddPointToArray(optimised, optimised[0]);
          i := 0;
        end
        else
        begin
          ErasePointsFromArray(optimised, i+1, j+1);
        end;
      end
      else
      begin
        IsUpdated := False;
      end
    end;
  end;
  if (IsModified) then
  begin
    // setup draw property for points remaining in optimised vector
    for i := 0 to FLength-1 do
      Points[i].IsDraw := False;

    for i := 0 to Length(optimised)-1 do
      Points[optimised[i].Index].IsDraw := True;
  end;
end;

function TTransPolygon.CheckCapacity(ALength, AMinLength: Integer): Boolean;
begin
  Result := (ALength >= AMinLength);
  if Result and (Length(Points) < ALength) then
    SetLength(Points, ALength)
  else
    FLength := 0;
end;

procedure TTransPolygon.CalculateStartEnd(ALength: Integer);
var
  i: Integer;
begin
  FLength := 0;
  FStart := ALength;
  FEnd := 0;

  // Calculate start, end and length
  for i := 0 to ALength-1 do
  begin
    if (Points[i].IsDraw) then
    begin
      Inc(FLength);

      if (i < FStart) then
        FStart := i;

      FEnd := i;
    end;
  end;
end;

function TTransPolygon.IsEmpty(): Boolean;
begin
  Result := (FLength = 0);
end;

procedure TTransPolygon.TransformArea(const AProjection: TProjection;
  AOptimize: TTransOptimizeMethod;
  const ANodes: TGeoPointArray;
  AOptimizeErrorTolerance: Double;
  AConstraint: TTransOutputConstraint);
begin
  if not CheckCapacity(Length(ANodes), 2) then
    Exit;

  TransformGeoToPixel(AProjection, ANodes);
  if (AOptimize <> tomNone) then
  begin
    if (AOptimize = tomFast) then
    begin
      DropSimilarPoints(AOptimizeErrorTolerance);
      DropRedundantPointsFast(AOptimizeErrorTolerance);
    end
    else
      DropRedundantPointsDouglasPeucker(AOptimizeErrorTolerance, True);

    DropEqualPoints();

    if (AConstraint = tocSimple) then
      EnsureSimple(True);

    //CalculateStartEnd(Length(ANodes));
    CalculateStartEnd(Length(Points));
  end;
end;

procedure TTransPolygon.TransformWay(const AProjection: TProjection;
  AOptimize: TTransOptimizeMethod; const ANodes: TGeoPointArray;
  AOptimizeErrorTolerance: Double; AConstraint: TTransOutputConstraint);
begin
  if not CheckCapacity(Length(ANodes), 1) then
    Exit;

  TransformGeoToPixel(AProjection, ANodes);
  if (AOptimize <> tomNone) then
  begin
    DropSimilarPoints(AOptimizeErrorTolerance);
    if (AOptimize = tomFast) then
    begin
      DropRedundantPointsFast(AOptimizeErrorTolerance);
    end
    else
      DropRedundantPointsDouglasPeucker(AOptimizeErrorTolerance, False);

    DropEqualPoints();

    if (AConstraint = tocSimple) then
      EnsureSimple(False);

    //CalculateStartEnd(Length(ANodes));
    CalculateStartEnd(Length(Points));
  end;
end;

procedure TTransPolygon.TransformBoundingBox(const AProjection: TProjection;
  AOptimize: TTransOptimizeMethod;
  const ABoundingBox: TGeoBox;
  AOptimizeErrorTolerance: Double;
  AConstraint: TTransOutputConstraint);
var
  Coords: TGeoPointArray;
begin
  SetLength(Coords, 4);

  // left bottom
  Coords[0].Assign(ABoundingBox.MinCoord);
  // left top
  Coords[1].Init(ABoundingBox.MaxCoord.Lat,
                 ABoundingBox.MinCoord.Lon);
  // right top
  Coords[2].Assign(ABoundingBox.MaxCoord);
  // right bottom
  Coords[3].Init(ABoundingBox.MinCoord.Lat,
                 ABoundingBox.MaxCoord.Lon);

  TransformArea(AProjection,
                AOptimize,
                Coords,
                AOptimizeErrorTolerance,
                AConstraint);
end;

function TTransPolygon.GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double): Boolean;
var
  iPos: Integer;
begin
  Result := False;
  if IsEmpty() then
    Exit;

  iPos := FStart;

  while (not Points[iPos].IsDraw) and (iPos < FLength) do
    Inc(iPos);

  AMinX := Points[iPos].XLon;
  AMaxX := AMinX;
  AMinY := Points[iPos].YLat;
  AMaxY := AMinY;

  while (iPos <= FEnd) do
  begin
    if Points[iPos].IsDraw then
    begin
      AMinX := min(AMinX, Points[iPos].XLon);
      AMaxX := max(AMaxX, Points[iPos].XLon);
      AMinY := min(AMinY, Points[iPos].YLat);
      AMaxY := max(AMaxY, Points[iPos].YLat);
    end;
    Inc(iPos);
  end;
  Result := True;
end;

{ TTransBuffer }

procedure TTransBuffer.Init();
begin
  Reset();
end;

procedure TTransBuffer.Assign(AOther: TTransBuffer);
var
  i: Integer;
begin
  FCount := AOther.Count;
  SetLength(Buffer, FCount);
  for i := 0 to FCount-1 do
  begin
    Buffer[i].Assign(AOther.Buffer[i]);
  end;
end;

procedure TTransBuffer.Reset();
begin
  FCount := 0;
end;

function TTransBuffer.PushCoord(X, Y: Double): Integer;
var
  NewSize: Integer;
begin
  Result := FCount;
  if FCount >= Length(Buffer) then
  begin
    NewSize := ((FCount * 3) div 2) + 1;
    SetLength(Buffer, NewSize);
  end;
  Buffer[Result].SetValue(X, Y);
  Inc(FCount);
end;

function TTransBuffer.GenerateParallelWay(AOrgStart, AOrgEnd: Integer;
  AOffset: Double; out AStart, AEnd: Integer): Boolean;
var
  OAX, OAY, OBX, OBY: Double;
  i: Integer;
  det1, det2, addX, addY: Double;
begin
  Result := False;
  if ((AOrgStart+1) > AOrgEnd) or (AOrgEnd >= Count) then
    Exit;  // To avoid "not initialized" warnings

  Normalize(Buffer[AOrgStart].Y - Buffer[AOrgStart+1].Y,
            Buffer[AOrgStart+1].X - Buffer[AOrgStart].X,
            OAX, OAY);

  OAX := AOffset * OAX;
  OAY := AOffset * OAY;

  AStart := PushCoord(Buffer[AOrgStart].X + OAX,
                      Buffer[AOrgStart].Y + OAY);

  for i := AOrgStart+1 to AOrgEnd-1 do
  begin
    Normalize(Buffer[i-1].Y - Buffer[i].Y,
              Buffer[i].X - Buffer[i-1].X,
              OAX, OAY);

    OAX := AOffset * OAX;
    OAY := AOffset * OAY;

    Normalize(Buffer[i].Y - Buffer[i+1].Y,
              Buffer[i+1].X - Buffer[i].X,
              OBX, OBY);

    OBX := AOffset * OBX;
    OBY := AOffset * OBY;


    det1 := Det(OBX - OAX,
                OBY - OAY,
                Buffer[i+1].X - Buffer[i].X,
                Buffer[i+1].Y - Buffer[i].Y);

    det2 := Det(Buffer[i].X - Buffer[i-1].X,
                Buffer[i].Y - Buffer[i-1].Y,
                Buffer[i+1].X - Buffer[i].X,
                Buffer[i+1].Y - Buffer[i].Y);

    if (fabs(det2) > 0.0001) then
    begin
      addX := det1 / det2 * (Buffer[i].X - Buffer[i-1].X);
      addY := det1 / det2 * (Buffer[i].Y - Buffer[i-1].Y);
      if (abs(addX) < (2 * abs(AOffset))) and (abs(addY) < 2 * abs(AOffset)) then
        PushCoord(Buffer[i].X + OAX + addX,
                  Buffer[i].Y + OAY + addY)
      else
      begin
        // cut the edge of too sharp angles
        PushCoord(Buffer[i].X + OAX,
                  Buffer[i].Y + OAY);
        PushCoord(Buffer[i].X + OBX,
                  Buffer[i].Y + OBY);
      end;
    end
    else
    begin
      PushCoord(Buffer[i].X + OAX,
                Buffer[i].Y + OAY);
    end;

    Normalize(Buffer[AOrgEnd-1].Y - Buffer[AOrgEnd].Y,
              Buffer[AOrgEnd].X - Buffer[AOrgEnd-1].X,
              OAX, OAY);

    OAX := AOffset * OAX;
    OAY := AOffset * OAY;

    AEnd := PushCoord(Buffer[AOrgEnd].X + OAX,
                      Buffer[AOrgEnd].Y + OAY);

    Result := True;
  end;
end;

function TTransBuffer.Polylabel(AStart, AEnd: Integer; APrecision: Double): TVertex2D;
var
  MinX, MinY, MaxX, MaxY: Double;
  width, height, cellSize, h, x, y: Double;
  i, numProbes: Integer;
  TmpCell, BestCell, BBoxCell: TPolyCell;
  c: TVertex2D;
  PolyPoints: TVertex2DArray;
begin
  // find the bounding box of the outer ring
  MinX := Buffer[AStart].X;
  MinY := Buffer[AStart].Y;
  MaxX := Buffer[AStart].X;
  MaxY := Buffer[AStart].Y;
  for i := AStart+1 to AEnd do
  begin
    MinX := min(MinX, Buffer[i].X);
    MinY := min(MinY, Buffer[i].Y);
    MaxX := max(MaxX, Buffer[i].X);
    MaxY := max(MaxY, Buffer[i].Y);
  end;

  width := maxX - minX;
  height := maxY - minY;
  cellSize := min(width, height);
  h := cellSize / 2;

  if (cellSize = 0) then
  begin
    Result.SetValue(MinX, MinY);
    Exit;
  end;

  FillVertex2DArray(PolyPoints, AStart, AEnd);

  // a priority queue of cells in order of their "potential" (max distance to polygon)
  FPolyCellQueue.Init();

  // cover polygon with initial cells
  x := MinX;
  while x < MaxX do
  begin
    y := MinY;
    while y < MaxY do
    begin
      TmpCell.Init(x + h, y + h, h, PolyPoints);
      FPolyCellQueue.Push(TmpCell);
      y := y + cellSize;
    end;
    x := x + cellSize;
  end;

  // take centroid as the first best guess
  BestCell.InitCentroid(PolyPoints);

  // special case for rectangular polygons
  BBoxCell.Init(MinX + width / 2, MinY + height / 2, 0, PolyPoints);
  if (BBoxCell.Dist > BestCell.Dist) then
    BestCell := BBoxCell;

  numProbes := FPolyCellQueue.Count;

  while (FPolyCellQueue.Count > 0) do
  begin
    // pick the most promising cell from the queue
    FPolyCellQueue.Pull(TmpCell);

    // update the best cell if we found a better one
    if (TmpCell.Dist > BestCell.Dist) then
    begin
      BestCell := TmpCell;
      //if (debug) console.log('found best %d after %d probes', Math.round(1e4 * cell.d) / 1e4, numProbes);
    end;

    // do not drill down further if there's no chance of a better solution
    if (TmpCell.MaxDist - BestCell.Dist) <= APrecision then
      Continue;

    // split the cell into four cells
    h := TmpCell.HalfSize / 2;
    x := TmpCell.CellCenter.X;
    y := TmpCell.CellCenter.Y;
    TmpCell.Init(x - h, y - h, h, PolyPoints);
    FPolyCellQueue.Push(TmpCell);
    TmpCell.Init(x + h, y - h, h, PolyPoints);
    FPolyCellQueue.Push(TmpCell);
    TmpCell.Init(x - h, y + h, h, PolyPoints);
    FPolyCellQueue.Push(TmpCell);
    TmpCell.Init(x + h, y + h, h, PolyPoints);
    FPolyCellQueue.Push(TmpCell);
    Inc(numProbes, 4);
  end;

  {if (debug) then
  begin
    WriteLn('num probes: ', numProbes);
    WriteLn('best distance: ', BestCell.Dist);
  end; }

  Result := BestCell.CellCenter;
end;

procedure TTransBuffer.FillVertex2DArray(out APolyArr: TVertex2DArray; AStart,
  AEnd: Integer);
var
  i: Integer;
begin
  SetLength(APolyArr, AEnd-AStart+1);
  for i := AStart to AEnd do
    APolyArr[i-AStart] := Buffer[i];
end;

procedure TTransBuffer.TransformArea(const AProjection: TProjection;
  AOptimize: TTransOptimizeMethod; const ANodes: TGeoPointArray; out AStart,
  AEnd: Integer; AOptimizeErrorTolerance: Double);
var
  IsStart: Boolean;
  i: Integer;
begin
  TransPolygon.TransformArea(AProjection, AOptimize, ANodes, AOptimizeErrorTolerance);
  Assert(not TransPolygon.IsEmpty());

  IsStart := True;
  for i := Low(TransPolygon.Points) to High(TransPolygon.Points) do
  begin
    if TransPolygon.Points[i].IsDraw then
    begin
      AEnd := PushCoord(TransPolygon.Points[i].XLon,
                        TransPolygon.Points[i].YLat);

      if IsStart then
      begin
        AStart := AEnd;
        IsStart := False;
      end;
    end;
  end;
end;

function TTransBuffer.TransformWay(const AProjection: TProjection;
  AOptimize: TTransOptimizeMethod; const ANodes: TGeoPointArray; out AStart,
  AEnd: Integer; AOptimizeErrorTolerance: Double): Boolean;
var
  IsStart: Boolean;
  i, n, iFirstDraw, iLastDraw: Integer;
  pPoint: ^TTransPoint;
begin
  Result := False;
  TransPolygon.TransformWay(AProjection, AOptimize, ANodes, AOptimizeErrorTolerance);
  if TransPolygon.IsEmpty() then
    Exit;

  n := 0; // visible count
  for i := 0 to Length(TransPolygon.Points)-1 do
  begin
    pPoint := Addr(TransPolygon.Points[i]);
    // hide off-screen points
    pPoint^.IsVisible := AProjection.IsPixelValid(pPoint^.XLon, pPoint^.YLat, AOptimizeErrorTolerance);
    pPoint^.IsDraw := pPoint^.IsVisible;
    if (i > 0) then
    begin
      if (not pPoint^.IsVisible) and (not TransPolygon.Points[i-1].IsVisible) then
      begin
        // previous point is visible. make this point drawable
        pPoint^.IsDraw := True;
      end
      else if (pPoint^.IsVisible) and (not TransPolygon.Points[i-1].IsDraw) then
      begin
        // this point visible, make previous point drawable
        TransPolygon.Points[i-1].IsDraw := True;
      end;
    end;
    if pPoint^.IsDraw then
      Inc(n);
  end;

  if n < 1 then
    Exit;

  IsStart := True;
  for i := 0 to Length(TransPolygon.Points)-1 do
  begin
    //if TransPolygon.Points[i].IsDraw then
    begin
      AEnd := PushCoord(TransPolygon.Points[i].XLon,
                        TransPolygon.Points[i].YLat);

      if IsStart then
      begin
        AStart := AEnd;
        IsStart := False;
      end;
    end;
  end;
  Result := True;
end;

{ TTransLineSegment }

procedure TTransLineSegment.Init(const A, B: TTransPoint);
begin
  Ref := A;
  XDelta := B.XLon - A.XLon;
  YDelta := B.YLat - A.YLat;
  InverseLength := 1 / ((XDelta * XDelta) + (YDelta * YDelta));
end;

function TTransLineSegment.IsValid(): Boolean;
begin
  Result := not ((XDelta = 0.0) and (YDelta = 0.0));
end;

function TTransLineSegment.CalculateDistanceSquared(const p: TTransPoint): Double;
var
  cx, cy, dx, dy, u: Double;
begin
  cx := p.XLon - Ref.XLon;
  cy := p.YLat - Ref.YLat;
  u := (cx * XDelta + cy * YDelta) * InverseLength;

  u := min(1.0, max(0.0, u));

  dx := cx - u * XDelta; // *-1 but we square below
  dy := cy - u * YDelta; // *-1 but we square below

  Result := (dx * dx) + (dy * dy);
end;

{ TPolyCell }

procedure TPolyCell.Init(AX, AY: Double; AHalfSize: Double;
  const APoly: TVertex2DArray);
var
  IsInside: Boolean = False;
  minDistSq: Double = Infinity;
  i, j, len: Integer;
  A, B: TVertex2D;
begin
  CellCenter.SetValue(AX, AY);
  HalfSize := AHalfSize;

  // signed distance from point to polygon outline (negative if point is outside)
  i := 0;
  len := Length(APoly);
  j := len - 1;
  while i < len do
  begin
    A := APoly[i];
    B := APoly[j];

    if (((A.Y > CellCenter.Y) <> (B.Y > CellCenter.Y))
    and (CellCenter.X < (B.X - A.X) * (CellCenter.Y - A.Y) / (B.Y - A.Y) + A.X))
    then
      IsInside := not IsInside;

    minDistSq := min(minDistSq, CalculateSquaredDistancePointToLineSegment(CellCenter, A, B));
    j := i;
    Inc(i);
  end;

  if IsInside then
    Dist := 1 * sqrt(minDistSq)
  else
    Dist := -1 * sqrt(minDistSq);

  MaxDist := Dist + HalfSize * SQRT2;
end;

procedure TPolyCell.InitCentroid(const APoly: TVertex2DArray);
var
  Area, X, Y, F: Double;
  i, j, len: Integer;
  A, B, C: TVertex2D;
begin
  Area := 0;
  X := 0;
  Y := 0;
  i := 0;
  len := Length(APoly);
  j := len - 1;
  while i < len do
  begin
    A := APoly[i];
    B := APoly[j];

    F := (A.X * B.Y) - (B.X * A.Y);
    X := X + (A.X + B.X) * F;
    Y := Y + (A.Y + B.Y) * F;
    Area := Area + F * 3;

    j := i;
    Inc(i);
  end;

  if Area = 0 then
    C.Assign(APoly[0])
  else
    C.SetValue(X / Area, Y / Area);

  Init(C.X, C.Y, 0, APoly);
end;

{ TPolyCellQueue }

procedure TPolyCellQueue.Init();
begin
  SetLength(FItems, 32);
  FCount := 0;
  FStartIndex := 15;
  FEndIndex := FStartIndex-1;
  FMaxDistMax := 0;
end;

procedure TPolyCellQueue.Push(const AItem: TPolyCell);
begin
  Inc(FCount);
  // items with great MaxDist go to head of queue
  if (AItem.MaxDist >= FMaxDistMax) and (FStartIndex > 0) then
  begin
    FMaxDistMax := AItem.MaxDist;
    Dec(FStartIndex);
    FItems[FStartIndex] := AItem;
  end
  else
  begin
    Inc(FEndIndex);
    if FEndIndex >= Length(FItems)-1 then
      SetLength(FItems, ((FEndIndex * 3) div 2) + 1);

    FItems[FEndIndex] := AItem;
  end;
end;

function TPolyCellQueue.Pull(out AItem: TPolyCell): Boolean;
begin
  Result := (FCount <> 0);
  if Result then
  begin
    Dec(FCount);
    Inc(FStartIndex);
    AItem := FItems[FStartIndex];
  end;
end;

end.

