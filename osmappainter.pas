(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2019  Sergey Bodrov

  Ported from libosmscout library
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
  OsMap rendering routines
MapPainter:
  MapPainter
*)
unit OsMapPainter;

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
  OsMapTypes, OsMapStyles, OsMapGeometry,
  OsMapParameters, OsMapObjTypes, OsMapObjects, OsMapStyleConfig,
  OsMapObjFeatures, OsMapLabels, OsMapTransform, OsMapProjection;

type
  TRenderSteps = (
    rsInitialize,     // Setup internal state of renderer for executing next steps with current projection and parameters
    rsDumpStatistics, // Prints details for debugging, if debug flag (performance, data) is set in renderer parameter
    rsPreprocessData, // Convert geographical coordinates of object points to screen coordinates,
    rsPrerender,      // Implementation specific preparison
    rsDrawGroundTiles, // Draw unknown/sea/land tiles and tiles with "coastlines"
    rsDrawOSMTileGrids,  // If special style exists, renders grid corresponding to OSM tiles
    rsDrawAreas,
    rsDrawWays,
    rsDrawWayDecorations,
    rsDrawWayContourLabels,
    rsPrepareAreaLabels,
    rsDrawAreaBorderLabels,
    rsDrawAreaBorderSymbols,
    rsPrepareNodeLabels,
    rsDrawLabels,
    rsPostrender      // Implementation specific final step
  );

  { Structure used for internal statistic collection }
  TDataStatistic = record
    InfoType: Integer;  // Type
    ObjectCount: Int64; // Sum of nodeCount, wayCount, areaCont
    NodeCount:   Int64; // Number of Node objects
    WayCount:    Int64; // Number of Way objects
    AreaCount:   Int64; // Number of Area objects
    CoordCount:  Int64; // Number of coordinates
    LabelCount:  Int64; // Number of labels
    IconCount:   Int64; // Number of icons
  end;

  { Data structure for holding temporary data about ways }
  TWayData = record
    Ref: TObjectFileRef;
    pBuffer: ^TFeatureValueBuffer; // Features of the line segment
    Layer: Byte;                 // Layer this way is in
    LineStyle: TLineStyle;       // Line style
    WayPriority: Integer;        // Priority of way (from style sheet)
    TransStart: Integer;         // Start of coordinates in transformation buffer
    TransEnd: Integer;           // End of coordinates in transformation buffer
    LineWidth: TReal;           // Line width in pixels
    IsStartClosed: Boolean;      // The end of the way is closed, it does not lead to another way or area
    IsEndClosed: Boolean;        // The end of the way is closed, it does not lead to another way or area
  end;

  TWayDataCompare = function(const A, B: TWayData): Integer;

  { TWayDataList }

  TWayDataList = object
    FCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    Items: array of TWayData;
    procedure Clear();
    function Append(const AItem: TWayData): Integer;
    procedure Sort(ASorter: TWayDataCompare);

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
  end;

  { We then draw lines in order of layer (Smaller layers first)
    Within a layer, we draw lines in order of line style priority (first overlays, lower priority value first)
    Within a style priority, we draw transparent lines over solid lines
    Within a style priority we draw lines in order of style sheet way priority
    (more important ways on top of less important ways, higher priority value first) }
  function CompareWayData(const AItem1, AItem2: TWayData): Integer;

type
  { Data structure for holding temporary data about way paths (a way may consist of
    multiple paths/lines rendered) }
  TWayPathData = record
    Ref: TObjectFileRef;
    pBuffer: ^TFeatureValueBuffer; // Features of the line segment
    pDrawOptions: PMapItemDrawOptions;
    TransStart: Integer;         // Start of coordinates in transformation buffer
    TransEnd: Integer;           // End of coordinates in transformation buffer
  end;

  { TWayPathDataList }

  TWayPathDataList = object
    FCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    Items: array of TWayPathData;
    procedure Clear();
    function Append(const AItem: TWayPathData): Integer;
    //procedure Sort(ASorter: TAreaSorter);

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
  end;

  TPolyData = record
    TransStart: Integer;         // Start of coordinates in transformation buffer
    TransEnd: Integer;           // End of coordinates in transformation buffer
  end;

  { Data structure for holding temporary data about areas }
  TAreaData = record
    Ref: TObjectFileRef;
    TypeInfo: TTypeInfo;         //
    pBuffer: ^TFeatureValueBuffer; // Features of the line segment
    pDrawOptions: PMapItemDrawOptions; // cached draw options
    FillStyle: TFillStyle;       // Fill style
    BorderStyle: TBorderStyle;   // Border style
    BoundingBox: TGeoBox;        // Bounding box of the area (in geo coordinates)
    //VisualCenter: TVertex2D;     // Visual center point for label, in screen coordinates
    IsOuter: Boolean;            // flag if this area is outer ring of some relation
    TransStart: Integer;         // Start of coordinates in transformation buffer
    TransEnd: Integer;           // End of coordinates in transformation buffer
    Clippings: array of TPolyData; // Clipping polygons to be used during drawing of this area
  end;
  PAreaData = ^TAreaData;
  TAreaDataCompare = function(const A, B: TAreaData): Integer;

  { TAreaDataList }

  TAreaDataList = object
  private
    FCount: Integer;
    FItems: array of PAreaData;
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
    function GetPItem(AIndex: Integer): PAreaData;
    procedure QSort(L, R: Integer);
  public
    procedure Clear();
    function Append(const AItem: TAreaData): Integer;
    procedure Sort(ASorter: TAreaDataCompare);

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property PItems[AIndex: Integer]: PAreaData read GetPItem;
  end;

  { TContourLabelHelper }
  { Helper class for drawing contours. Allows the MapPainter base class
    to inject itself at certain points in the contour label rendering code of
    the actual backend. }
  TContourLabelHelper = object
  public
    ContourLabelOffset: TReal;
    ContourLabelSpace: TReal;
    PathLength: TReal;
    TextWidth: TReal;
    CurrentOffset: TReal;

    function Init(APathLength: TReal = 0.0; ATextWidth: TReal = 0.0): Boolean;
    function ContinueDrawing(): Boolean;
    function GetCurrentOffset(): TReal;
    procedure AdvancePartial(AWidth: TReal);
    procedure AdvanceText();
    procedure AdvanceSpace();
  end;

  { render step method }
  TStepMethod = procedure(const AProjection: TProjection;
                          const AMapParameter: TMapParameter;
                          const AMapData: TMapData) of object;

  { TMapPainter }
  { Abstract base class of all renders (though you can always write
   your own renderer without inheriting from this class) It
   implements the general rendering algorithm. Concrete renders are
   implemented by implementing the abstract methods defined by this class
   and used as callbacks to the concrete renderer. }
  TMapPainter = class(TObject)
  private
    //FStepMethods: array [TRenderSteps] of TStepMethod;
    FErrorTolerancePixel: TReal;

    FAreaDataList: TAreaDataList;
    FWayDataList: TWayDataList;
    FWayPathDataList: TWayPathDataList;

    FTextStyles: TTextStyleList; // Temporary storage for StyleConfig return value
    FLineStyles: TLineStyleList; // Temporary storage for StyleConfig return value
    FBorderStyles: TBorderStyleList; // Temporary storage for StyleConfig return value

    { Fallback styles in case they are missing for the style sheet }
    FLandFill: TFillStyle;
    FSeaFill: TFillStyle;
    FCoastlineSegmentAttributes: TFeatureValueBuffer;

    { Precalculations }
    FStandardFontHeight: TReal; // Default font height in pixels
    FAreaMinDimension: TReal;   // Minimal width or height in pixels for visible area

    FOnLog: TGetStrProc;

  private
    { Debugging }
    procedure DumpDataStatistics(const AProjection: TProjection;
                            const AParameter: TMapParameter;
                            const AData: TMapData);

    { Private draw algorithm implementation routines. }

    { Get label text for feature of specified type }
    function GetLabelText(AFeatureType: TFeatureType;
      const AParameter: TMapParameter;
      const ABuffer: TFeatureValueBuffer): string;

    procedure PrepareNode(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ANode: TMapNode);

    procedure PrepareNodes(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    function CalculatePaths(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ARef: TObjectFileRef;
                     const ABuffer: TFeatureValueBuffer;
                     const AWay: TMapWay): Boolean;

    procedure PrepareWays(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure PrepareArea(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AArea: TMapArea);

    procedure PrepareAreaLabel(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AAreaData: TAreaData);

    procedure PrepareAreas(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure RegisterPointWayLabel(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AShieldStyle: TPathShieldStyle;
                     const AText: string;
                     const ANodes: TGeoPointArray);

    { Request layout of a point label
      AProjection - Projection instance to use
      AParameter - General map drawing parameter that might influence the result
      ABuffer - The FeatureValueBuffer of the object that owns the label
      AIconStyle - An optional icon style to use
      ATextStyles - A list of text styles to use (the object could have more than
       label styles attached)
      X, Y - position to place the label at (currently always the center of the area or the coordinate of the node)
      AObjectWidth - The (rough) width of the object
      AObjectHeight - The (rough) height of the object }
    function LayoutPointLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ABuffer: TFeatureValueBuffer;
                     const AIconStyle: TIconStyle;
                     const ATextStyles: TTextStyleList;
                     X, Y: TReal;
                     AObjectWidth: TReal = 0;
                     AObjectHeight: TReal = 0): Boolean;

    function DrawWayDecoration(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TWayPathData): Boolean;

    function CalculateWayShieldLabels(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapWay): Boolean;

    function DrawWayContourLabel(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TWayPathData): Boolean;

    function DrawAreaBorderLabel(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AAreaData: TAreaData): Boolean;

    function DrawAreaBorderSymbol(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AAreaData: TAreaData): Boolean;

    procedure DrawOSMTileGrid(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AMagnification: TMagnification;
                     const AOsmTileLine: TLineStyle);

    { This are the official render step methods. One method for each render step. }

    { Base method that must get called to initial the renderer for a render action.
      The derived method of the concrete renderer implementation can have
      Returns False if there was either an error or of the rendering was already interrupted, else True }
    procedure InitializeRender(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DumpStatistics(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure PreprocessData(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure Prerender(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawGroundTiles(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawOSMTileGrids(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawAreas(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawWays(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawWayDecorations(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawWayContourLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure PrepareAreaLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawAreaBorderLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure DrawAreaBorderSymbols(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure PrepareNodeLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

    procedure Postrender(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData);

  protected
    FDebugLabel: TTextStyle;
    FIsBusy: Boolean;
    { current render step }
    FCurStep: TRenderSteps;
    FCurItemIndex: Integer;
    FCurItemDesc: string;

    FStyleConfig: TStyleConfig; // Reference to the style configuration to be used

    { Scratch variables for path optimization algorithm }
    FTransBuffer: TTransBuffer; // Static (avoid reallocation) buffer of transformed coordinates

    { Attribute readers }
    FNameReader: TFeatureValueReader;
    FNameAltReader: TFeatureValueReader;
    FRefReader: TFeatureValueReader;
    FLayerReader: TFeatureValueReader;
    FWidthReader: TFeatureValueReader;
    FAddressReader: TFeatureValueReader;
    FLanesReader: TFeatureValueReader;
    FAccessReader: TFeatureValueReader;

    { Presets, precalculations and similar }
    FEmptyDash: array of TReal;    // Empty dash array
    FTunnelDash: array of TReal;   // Dash array for drawing tunnel border
    FAreaMarkStyle: TFillStyle;     // Marker fill style for internal debugging
    FContourLabelOffset: TReal;    // Same value as in MapParameter but converted to pixel
    FContourLabelSpace: TReal;     // Same value as in MapParameter but converted to pixel
    FShieldGridSizeHoriz: TReal;   // Width of a cell for shield label placement
    FShieldGridSizeVert: TReal;    // Height of a cell for shield label placement

  protected
    { Useful global helper functions. }

    function IsVisibleArea(const AProjection: TProjection;
                       const ABoundingBox: TGeoBox;
                       APixelOffset: TReal): Boolean;

    { Return True if given ABoundingBox for way path is intersects with
      projection visible area. APixelOffset is half width of Way line, in pixels }
    function IsVisibleWay(const AProjection: TProjection;
                       const ABoundingBox: TGeoBox;
                       APixelOffset: TReal): Boolean;

    procedure Transform(const AProjection: TProjection;
                       const AParameter: TMapParameter;
                       const ACoord: TGeoPoint;
                       var X, Y: TReal);

    { translate meters to pixels, result not less than AMinPixel }
    function GetProjectedWidth(const AProjection: TProjection;
                       AMinPixel, AWidth: TReal): TReal; overload;

    { translate meters to pixels }
    function GetProjectedWidth(const AProjection: TProjection;
                       AWidth: TReal): TReal; overload;


    // GetWayData -> FWayDataList
    // GetAreaData -> FAreaDataList

    { === Low level drawing routines that have to be implemented by
      the concrete drawing engine. }

    { Some optional callbacks between individual processing steps. }
    procedure AfterPreprocessing(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData); virtual;

    procedure BeforeDrawing(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData); virtual; abstract;

    procedure AfterDrawing(const AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData); virtual;

    { Return true, if the icon in the IconStyle is available and can be drawn.
      If this method returns false, possibly a fallback (using a Symbol)
      will be chosen.

      Icon style dimensions and iconId may be setup for later usage. }
    function HasIcon(AStyleConfig: TStyleConfig;
                     AProjection: TProjection;
                     AParameter: TMapParameter;
                     AStyle: TIconStyle): Boolean; virtual; abstract;

    { Returns the height of the font in pixel in relation to the given AFontSize in mm }
    function GetFontHeight(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     AFontSize: TReal): TReal; virtual; abstract;

    { (Optionally) fills the area with the given default color
      for ground. In 2D backends this just fills the given area,
      3D backends might draw a sphere or an infinite plane. }
    procedure DrawGround(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AStyle: TFillStyle); virtual; abstract;

    { Register regular label with given text at the given pixel coordinate
      in a style defined by the given LabelStyle. }
    procedure RegisterRegularLabel(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ALabels: TLabelDataList;
                     const APosition: TVertex2D;
                     AObjectWidth: TReal); virtual; abstract;
    { Register contour label }
    procedure RegisterContourLabel(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ALabel: TPathLabelData;
                     const ALabelPath: TLabelPath); virtual; abstract;
    procedure DrawLabels(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TMapData); virtual; abstract;

    { Draw the Icon as defined by the IconStyle at the given pixel coordinate (icon center). }
    procedure DrawIcon(AStyle: TIconStyle;
                     ACenterX, ACenterY: TReal;
                     AWidth, Aheight: TReal); virtual; abstract;

    { Draw the Symbol as defined by the SymbolStyle at the given pixel coordinate (symbol center). }
    procedure DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: TReal); virtual; abstract;

    { Draw simple line with the given style,the given color, the given width
      and the given untransformed nodes. }
    procedure DrawPath(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AColor: TMapColor;
                     AWidth: TReal;
                     const ADash: array of TReal;
                     AStartCap: TLineCapStyle;
                     AEndCap: TLineCapStyle;
                     ATransStart, ATransEnd: Integer); virtual; abstract;

    { Draw the given text as a contour of the given path in a style defined
      by the given LabelStyle. }
    procedure DrawContourSymbol(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const ASymbol: TMapSymbol;
                     ASpace: TReal;
                     ATransStart, ATransEnd: Integer); virtual; abstract;

    { Draw the given area using the given FillStyle
      for the area outline. }
    procedure DrawArea(const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TAreaData); virtual; abstract;

    { Compute suggested label width for given parameters.
      It may be used by backend for layout labels with wrapping words. }
    function GetProposedLabelWidth(const AParameter: TMapParameter;
                     AAverageCharWidth: TReal;
                     AObjectWidth: TReal;
                     AStringLength: Integer): TReal; virtual;

    procedure DrawWay(AStyleConfig: TStyleConfig;
                     const AProjection: TProjection;
                     const AParameter: TMapParameter;
                     const AData: TWayData); virtual;
  public
    constructor Create(const AStyleConfig: TStyleConfig);
    destructor Destroy; override;

    { Get current renderer state description, for debugging }
    function GetStateStr(): string;

    procedure CheckDebug(AMapData: TMapData);

    {  }
    function Draw(AProjection: TProjection;
                  AParameter: TMapParameter;
                  AData: TMapData;
                  AStartStep: TRenderSteps;
                  AEndStep: TRenderSteps): Boolean;

    function DrawMap(AProjection: TProjection;
                     AParameter: TMapParameter;
                     AData: TMapData): Boolean; virtual;

    property IsBusy: Boolean read FIsBusy;
    property CurrentStep: TRenderSteps read FCurStep;
    property OnLog: TGetStrProc read FOnLog write FOnLog;

    // debug
    property AreaDataList: TAreaDataList read FAreaDataList;
  end;

  {$ifdef FPC}
  TMapPainterList = specialize TFPGList<TMapPainter>;
  {$else}
  TMapPainterList = TList<TMapPainter>;
  {$endif}

  { Batch renderer helps to render map based on multiple databases
    - map data and corresponding MapPainter }

  { TMapPainterBatch }

  TMapPainterBatch = class
  protected
    FData: TMapDataList;
    FPainters: TMapPainterList;

    { Render bach of multiple databases, step by step (\see RenderSteps).
      All painters should have initialised its (backend specific) state. }
    function BatchPaintInternal(const AProjection: TProjection;
                  const AParameter: TMapParameter): Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure AddData(var AData: TMapData; APainter: TMapPainter);
  end;

  { TMapPainterNoOp }

  TMapPainterNoOp = class(TMapPainter)
  protected
    function HasIcon(AStyleConfig: TStyleConfig;
      AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TIconStyle): Boolean; override;

    function GetFontHeight(const AProjection: TProjection;
      const AParameter: TMapParameter; AFontSize: TReal): TReal; override;

    procedure DrawGround(const AProjection: TProjection;
      const AParameter: TMapParameter; const AStyle: TFillStyle); override;

    procedure RegisterRegularLabel(const AProjection: TProjection;
      const AParameter: TMapParameter; const ALabels: TLabelDataList;
      const APosition: TVertex2D; AObjectWidth: TReal); override;

    procedure RegisterContourLabel(const AProjection: TProjection;
      const AParameter: TMapParameter; const ALabel: TPathLabelData;
      const ALabelPath: TLabelPath); override;

    procedure DrawLabels(const AProjection: TProjection;
      const AParameter: TMapParameter; const AData: TMapData); override;

    procedure DrawIcon(AStyle: TIconStyle; ACenterX, ACenterY: TReal;
      AWidth, Aheight: TReal); override;

    procedure DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: TReal); override;

    procedure DrawPath(const AProjection: TProjection;
      const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
      const ADash: array of TReal; AStartCap: TLineCapStyle;
      AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer); override;

    procedure DrawContourSymbol(const AProjection: TProjection;
      const AParameter: TMapParameter; const ASymbol: TMapSymbol;
      ASpace: TReal; ATransStart, ATransEnd: Integer); override;

    procedure DrawArea(const AProjection: TProjection;
      const AParameter: TMapParameter; const AData: TAreaData); override;
  public
    constructor Create(const AStyleConfig: TStyleConfig);
    function DrawMap(AProjection: TProjection; AParameter: TMapParameter; AData: TMapData): Boolean; override;
  end;

  function CompareAreas(const A, B: TMapArea): Integer;


implementation

uses Math, OsMapUtils;

const
  RenderStepsNames:  array[TRenderSteps] of string = (
    'Initialize',
    'DumpStatistics',
    'PreprocessData',
    'Prerender',
    'DrawGroundTiles',
    'DrawOSMTileGrids',
    'DrawAreas',
    'DrawWays',
    'DrawWayDecorations',
    'DrawWayContourLabels',
    'PrepareAreaLabels',
    'DrawAreaBorderLabels',
    'DrawAreaBorderSymbols',
    'PrepareNodeLabels',
    'DrawLabels',
    'Postrender'
  );

function CompareWayData(const AItem1, AItem2: TWayData): Integer;
begin
  Result := 0;
  if AItem1.Layer > AItem2.Layer then
    Result := 1
  else if AItem1.Layer < AItem2.Layer then
    Result := -1;

  if Result <> 0 then Exit;

  if AItem1.LineStyle.ZIndex > AItem2.LineStyle.ZIndex then
    Result := 1
  else if AItem1.LineStyle.ZIndex < AItem2.LineStyle.ZIndex then
    Result := -1;

  if Result <> 0 then Exit;

  if AItem1.LineStyle.Priority > AItem2.LineStyle.Priority then
    Result := 1
  else if AItem1.LineStyle.Priority < AItem2.LineStyle.Priority then
    Result := -1;

  if Result <> 0 then Exit;

  if AItem1.WayPriority > AItem2.WayPriority then
    Result := 1
  else if AItem1.WayPriority < AItem2.WayPriority then
    Result := -1;
end;

function CompareAreaData(const A, B: TAreaData): Integer;
var
  n1, n2: Integer;
begin
  {if A.TypeInfo.ZOrder > B.TypeInfo.ZOrder then
    Exit(1)
  else if A.TypeInfo.ZOrder < B.TypeInfo.ZOrder then
    Exit(-1); }

  if A.pDrawOptions^.ZOrder > B.pDrawOptions^.ZOrder then
    Exit(1)
  else if A.pDrawOptions^.ZOrder < B.pDrawOptions^.ZOrder then
    Exit(-1);

  n1 := StrToIntDef(A.pBuffer^.GetFeatureValue(ftLayer), 1);
  n2 := StrToIntDef(B.pBuffer^.GetFeatureValue(ftLayer), 1);
  if n1 > n2 then
    Exit(1)
  else if (n1 < n2) then
    Exit(-1);

  if Assigned(A.FillStyle) and Assigned(B.FillStyle) then
  begin
    if A.FillStyle.FillColor.IsSolid() and (not B.FillStyle.FillColor.IsSolid()) then
      Exit(1)
    else if (not A.FillStyle.FillColor.IsSolid()) and B.FillStyle.FillColor.IsSolid() then
      Exit(-1);
  end
  else if Assigned(A.FillStyle) then
    Exit(1)
  else if Assigned(B.FillStyle) then
    Exit(-1);


  if (A.BoundingBox.MinCoord.Lon > B.BoundingBox.MinCoord.Lon)
  and (A.BoundingBox.MaxCoord.Lon < B.BoundingBox.MaxCoord.Lon)
  then
    Exit(1)
  else
  if (A.BoundingBox.MinCoord.Lat > B.BoundingBox.MinCoord.Lat)
  and (A.BoundingBox.MaxCoord.Lat < B.BoundingBox.MaxCoord.Lat)
  then
    Exit(1)
  else
    Exit(0);


  {if (A.BoundingBox.MinCoord.Lon = B.BoundingBox.MinCoord.Lon) then
  begin
    if (A.BoundingBox.MaxCoord.Lon = B.BoundingBox.MaxCoord.Lon) then
    begin
      if (A.BoundingBox.MinCoord.Lat = B.BoundingBox.MinCoord.Lat) then
      begin
        if (A.BoundingBox.MaxCoord.Lat = B.BoundingBox.MaxCoord.Lat) then
        begin
          (**
           * Condition for the case when one area exists in two relations
           *  - in one as outer ring (type of relation is used) and in second relation as inner ring.
           * In such case, we want to draw area with outer type after that one of inner type
           *)
          if ((not A.IsOuter) and B.isOuter) then
            Result := 1
          else
            Result := 0;
        end
        else
          Result := Trunc(A.BoundingBox.MaxCoord.Lat - B.BoundingBox.MaxCoord.Lat);
      end
      else
        Result := Trunc(A.BoundingBox.MinCoord.Lat - B.BoundingBox.MinCoord.Lat);
    end
    else
      Result := Trunc(A.BoundingBox.MaxCoord.Lon - B.BoundingBox.MaxCoord.Lon);
  end
  else
    Result := Trunc(A.BoundingBox.MinCoord.Lon - B.BoundingBox.MinCoord.Lon);  }

end;

function CompareAreas(const A, B: TMapArea): Integer;
var
  n1, n2: Integer;
begin
  if A.TypeInfo.ZOrder > B.TypeInfo.ZOrder then
    Exit(1)
  else if A.TypeInfo.ZOrder < B.TypeInfo.ZOrder then
    Exit(-1);

  n1 := StrToIntDef(A.Rings[0].FeatureValueBuffer.GetFeatureValue(ftLayer), 1);
  n2 := StrToIntDef(B.Rings[0].FeatureValueBuffer.GetFeatureValue(ftLayer), 1);
  if n1 > n2 then
    Exit(1)
  else if (n1 < n2) then
    Exit(-1);

  if (A.Rings[0].BBox.MinCoord.Lon > B.Rings[0].BBox.MinCoord.Lon)
  and (A.Rings[0].BBox.MaxCoord.Lon < B.Rings[0].BBox.MaxCoord.Lon)
  then
    Exit(1)
  else
  if (A.Rings[0].BBox.MinCoord.Lat > B.Rings[0].BBox.MinCoord.Lat)
  and (A.Rings[0].BBox.MaxCoord.Lat < B.Rings[0].BBox.MaxCoord.Lat)
  then
    Exit(1);

  if A.FileOffset > B.FileOffset then
    Result := 1
  else if A.FileOffset < B.FileOffset then
    Result := -1
  else
    Result := 0;
end;

function FMod(a, b: TReal): TReal;
begin
  Result := a - b * Int(a / b);
end;

procedure GetGridPoints(const ANodes: TGeoPointArray;
  AGridSizeHoriz, AGridSizeVert: TReal;
  var AIntersections: TGeoPointArray);
var
  i, n, cellXStart, cellYStart, cellXEnd, cellYEnd: Integer;
  lower, upper: TReal;
  xIndex, yIndex: Integer;
  intersection: TGeoPoint;
  xCoord, yCoord: TReal;
begin
  Assert(Length(ANodes) >= 2);

  for i := 0 to Length(ANodes)-2 do
  begin
    cellXStart := Trunc((ANodes[i].Lon + 180.0) / AGridSizeHoriz);
    cellYStart := Trunc((ANodes[i].Lat + 90.0) / AGridSizeVert);

    cellXEnd := Trunc((ANodes[i+1].Lon + 180.0) / AGridSizeHoriz);
    cellYEnd := Trunc((ANodes[i+1].Lat + 90.0) / AGridSizeVert);

    if (cellXStart <> cellXEnd) then
    begin
      lower := min(cellYStart, cellYEnd) * AGridSizeVert - 90.0;
      upper := (max(cellYStart, cellYEnd)+1) * AGridSizeVert - 90.0;

      for xIndex := cellXStart+1 to cellXEnd do
      begin
        xCoord := xIndex * AGridSizeHoriz - 180.0;

        if (GetLineIntersection(ANodes[i],
                                ANodes[i+1],
                                GeoCoord(lower, xCoord),
                                GeoCoord(upper, xCoord),
                                intersection))
        then
        begin
          n := Length(AIntersections);
          SetLength(AIntersections, n+1);
          AIntersections[n].Assign(intersection);
        end;
      end;
    end;

    if (cellYStart <> cellYEnd) then
    begin
      lower := min(cellXStart, cellXEnd) * AGridSizeHoriz - 180.0;
      upper := (max(cellXStart, cellXEnd)+1) * AGridSizeHoriz - 180.0;

      for yIndex := cellYStart+1 to cellYEnd do
      begin
        yCoord := yIndex * AGridSizeVert - 90.0;

        if (GetLineIntersection(ANodes[i],
                                ANodes[i+1],
                                GeoCoord(yCoord, lower),
                                GeoCoord(yCoord, upper),
                                intersection))
        then
        begin
          n := Length(AIntersections);
          SetLength(AIntersections, n+1);
          AIntersections[n].Assign(intersection);
        end;
      end;
    end;
  end;
end;

{ TContourLabelHelper }

function TContourLabelHelper.Init(APathLength: TReal; ATextWidth: TReal
  ): Boolean;
var
  a: TReal;
begin
  Result := False;
  PathLength := APathLength;
  TextWidth := ATextWidth;
  a := PathLength - TextWidth - (2 * ContourLabelOffset);
  if a <= 0.0 then
    Exit;
  CurrentOffset := FMod(a, TextWidth + ContourLabelSpace) / 2 + ContourLabelOffset;
  Result := True;
end;

function TContourLabelHelper.ContinueDrawing(): Boolean;
begin
  Result := (CurrentOffset < PathLength);
end;

function TContourLabelHelper.GetCurrentOffset(): TReal;
begin
  Result := CurrentOffset;
end;

procedure TContourLabelHelper.AdvancePartial(AWidth: TReal);
begin
  CurrentOffset := CurrentOffset + AWidth;
end;

procedure TContourLabelHelper.AdvanceText();
begin
  CurrentOffset := CurrentOffset + TextWidth;
end;

procedure TContourLabelHelper.AdvanceSpace();
begin
  CurrentOffset := CurrentOffset + ContourLabelSpace;
end;

{ TMapPainter }

procedure TMapPainter.DumpDataStatistics(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
begin
  //Assert(False, 'not implemented');
  { TODO : implement }
end;

function TMapPainter.GetLabelText(AFeatureType: TFeatureType;
  const AParameter: TMapParameter; const ABuffer: TFeatureValueBuffer): string;
begin
  Result := ABuffer.GetFeatureValue(AFeatureType);
end;

procedure TMapPainter.PrepareNode(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const ANode: TMapNode);
var
  IconStyle: TIconStyle;
  x, y: TReal;
begin
  IconStyle := AStyleConfig.GetNodeIconStyle(ANode.FeatureValueBuffer, AProjection);

  FTextStyles.Clear();
  AStyleConfig.GetNodeTextStyles(ANode.FeatureValueBuffer, AProjection, FTextStyles);

  Transform(AProjection, AParameter, ANode.Coord, x, y);

  LayoutPointLabels(AProjection,
                    AParameter,
                    ANode.FeatureValueBuffer,
                    IconStyle,
                    FTextStyles,
                    x, y);
end;

procedure TMapPainter.PrepareNodes(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
var
  i: Integer;
  {$ifdef DEBUG_NODE_DRAW}
  times: array of TReal;
  nodeTimer: TStopClockNano;
  overallTime: TReal;
  {$endif}
begin
{$ifdef DEBUG_NODE_DRAW}
  SetLength(times, AStyleConfig.TypeConfig.GetMaxTypeId()+1);
  for i := 0 to High(times) do
    times[i] := 0.0;
{$endif}

  for i := 0 to AData.NodeList.Count-1 do
  begin
    PrepareNode(AStyleConfig, AProjection, AParameter, AData.NodeList.Items[i]);

    {$ifdef DEBUG_NODE_DRAW}
    nodeTimer.Stop();
    times[AData.NodeList.Items[i].GetType().NodeId] += nodeTimer.GetNanoseconds();
    {$endif}
  end;

  for i := 0 to AData.PoiNodeList.Count-1 do
  begin
    PrepareNode(AStyleConfig, AProjection, AParameter, AData.PoiNodeList.Items[i]);

    {$ifdef DEBUG_NODE_DRAW}
    nodeTimer.Stop();
    times[AData.PoiNodeList.Items[i].GetType().NodeId] += nodeTimer.GetNanoseconds();
    {$endif}
  end;

{$ifdef DEBUG_NODE_DRAW}
  for i := 0 to High(times) do
  begin
    overallTime := times[i];

    if (overallTime > 0.0) and Assigned(OnLog) then
      OnLog('Node type ' + AStyleConfig.TypeConfig.GetNodeTypeInfo(i).TypeName + ' ' + FloatToStr(times[i]) + ' nsecs');
  end;
{$endif}
end;

function TMapPainter.CalculatePaths(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const ARef: TObjectFileRef; const ABuffer: TFeatureValueBuffer;
  const AWay: TMapWay): Boolean;
var
  IsTransformed: Boolean;
  TransStart, TransEnd: Integer;
  MainSlotWidth, LineWidth, LineOffset: TReal;
  btAccessValue, btLanesValue, btWidthValue, btLayerValue, btLanes, btLane: Byte;
  i, ii: Integer;
  LineStyle: TLineStyle;
  WayData: TWayData;
  nodes: TGeoPointArray;
  Segment: TSegmentGeoBox;
  WayPathData: TWayPathData;
  LanesSpace, LaneOffset: TReal;
begin
  Result := False;
  if (not IsVisibleWay(AProjection, AWay.GetBoundingBox(), 10)) then
    Exit;

  FLineStyles.Clear();
  AStyleConfig.GetWayLineStyles(ABuffer, AProjection, FLineStyles);

  if (FLineStyles.Count = 0) then
  begin
    AWay.DrawOptions.IsStyleVisible := False;
    Exit;
  end;

  IsTransformed := False;

  TransStart := 0; // Make the compiler happy
  TransEnd := 0;   // Make the compiler happy
  MainSlotWidth := 0.0;

  for LineStyle in FLineStyles do
  begin
    LineWidth := 0.0;
    LineOffset := 0.0;
    btLanesValue := 0;

    if (LineStyle.Width > 0.0) then
    begin
      if FWidthReader.ReadValueByte(ABuffer, btWidthValue) then
        LineWidth := LineWidth + GetProjectedWidth(AProjection, btWidthValue)
      else
        LineWidth := LineWidth + GetProjectedWidth(AProjection, LineStyle.Width);
    end;

    if (LineStyle.DisplayWidthMM > 0.0) then
      LineWidth := LineWidth + AProjection.MillimetersToPixels(LineStyle.DisplayWidthMM);

    if (lineStyle.Slot = '') then
      MainSlotWidth := LineWidth;

    //if (LineWidth = 0.0) then
    if (LineWidth < AParameter.LineMinWidthPixel) then
      Continue;

    if (not IsVisibleWay(AProjection, AWay.GetBoundingBox(), LineWidth / 2)) then
      Exit;

    case LineStyle.OffsetRel of
      lorBase:         LineOffset := 0.0;
      lorLeftOutline:  LineOffset := -MainSlotWidth / 2.0;
      lorRightOutline: LineOffset := MainSlotWidth / 2.0;
      lorLaneDivider:
      begin
        if (not FLanesReader.ReadValueByte(ABuffer, btLanesValue))
        and (not FAccessReader.ReadValueByte(ABuffer, btAccessValue))
        then
        begin
          AWay.DrawOptions.IsStyleVisible := False;
          Exit;
        end;
      end;
    end;

    if (LineStyle.Offset <> 0.0) then
      LineOffset := LineOffset + GetProjectedWidth(AProjection, LineStyle.Offset);

    if (LineStyle.DisplayOffset <> 0.0) then
      LineOffset := LineOffset + AProjection.MillimetersToPixels(LineStyle.DisplayOffset);

    WayData.Ref := ARef;
    WayData.LineWidth := LineWidth;

    //if (not IsVisibleWay(AProjection, AWay.GetBoundingBox(), LineWidth / 2)) then
    //  Continue;

    if (not IsTransformed) then
    begin
      if (Length(AWay.Segments) <= 1) then
      begin
        if not FTransBuffer.TransformWay(AProjection, AParameter.OptimizeWayNodes,
          AWay.Nodes, TransStart, TransEnd, FErrorTolerancePixel)
        then
          Exit;
      end
      else
      begin
        for Segment in AWay.Segments do
        begin
          if (AProjection.GetDimensions().IsIntersects(Segment.BBox, False)) then
          begin
            // TODO: add TransBuffer::Transform* methods with vector subrange (begin/end)
            for ii := Segment.FromIndex to Segment.ToIndex do
            begin
              AppendGeoPointToArray(nodes, AWay.Nodes[ii]);
            end;
          end
          else
          begin
            AppendGeoPointToArray(nodes, AWay.Nodes[Segment.FromIndex]);
            AppendGeoPointToArray(nodes, AWay.Nodes[Segment.ToIndex-1]);
          end;
        end;
        if not FTransBuffer.TransformWay(AProjection,
                                 AParameter.OptimizeWayNodes,
                                 nodes,
                                 TransStart, TransEnd,
                                 FErrorTolerancePixel)
        then
          Exit;
      end;

      WayPathData.Ref := ARef;
      WayPathData.pBuffer := Addr(ABuffer);
      WayPathData.pDrawOptions := Addr(AWay.DrawOptions);
      WayPathData.TransStart := TransStart;
      WayPathData.TransEnd := TransEnd;

      FWayPathDataList.Append(WayPathData);

      IsTransformed := True;
    end;

    WayData.Layer := 0;
    WayData.pBuffer := Addr(ABuffer);
    //WayData.pDrawOptions := Addr(AWay.DrawOptions);
    WayData.LineStyle := LineStyle;
    WayData.WayPriority := AStyleConfig.GetWayPrio(ABuffer.TypeInfo);
    //WayData.IsStartClosed := (AWay.Nodes[0].Serial = 0);
    //WayData.IsEndClosed := (AWay.Nodes[Length(AWay.Nodes)-1].Serial = 0);
    WayData.IsStartClosed := True;
    WayData.IsEndClosed := True;

    if FLayerReader.ReadValueByte(ABuffer, btLayerValue) then
      WayData.Layer := btLayerValue;

    if (LineOffset <> 0.0) then
    begin
      FTransBuffer.GenerateParallelWay(TransStart, TransEnd,
                                       LineOffset,
                                       WayData.TransStart, WayData.TransEnd);
    end
    else
    begin
      WayData.TransStart := TransStart;
      WayData.TransEnd := TransEnd;
    end;

    if (LineStyle.OffsetRel = lorLaneDivider) then
    begin
      btLanes := btLanesValue;

      if TAccessFeature.IsOneway(btAccessValue) then
        btLanes := ABuffer.TypeInfo.OnewayLanes
      else
        btLanes := ABuffer.TypeInfo.Lanes;

      if (btLanes < 2) then
      begin
        AWay.DrawOptions.IsStyleVisible := False;
        Exit;
      end;

      LanesSpace := MainSlotWidth / btLanes;
      LaneOffset := -MainSlotWidth / 2.0 + LanesSpace;

      for btLane := 1 to btLanes do
      begin
        FTransBuffer.GenerateParallelWay(TransStart, TransEnd,
                                         LaneOffset,
                                         WayData.TransStart, WayData.TransEnd);
        FWayDataList.Append(WayData);
        LaneOffset := LaneOffset + LanesSpace;
      end;
    end
    else
      FWayDataList.Append(WayData);

    Result := True;
  end;
end;

procedure TMapPainter.PrepareWays(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
var
  //timer: TStopClock;
  way: TMapWay;
  ZoomLevel: Byte;
  IsPathVisible: Boolean;
begin
  FWayDataList.Clear();
  FWayPathDataList.Clear();

  ZoomLevel := Byte(AProjection.Magnification.Level);

  FCurItemIndex := 0;
  FCurItemDesc := 'Way';
  for way in AData.WayList do
  begin
    if (way.DrawOptions.ZoomLevel <> ZoomLevel) then
    begin
      ResetMapItemDrawOptions(way.DrawOptions);
      way.DrawOptions.ZoomLevel := ZoomLevel;
    end;
    if (way.DrawOptions.IsStyleVisible) then
    begin
      IsPathVisible := CalculatePaths(AStyleConfig, AProjection, AParameter,
        ObjectFileRef(way.FileOffset, refWay),
        way.FeatureValueBuffer, way);
      if IsPathVisible and way.DrawOptions.IsTextLabelVisible then
      begin
        CalculateWayShieldLabels(AStyleConfig, AProjection, AParameter, way);
      end;

    end;

    Inc(FCurItemIndex);
  end;

  FCurItemIndex := 0;
  FCurItemDesc := 'PoiWay';
  for way in AData.PoiWayList do
  begin
    if (way.DrawOptions.ZoomLevel <> ZoomLevel) then
    begin
      ResetMapItemDrawOptions(way.DrawOptions);
      way.DrawOptions.ZoomLevel := ZoomLevel;
    end;
    if (way.DrawOptions.IsStyleVisible) then
    begin
      IsPathVisible := CalculatePaths(AStyleConfig, AProjection, AParameter,
        ObjectFileRef(way.FileOffset, refWay),
        way.FeatureValueBuffer, way);
      if IsPathVisible and way.DrawOptions.IsTextLabelVisible then
      begin
        CalculateWayShieldLabels(AStyleConfig, AProjection, AParameter, way);
      end;
    end;
    Inc(FCurItemIndex);
  end;

  FWayDataList.Sort(@CompareWayData);

end;

procedure TMapPainter.PrepareArea(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AArea: TMapArea);
var
  td: array of TPolyData;
  nodes: TGeoPointArray;
  RingId, BorderStyleIndex: Integer;
  IsFoundRing: Boolean;
  TypeInfo: TTypeInfo;
  FillStyle: TFillStyle;
  BorderStyle: TBorderStyle;
  FillProcessor: TFillStyleProcessor;
  AreaData: TAreaData;
  dBorderWidth, dOffset: TReal;
  ClipCount: Integer;
  TransStart, TransEnd: Integer;
  ZoomLevel: Byte;

  procedure _TransformAreaRing(var ARing: TMapAreaRing; i: Integer);
  var
    Segment: TSegmentGeoBox;
    ii: Integer;
  begin
    if (Length(ARing.Segments) <= 1) then
    begin
      FTransBuffer.TransformArea(AProjection,
                                AParameter.OptimizeAreaNodes,
                                ARing.Nodes,
                                td[i].TransStart, td[i].TransEnd,
                                FErrorTolerancePixel);
    end
    else
    begin
      for Segment in ARing.Segments do
      begin
        if (AProjection.GetDimensions().IsIntersects(Segment.BBox, False)) then
        begin
          // TODO: add TransBuffer::Transform* methods with vector subrange (begin/end)
          for ii := Segment.FromIndex to Segment.ToIndex do
          begin
            AppendGeoPointToArray(nodes, ARing.Nodes[ii]);
          end;
        end
        else
        begin
          AppendGeoPointToArray(nodes, ARing.Nodes[Segment.FromIndex]);
          AppendGeoPointToArray(nodes, ARing.Nodes[Segment.ToIndex-1]);
        end;
      end;
      FTransBuffer.TransformArea(AProjection,
                                AParameter.OptimizeAreaNodes,
                                nodes,
                                td[i].TransStart, td[i].TransEnd,
                                FErrorTolerancePixel);
    end;
  end;

var
  i, ii, j, idx: Integer;
  pRing: ^TMapAreaRing;

begin
  SetLength(td, Length(AArea.Rings));
  FBorderStyles.Clear();

  {for i := 0 to Length(AArea.Rings)-1 do
  begin
    pRing := Addr(AArea.Rings[i]);
    // The master ring does not have any nodes, so we skip it
    // Rings with less than 3 nodes should be skipped, too (no area)
    if pRing^.IsMasterRing() or (Length(pRing^.Nodes) < 3) then
      Continue;
    _TransformAreaRing();
  end; }

  RingId := OUTER_RING_ID;
  IsFoundRing := True;
  ZoomLevel := Byte(AProjection.Magnification.Level);

  i := 0;
  while (IsFoundRing) do
  begin
    IsFoundRing := False;

    if i >= Length(AArea.Rings) then
      Continue;
    //for i := 0 to Length(AArea.Rings)-1 do
    begin
      pRing := Addr(AArea.Rings[i]);

      if pRing^.DrawOptions.ZoomLevel <> ZoomLevel then
      begin
        ResetMapItemDrawOptions(pRing^.DrawOptions);
        pRing^.DrawOptions.ZoomLevel := ZoomLevel;
      end;

      if not pRing^.DrawOptions.IsStyleVisible then
        Continue;

      if (pRing^.Ring <> RingId) then
        Continue;

      AreaData.IsOuter := pRing^.IsOuterRing();
      if (not AreaData.IsOuter) or pRing^.GetType().IsIgnore then
        Continue;

      // The master ring does not have any nodes, so we skip it
      // Rings with less than 3 nodes should be skipped, too (no area)
      if pRing^.IsMasterRing() or (Length(pRing^.Nodes) < 3) then
        Continue;

      AreaData.BoundingBox := pRing^.GetBoundingBox();
      // 10 pixels tolerance around area
      if (not IsVisibleArea(AProjection, AreaData.BoundingBox, 10)) then
        Continue;

      if (AreaData.IsOuter) then
        TypeInfo := AArea.GetType()
      else
        TypeInfo := pRing^.GetType();

      FillStyle := AStyleConfig.GetAreaFillStyle(pRing^.FeatureValueBuffer, AProjection);

      FillProcessor := AStyleConfig.GetFillStyleProcessor(pRing^.GetType().Index);

      if Assigned(FillProcessor) then
      begin
        FillStyle := FillProcessor.Process(pRing^.FeatureValueBuffer, FillStyle);
      end;

      AStyleConfig.GetAreaBorderStyles(pRing^.FeatureValueBuffer, AProjection, FBorderStyles);

      if (not Assigned(FillStyle)) and (FBorderStyles.Count = 0) then
      begin
        pRing^.DrawOptions.IsStyleVisible := False;
        Continue;
      end;

      _TransformAreaRing(pRing^, i);

      borderStyleIndex := 0;

      if (FBorderStyles.Count <> 0)
      and (FBorderStyles[0].DisplayOffset = 0.0)
      and (FBorderStyles[0].Offset = 0.0) then
      begin
        BorderStyle := FBorderStyles[borderStyleIndex];
        Inc(BorderStyleIndex);
      end
      else
        BorderStyle := nil;

      IsFoundRing := True;

      if Assigned(BorderStyle) then
        dBorderWidth := BorderStyle.WidthMM
      else
        dBorderWidth := 0.0;

      {if (not IsVisibleArea(AProjection, AreaData.BoundingBox, dBorderWidth / 2.0)) then
        Continue;  }

      // Collect possible clippings. We only take into account inner rings of the next level
      // that do not have AreaData type and thus act as AreaData clipping region. If AreaData inner pRing^ has AreaData type,
      // we currently assume that it does not have alpha and paints over its region and clipping is
      // not required.
      // Since we know that rings are created deep first, we only take into account direct followers
      // in the list with pRing^+1.
      j := i+1;
      SetLength(AreaData.Clippings, Length(AArea.Rings) - j + 1);
      ClipCount := 0;
      while (j < Length(AArea.Rings))
        and (AArea.Rings[j].Ring = RingId + 1) // inner rings
        and (not AArea.Rings[j].GetType().IsIgnore)
      do
      begin
        _TransformAreaRing(AArea.Rings[j], j);
        AreaData.Clippings[ClipCount] := td[j];
        Inc(ClipCount);
        Inc(j);
      end;
      SetLength(AreaData.Clippings, ClipCount);

      AreaData.Ref := AArea.GetObjectFileRef();
      AreaData.TypeInfo := TypeInfo;
      AreaData.pBuffer := Addr(pRing^.FeatureValueBuffer);
      AreaData.pDrawOptions := Addr(pRing^.DrawOptions);
      AreaData.FillStyle := FillStyle;
      AreaData.BorderStyle := BorderStyle;
      AreaData.TransStart := td[i].TransStart;
      AreaData.TransEnd := td[i].TransEnd;

      FAreaDataList.Append(AreaData);

      for idx := BorderStyleIndex to FBorderStyles.Count-1 do
      begin
        BorderStyle := FBorderStyles[idx];

        dOffset := 0.0;

        TransStart := td[i].TransStart;
        TransEnd := td[i].TransEnd;

        if (BorderStyle.Offset <> 0.0) then
          dOffset := dOffset + GetProjectedWidth(AProjection, BorderStyle.Offset);

        if (BorderStyle.DisplayOffset <> 0.0) then
          dOffset := dOffset + AProjection.MillimetersToPixels(BorderStyle.DisplayOffset);

        if (dOffset <> 0.0) then
          FTransBuffer.GenerateParallelWay(TransStart, TransEnd,
                                           dOffset,
                                           TransStart, TransEnd);

        AreaData.Ref := AArea.GetObjectFileRef();
        AreaData.TypeInfo := TypeInfo;
        AreaData.pBuffer := nil;
        AreaData.pDrawOptions := Addr(pRing^.DrawOptions);
        AreaData.FillStyle := nil;
        AreaData.BorderStyle := BorderStyle;
        AreaData.TransStart := TransStart;
        AreaData.TransEnd := TransEnd;

        FAreaDataList.Append(AreaData);
      end;
      Inc(i, 1 + ClipCount);
    end;

    Inc(RingId);
  end;
end;

procedure TMapPainter.PrepareAreaLabel(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AAreaData: TAreaData);
var
  IconStyle: TIconStyle;
  x1, x2, y1, y2: TReal;
  AreaCenter: TVertex2D;
begin
  IconStyle := AStyleConfig.GetAreaIconStyle(AAreaData.pBuffer^, AProjection);

  FTextStyles.Clear();
  AStyleConfig.GetAreaTextStyles(AAreaData.pBuffer^, AProjection, FTextStyles);

  if (not Assigned(IconStyle)) and (FTextStyles.Count = 0) then
  begin
    AAreaData.pDrawOptions^.IsTextLabelVisible := False;
    Exit;
  end;

  AProjection.GeoToPixel(AAreaData.BoundingBox.MinCoord,
                        x1, y1);

  AProjection.GeoToPixel(AAreaData.BoundingBox.MaxCoord,
                        x2, y2);

  AreaCenter := FTransBuffer.Polylabel(AAreaData.TransStart, AAreaData.TransEnd);

  if not LayoutPointLabels(AProjection, AParameter, AAreaData.pBuffer^,
                    IconStyle, FTextStyles,
                    //(x1 + x2) / 2, (y1 + y2) / 2,
                    AreaCenter.X, AreaCenter.Y,
                    max(x1, x2) - min(x1, x2),
                    max(y1, y2) - min(y1, y2)) then
  begin
    AAreaData.pDrawOptions^.IsTextLabelVisible := False;
  end;
end;

procedure TMapPainter.PrepareAreas(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
var
  i: Integer;
  TmpArea: TMapArea;
begin
  FAreaDataList.Clear();

  //Areas
  FCurItemDesc := 'Area';
  for i := 0 to AData.AreaList.Count-1 do
  begin
    FCurItemIndex := i;

    if AProjection.GeoBoxIsIn(AData.AreaList[i].GetBoundingBox()) then
      PrepareArea(AStyleConfig, AProjection, AParameter, AData.AreaList[i]);
  end;

  //FAreaDataList.Sort(@CompareAreaData);

  // POI Areas
  FCurItemDesc := 'PoiArea';
  for i := 0 to AData.PoiAreaList.Count-1 do
  begin
    FCurItemIndex := i;
    PrepareArea(AStyleConfig, AProjection, AParameter, AData.PoiAreaList[i]);
  end;
end;

procedure TMapPainter.RegisterPointWayLabel(const AProjection: TProjection;
  const AParameter: TMapParameter;
  const AShieldStyle: TPathShieldStyle;
  const AText: string;
  const ANodes: TGeoPointArray);
var
  LbStyle: TLabelStyle;
  gridPoints: TGeoPointArray;
  x, y: TReal;
  i: Integer;
  LbBox: TLabelData;
  LbList: TLabelDataList;
begin
  LbStyle := AShieldStyle.ShieldStyle;

  SetLength(gridPoints, 0);

  GetGridPoints(ANodes,
                FShieldGridSizeHoriz,
                FShieldGridSizeVert,
                gridPoints);

  for i := 0 to High(gridPoints) do
  begin
    AProjection.GeoToPixel(gridPoints[i], x, y);

    LbBox.Init();
    LbBox.Priority := LbStyle.Priority;
    LbBox.Alpha := 1.0;
    LbBox.FontSize := AProjection.MillimetersToPixels(LbStyle.SizeMM);
    LbBox.Style := LbStyle;
    LbBox.Text := AText;

    LbList.Clear();
    LbList.Add(LbBox);
    RegisterRegularLabel(AProjection, AParameter, LbList, Vertex2D(x, y), {proposedWidth} -1);
  end;
end;

function TMapPainter.LayoutPointLabels(const AProjection: TProjection;
  const AParameter: TMapParameter;
  const ABuffer: TFeatureValueBuffer;
  const AIconStyle: TIconStyle;
  const ATextStyles: TTextStyleList;
  X, Y: TReal;
  AObjectWidth: TReal;
  AObjectHeight: TReal): Boolean;
var
  LbLayoutData: TLabelDataList;
  LbData: TLabelData;
  TextStyle: TTextStyle;
  s, sLabel: string;
  dFactor: TReal;
  dHeight, dAlpha, dMaxHeight, dMinAlpha, dNormHeight: TReal;
begin
  Result := False;
  if Assigned(AIconStyle) then
  begin
    if (AIconStyle.IconName <> '')
    and HasIcon(FStyleConfig, AProjection, AParameter, AIconStyle)
    then
    begin
      LbData.Init();
      LbData.DataType := TLabelDataType.ldtIcon;
      LbData.Position := AIconStyle.Position;

      // TODO: add priority to icons
      //LbData.Priority := AIconStyle.Priority;

      LbData.IconStyle := AIconStyle;
      LbData.IconWidth := AIconStyle.Width;
      LbData.IconHeight := AIconStyle.Height;

      LbLayoutData.Add(LbData);
    end
    else if Assigned(AIconStyle.Symbol) then
    begin
      LbData.Init();
      LbData.DataType := TLabelDataType.ldtSymbol;
      LbData.Position := AIconStyle.Position;

      LbData.IconStyle := AIconStyle;

      // TODO: add priority to symbols
      //LbData.Priority := AIconStyle.Priority;

      LbData.IconWidth := AProjection.MillimetersToPixels(AIconStyle.Symbol.Width);
      LbData.IconHeight := AProjection.MillimetersToPixels(AIconStyle.Symbol.Height);

      LbLayoutData.Add(LbData);
    end;
  end;

  for TextStyle in ATextStyles do
  begin
    //sLabel := GetLabelText(TextStyle.FeatureType, AParameter, ABuffer);
    if AParameter.LabelIncludesAddress then
    begin
      sLabel := GetLabelText(ftAddress, AParameter, ABuffer);
      s := GetLabelText(ftName, AParameter, ABuffer);
      if (sLabel <> '') and (s <> '') then
        sLabel := sLabel + ' ' + s
      else
        slabel := sLabel + s;
    end
    else
    begin
      sLabel := GetLabelText(TextStyle.FeatureType, AParameter, ABuffer);
    end;

    if (sLabel = '') then
      Continue;

    LbData.Init();
    LbData.DataType := ldtText;
    LbData.Priority := TextStyle.Priority;

    if AParameter.IsDrawFadings
    and (TextStyle.ScaleAndFadeMagLevel <> 0)
    and (AProjection.Magnification.Level > TextStyle.ScaleAndFadeMagLevel)
    then
    begin
      dFactor := AProjection.Magnification.Level - TextStyle.ScaleAndFadeMagLevel;
      LbData.FontSize := AProjection.MillimetersToPixels(TextStyle.SizeMM);
      if dFactor > 1 then
        LbData.FontSize := LbData.FontSize * power(1.5, dFactor);

      LbData.Alpha := min(TextStyle.TextColor.A / dFactor, 1.0);
    end
    else if (textStyle.IsAutoSize) then
    begin
      //dHeight := abs(AObjectHeight * 0.1);
      dHeight := abs(AObjectHeight * 0.3);

      if (dHeight = 0) or (dHeight < FStandardFontHeight) then
        Continue;

      // Retricts the dHeight of a label to dMaxHeight
      dAlpha := TextStyle.TextColor.A;
      //dMaxHeight := AProjection.Height / 5.0;
      dMaxHeight := AProjection.MillimetersToPixels(AParameter.FontSizeMax);

      if (dHeight > dMaxHeight) then
      begin
        // If the dHeight exceeds dMaxHeight the dAlpha value will be decreased
        dMinAlpha := AProjection.Height;
        dNormHeight := (dHeight - dMaxHeight) / (dMinAlpha - dMaxHeight);
        dAlpha := dAlpha * Min(Max(1-dNormHeight, 0.2), 1.0);
        dHeight := dMaxHeight;
      end;

      LbData.FontSize := dHeight; // dHeight / FStandardFontHeight;
      LbData.Alpha := dAlpha;
    end
    else
    begin
      //LbData.FontSize := TextStyle.Size;
      LbData.FontSize := AProjection.MillimetersToPixels(TextStyle.SizeMM);
      LbData.Alpha := TextStyle.TextColor.A;
    end;

    { TODO : from parameters }
    if LbData.FontSize < 8 then
      Continue;

    LbData.Position := TextStyle.Position;
    LbData.Text := sLabel;
    LbData.Style := TextStyle;

    LbLayoutData.Add(LbData);
  end;

  if (LbLayoutData.Count = 0) then
  begin
    Exit;
  end;

  { TODO : sort }
  {std::stable_sort(LbLayoutData.begin(),
                   LbLayoutData.end(),
                   LabelLayoutDataSorter); }

  RegisterRegularLabel(AProjection, AParameter, LbLayoutData, Vertex2D(X, Y), AObjectWidth);
  Result := True;
end;

function TMapPainter.DrawWayDecoration(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TWayPathData): Boolean;
var
  PathSymbolStyle: TPathSymbolStyle;
  lineOffset, symbolSpace: TReal;
  transStart, transEnd: Integer;
begin
  Result := False;
  PathSymbolStyle := AStyleConfig.GetWayPathSymbolStyle(AData.pBuffer^, AProjection);

  if (not Assigned(PathSymbolStyle)) then
    Exit;

  lineOffset := 0.0;
  transStart := AData.TransStart;
  transEnd := AData.TransEnd;
  symbolSpace := AProjection.MillimetersToPixels(PathSymbolStyle.SymbolSpace);

  if (PathSymbolStyle.Offset <> 0.0) then
    lineOffset := lineOffset + GetProjectedWidth(AProjection, PathSymbolStyle.Offset);

  if (PathSymbolStyle.DisplayOffset <> 0.0) then
    lineOffset := lineOffset + AProjection.MillimetersToPixels(PathSymbolStyle.DisplayOffset);

  if (lineOffset <> 0.0) then
  begin
    FTransBuffer.GenerateParallelWay(transStart,
                                     transEnd,
                                     lineOffset,
                                     transStart,
                                     transEnd);
  end;

  DrawContourSymbol(AProjection,
                    AParameter,
                    PathSymbolStyle.Symbol,
                    symbolSpace,
                    transStart, transEnd);

  Result := True;
end;

function TMapPainter.CalculateWayShieldLabels(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapWay): Boolean;
var
  ShieldStyle: TPathShieldStyle;
  sLabel: string;
begin
  Result := False;
  ShieldStyle := AStyleConfig.GetWayPathShieldStyle(AData.FeatureValueBuffer, AProjection);

  if not Assigned(ShieldStyle) then
  begin
    AData.DrawOptions.IsTextLabelVisible := False;
    Exit;
  end;

  sLabel := GetLabelText(ShieldStyle.ShieldStyle.FeatureType, AParameter, AData.FeatureValueBuffer);
  if ShieldStyle.ShieldStyle.SizeMM = 0 then
    ShieldStyle.ShieldStyle.SizeMM := AParameter.FontSize;

  if (sLabel = '')
  or (Length(AData.Nodes) < 2)
  //or (ShieldStyle.ShieldStyle.Size < 4)
  then
  begin
    AData.DrawOptions.IsTextLabelVisible := False;
    Exit;
  end;

  RegisterPointWayLabel(AProjection,
                        AParameter,
                        ShieldStyle,
                        sLabel,
                        AData.Nodes);

  Result := True;
end;

function TMapPainter.DrawWayContourLabel(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TWayPathData): Boolean;
var
  PathTextStyle: TPathTextStyle;
  lineOffset: TReal;
  transStart, transEnd: Integer;
  sLabel: string;
  LabelData: TPathLabelData;
  LabelPath: TLabelPath;
  j: Integer;
begin
  Result := False;

  PathTextStyle := AStyleConfig.GetWayPathTextStyle(AData.pBuffer^, AProjection);

  if (not Assigned(PathTextStyle)) then
  begin
    AData.pDrawOptions^.IsTextLabelVisible := False;
    Exit;
  end;

  lineOffset := 0.0;
  transStart := AData.TransStart;
  transEnd := AData.TransEnd;
  sLabel := GetLabelText(PathTextStyle.FeatureType, AParameter, AData.pBuffer^);

  if (sLabel = '') then
  begin
    AData.pDrawOptions^.IsTextLabelVisible := False;
    Exit;
  end;

  if (PathTextStyle.Offset <> 0.0) then
    lineOffset := lineOffset + GetProjectedWidth(AProjection, PathTextStyle.Offset);

  if (PathTextStyle.DisplayOffset <> 0.0) then
    lineOffset := lineOffset + AProjection.MillimetersToPixels(PathTextStyle.DisplayOffset);

  if (lineOffset <> 0.0) then
  begin
    FTransBuffer.GenerateParallelWay(transStart,
                                     transEnd,
                                     lineOffset,
                                     transStart,
                                     transEnd);
  end;

  LabelData.Init(PathTextStyle);
  LabelData.Text := sLabel;
  LabelData.ContourLabelOffset := FContourLabelOffset;
  LabelData.ContourLabelSpace := FContourLabelSpace;

  // TODO: use coordBuffer for label path
  LabelPath.Init();

  for j := transStart to transEnd do
  begin
    LabelPath.AddPoint(FTransBuffer.Buffer[j].X,
                       FTransBuffer.Buffer[j].Y);
  end;
  RegisterContourLabel(AProjection, AParameter, LabelData, LabelPath);

  Result := True;
end;

function TMapPainter.DrawAreaBorderLabel(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection;
  const AParameter: TMapParameter;
  const AAreaData: TAreaData): Boolean;
var
  BorderTextStyle: TPathTextStyle;
  lineOffset: TReal;
  transStart, transEnd: Integer;
  sLabel: string;
  helper: TContourLabelHelper;
  labelPath: TLabelPath;
  j: Integer;
  labelData: TPathLabelData;
begin
  Result := False;
  BorderTextStyle := AStyleConfig.GetAreaBorderTextStyle(AAreaData.pBuffer^, AProjection);

  if (not Assigned(borderTextStyle)) then
  begin
    AAreaData.pDrawOptions^.IsBorderLabelVisible := False;
    Exit;
  end;

  lineOffset := 0.0;
  transStart := AAreaData.TransStart;
  transEnd := AAreaData.TransEnd;
  sLabel := GetLabelText(BorderTextStyle.FeatureType, AParameter, AAreaData.pBuffer^);

  if (sLabel = '') then
  begin
    AAreaData.pDrawOptions^.IsBorderLabelVisible := False;
    Exit;
  end;

  if (BorderTextStyle.Offset <> 0.0) then
    lineOffset := lineOffset + GetProjectedWidth(AProjection, BorderTextStyle.Offset);

  if (borderTextStyle.DisplayOffset <> 0.0) then
    lineOffset := lineOffset + AProjection.MillimetersToPixels(BorderTextStyle.DisplayOffset);

  if (lineOffset <> 0.0) then
    FTransBuffer.GenerateParallelWay(transStart,
                                     transEnd,
                                     lineOffset,
                                     transStart,
                                     transEnd);


  helper.Init();

  // TODO: use coordBuffer for label path
  labelPath.Init();

  for j := transStart to transEnd do
  begin
    labelPath.AddPoint(FTransBuffer.Buffer[j].X, FTransBuffer.Buffer[j].Y);
  end;

  labelData.Init(BorderTextStyle);
  labelData.Text := sLabel;
  labelData.ContourLabelOffset := FContourLabelOffset;
  labelData.ContourLabelSpace := FContourLabelSpace;

  RegisterContourLabel(AProjection, AParameter, labelData, labelPath);

  Result := True;
end;

function TMapPainter.DrawAreaBorderSymbol(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AAreaData: TAreaData): Boolean;
var
  BorderSymbolStyle: TPathSymbolStyle;
  lineOffset: TReal;
  transStart, transEnd: Integer;
  symbolSpace: TReal;
begin
  Result := False;
  BorderSymbolStyle := AStyleConfig.GetAreaBorderSymbolStyle(AAreaData.pBuffer^, AProjection);

  if (not Assigned(BorderSymbolStyle)) then
    Exit;

  lineOffset := 0.0;
  transStart := AAreaData.TransStart;
  transEnd := AAreaData.TransEnd;
  symbolSpace := AProjection.MillimetersToPixels(BorderSymbolStyle.SymbolSpace);

  if (BorderSymbolStyle.Offset <> 0.0) then
    lineOffset := lineOffset + GetProjectedWidth(AProjection, BorderSymbolStyle.Offset);

  if (BorderSymbolStyle.DisplayOffset <> 0.0) then
    lineOffset := lineOffset + AProjection.MillimetersToPixels(BorderSymbolStyle.DisplayOffset);

  if (lineOffset <> 0.0) then
  begin
    FTransBuffer.GenerateParallelWay(transStart,
                                     transEnd,
                                     lineOffset,
                                     transStart,
                                     transEnd);
  end;

  DrawContourSymbol(AProjection,
                    AParameter,
                    BorderSymbolStyle.Symbol,
                    symbolSpace,
                    transStart, transEnd);

  Result := True;
end;

procedure TMapPainter.DrawOSMTileGrid(const AProjection: TProjection;
  const AParameter: TMapParameter; const AMagnification: TMagnification;
  const AOsmTileLine: TLineStyle);
var
  BoundingBox: TGeoBox;
  TileA, TileB: TOSMTileXY;
  StartTileX, EndTileX: LongWord;
  StartTileY, EndTileY: LongWord;
  Points: TGeoPointArray;
  x, y: LongWord;
  TransStart, TransEnd: Integer;
  WayItem: TWayData;
begin
  BoundingBox := AProjection.GetDimensions();

  TileA := GetOSMTile(AMagnification, BoundingBox.MinCoord);
  TileB := GetOSMTile(AMagnification, BoundingBox.MaxCoord);
  StartTileX := min(TileA.X, TileB.X);
  EndTileX := max(TileA.X, TileB.X);
  StartTileY := min(TileA.Y, TileB.Y);
  EndTileY := max(TileA.Y, TileB.Y);

  if (StartTileX > 0) then
    Dec(StartTileX);

  if (StartTileY > 0) then
    Dec(StartTileY);

  // --- Horizontal lines

  for y := StartTileY to EndTileY do
  begin
    SetLength(Points, EndTileX - StartTileX + 1);

    for x := StartTileX to EndTileX do
    begin
      Points[x-StartTileX].Assign(OSMTileXY(x, y).GetTopLeftCoord(AMagnification));
    end;

    FTransBuffer.TransformWay(AProjection,
                              AParameter.OptimizeWayNodes,
                              Points,
                              TransStart,
                              TransEnd,
                              FErrorTolerancePixel);

    WayItem.pBuffer := Addr(FCoastlineSegmentAttributes);
    WayItem.Layer := 0;
    WayItem.LineStyle := AOsmTileLine;
    WayItem.WayPriority := MaxInt;
    WayItem.TransStart := TransStart;
    WayItem.TransEnd := TransEnd;
    WayItem.LineWidth := GetProjectedWidth(AProjection,
                                     AProjection.MillimetersToPixels(AOsmTileLine.DisplayWidthMM),
                                     AOsmTileLine.Width);
    WayItem.IsStartClosed := False;
    WayItem.IsEndClosed := False;

    FWayDataList.Append(WayItem);
  end;

  // --- Vertical lines

  for x := StartTileX to EndTileX do
  begin
    SetLength(Points, EndTileY - StartTileY + 1);

    for y := StartTileY to EndTileY do
    begin
      Points[y-StartTileY] := OSMTileXY(x, y).GetTopLeftCoord(AMagnification);
    end;

    FTransBuffer.TransformWay(AProjection,
                              AParameter.OptimizeWayNodes,
                              Points,
                              TransStart,
                              TransEnd,
                              FErrorTolerancePixel);

    WayItem.pBuffer := Addr(FCoastlineSegmentAttributes);
    WayItem.Layer := 0;
    WayItem.LineStyle := AOsmTileLine;
    WayItem.WayPriority := MaxInt;
    WayItem.TransStart := TransStart;
    WayItem.TransEnd := TransEnd;
    WayItem.LineWidth:= GetProjectedWidth(AProjection,
                                     AProjection.MillimetersToPixels(AOsmTileLine.DisplayWidthMM),
                                     AOsmTileLine.Width);
    WayItem.IsStartClosed := False;
    WayItem.IsEndClosed := False;

    FWayDataList.Append(WayItem);
  end;
end;

procedure TMapPainter.InitializeRender(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
begin
  FErrorTolerancePixel := AProjection.MillimetersToPixels(AParameter.IsOptimizeErrorToleranceMm);
  FAreaMinDimension    := AProjection.MillimetersToPixels(AParameter.AreaMinDimensionMM);
  FContourLabelOffset  := AProjection.MillimetersToPixels(AParameter.ContourLabelOffset);
  FContourLabelSpace   := AProjection.MillimetersToPixels(AParameter.ContourLabelSpace);

  FShieldGridSizeHoriz := 360.0 / (power(2, AProjection.Magnification.Level+1));
  FShieldGridSizeVert  := 180.0 / (power(2, AProjection.Magnification.Level+1));

  FTransBuffer.Reset();
  { TODO : ivestigate }
  //FStandardFontHeight    := GetFontHeight(AProjection, AParameter, 1.0);
  FStandardFontHeight := AProjection.MillimetersToPixels(AParameter.FontSize);
end;

procedure TMapPainter.DumpStatistics(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
begin
  if AParameter.IsDebugPerformance then
  begin
    //log.Info()
    //  << "Data: " << data.nodes.size() << "+" << data.poiNodes.size() << " " << data.ways.size() << " " << data.areas.size();
  end;

  if (AParameter.WarningCoordCountLimit > 0)
  or (AParameter.WarningObjectCountLimit > 0)
  or (AParameter.IsDebugData) then
  begin
    DumpDataStatistics(AProjection, AParameter, AData);
  end;
end;

procedure TMapPainter.PreprocessData(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  prepareWaysTimer, prepareAreasTimer: TStopClock;
begin
  CheckDebug(AData);
  prepareWaysTimer.Init();
  PrepareWays(FStyleConfig, AProjection, AParameter, AData);
  prepareWaysTimer.Stop();

  if AParameter.IsAborted then
    Exit;

  CheckDebug(AData);
  prepareAreasTimer.Init();
  PrepareAreas(FStyleConfig, AProjection, AParameter, AData);
  prepareAreasTimer.Stop();

  // Optional callback after preprocessing data
  AfterPreprocessing(FStyleConfig, AProjection, AParameter, AData);

  if AParameter.IsDebugPerformance and Assigned(OnLog)
  and (prepareWaysTimer.IsSignificant() or prepareAreasTimer.IsSignificant())
  then
  begin
    OnLog('Prep ways: ' + prepareWaysTimer.ResultString() + ' (sec), areas: ' + prepareAreasTimer.ResultString() + ' (sec)');
  end;
end;

procedure TMapPainter.Prerender(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  boundingBox: TGeoBox;
begin
  BeforeDrawing(FStyleConfig, AProjection, AParameter, AData);

  if (AParameter.IsDebugPerformance) and Assigned(OnLog) then
  begin
    boundingBox := AProjection.GetDimensions();

    OnLog('Draw: ' + boundingBox.GetDisplayText()
      + ' zoom: ' + FloatToStr(AProjection.Magnification.Magnification) + 'x/' + IntToStr(AProjection.Magnification.Level)
      + ' screen: ' + IntToStr(AProjection.Width) + 'x' + IntToStr(AProjection.Height) + ' ' + FloatToStr(AProjection.DPI) + ' DPI');
  end;

end;

procedure TMapPainter.DrawGroundTiles(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  {$ifdef DEBUG_GROUNDTILES}
  drawnLabels: TGeoCoordArray;
  cc: TGeoCoord;
  sLabel: string;
  labelBox: TLabelData;
  vect: TLabelDataList
  {$endif}
  landFill, seaFill, coastFill, unknownFill: TFillStyle;
  coastlineLine: TLineStyle;
  points: TGeoPointArray;
  i, iStart, iEnd, ist, idx, iLen: Integer;
  tile: TGroundTile;
  areaData: TAreaData;
  minCoord, maxCoord: TGeoPoint;
  lat, lon: TReal;
  x, y, px, py: TReal;
  lineStart, lineEnd: Integer;
  wd: TWayData;
begin
  landFill := FStyleConfig.GetLandFillStyle(AProjection);

  if (not Assigned(landFill)) then
    landFill := Self.FLandFill;

  if AParameter.IsRenderBackground then
    DrawGround(AProjection, AParameter, landFill);

  if (not AParameter.IsRenderSeaLand) then
    Exit;

  seaFill := FStyleConfig.GetSeaFillStyle(AProjection);
  coastFill := FStyleConfig.GetCoastFillStyle(AProjection);
  unknownFill := FStyleConfig.GetUnknownFillStyle(AProjection);
  coastlineLine := FStyleConfig.GetCoastlineLineStyle(AProjection);
  iStart := 0;
  iEnd := 0;

  if (not Assigned(seaFill)) then
    seaFill := FSeaFill;

  FCurItemIndex := -1;
  for tile in AData.GroundTileList do
  begin
    Inc(FCurItemIndex);
    if (tile.TileType = TGroundTileType.gtUnknown) and (not AParameter.IsRenderUnknowns) then
      Continue;

    {$ifdef DEBUG_GROUNDTILES}
    case tile.TileType of
      gtLand:    WriteLn('Drawing land tile: ', tile.XRel, ',', tile.YRel);
      gtWater:   WriteLn('Drawing water tile: ', tile.XRel, ',', tile.YRel);
      gtCoast:   WriteLn('Drawing coast tile: ', tile.XRel, ',', tile.YRel);
      gtUnknown: WriteLn('Drawing unknown tile: ', tile.XRel, ',', tile.YRel);
    end;
    {$endif}

    case tile.TileType of
      gtLand:    areaData.FillStyle := landFill;
      gtWater:   areaData.FillStyle := seaFill;
      gtCoast:   areaData.FillStyle := coastFill;
      gtUnknown: areaData.FillStyle := unknownFill;
    end;

    if (not Assigned(areaData.FillStyle)) then
      Continue;

    minCoord.Lat := tile.YAbs * tile.CellHeight - 90.0;
    minCoord.Lon := tile.XAbs * tile.CellWidth - 180.0;
    maxCoord.Lat := minCoord.Lat + tile.CellHeight;
    maxCoord.Lon := minCoord.Lon + tile.CellWidth;

    areaData.BoundingBox.SetValue(minCoord, maxCoord);

    if (Length(tile.Coords) = 0) then
    begin
      {$ifdef DEBUG_GROUNDTILES}
      WriteLn(' >= fill');
      {$endif}
      // Fill the cell completely with the fill for the given cell type
      SetLength(points, 5);

      points[0].Assign(areaData.BoundingBox.MinCoord);
      points[1].Init(areaData.BoundingBox.MinCoord.Lat,
                     areaData.BoundingBox.MaxCoord.Lon);
      points[2].Assign(areaData.BoundingBox.MaxCoord);
      points[3].Init(areaData.BoundingBox.MaxCoord.Lat,
                     areaData.BoundingBox.MinCoord.Lon);
      points[4] := points[0];

      FTransBuffer.TransPolygon.TransformArea(AProjection, tomNone, points, FErrorTolerancePixel);

      ist := FTransBuffer.TransPolygon.StartIndex;

      iStart := FTransBuffer.PushCoord(floor(FTransBuffer.TransPolygon.Points[ist+0].XLon),
                                        ceil(FTransBuffer.TransPolygon.Points[ist+0].YLat));


      FTransBuffer.PushCoord(ceil(FTransBuffer.TransPolygon.Points[ist+1].XLon),
                             ceil(FTransBuffer.TransPolygon.Points[ist+1].YLat));

      FTransBuffer.PushCoord(ceil(FTransBuffer.TransPolygon.Points[ist+2].XLon),
                            floor(FTransBuffer.TransPolygon.Points[ist+2].YLat));

      FTransBuffer.PushCoord(floor(FTransBuffer.TransPolygon.Points[ist+3].XLon),
                             floor(FTransBuffer.TransPolygon.Points[ist+3].YLat));

      iEnd := FTransBuffer.PushCoord(floor(FTransBuffer.TransPolygon.Points[ist+4].XLon),
                                      ceil(FTransBuffer.TransPolygon.Points[ist+4].YLat));
    end
    else
    begin
      {$ifdef DEBUG_GROUNDTILES}
      WriteLn(' >= sub');
      {$endif}
      SetLength(points, Length(tile.Coords));

      for i := 0 to Length(tile.Coords)-1 do
      begin
        lat := areaData.BoundingBox.MinCoord.Lat + (tile.Coords[i].Y * tile.CellHeight / GROUND_TILE_CELL_MAX);
        lon := areaData.BoundingBox.MinCoord.Lon + (tile.Coords[i].X * tile.CellWidth / GROUND_TILE_CELL_MAX);

        points[i].Init(lat, lon);
      end;

      FTransBuffer.TransPolygon.TransformArea(AProjection, tomNone, points, FErrorTolerancePixel);

      for i := FTransBuffer.transPolygon.StartIndex to FTransBuffer.TransPolygon.EndIndex do
      begin
        if (tile.Coords[i].X = 0) then
          x := floor(FTransBuffer.TransPolygon.Points[i].XLon)
        else if (tile.Coords[i].X = GROUND_TILE_CELL_MAX) then
          x := ceil(FTransBuffer.TransPolygon.Points[i].XLon)
        else
          x := FTransBuffer.TransPolygon.Points[i].XLon;

        if (tile.Coords[i].Y = 0) then
          y := ceil(FTransBuffer.TransPolygon.Points[i].YLat)
        else if (tile.Coords[i].Y = GROUND_TILE_CELL_MAX) then
          y := floor(FTransBuffer.TransPolygon.Points[i].YLat)
        else
          y := FTransBuffer.TransPolygon.Points[i].YLat;

        idx := FTransBuffer.PushCoord(x, y);

        if (i = FTransBuffer.TransPolygon.StartIndex) then
          iStart := idx
        else if (i = FTransBuffer.TransPolygon.EndIndex) then
          iEnd := idx;
      end;

      if Assigned(coastlineLine) then
      begin
        lineStart := 0;
        iLen := Length(tile.Coords);
        while (lineStart < iLen) do
        begin
          while (lineStart < iLen) and (not tile.Coords[lineStart].IsCoast) do
            Inc(lineStart);

          if (lineStart >= iLen) then
            Continue;

          lineEnd := lineStart;

          while (lineEnd < iLen) and (tile.Coords[lineEnd].IsCoast) do
            Inc(lineEnd);

          if (lineStart <> lineEnd) then
          begin
            wd.pBuffer := Addr(FCoastlineSegmentAttributes);
            wd.Layer := 0;
            wd.LineStyle := coastlineLine;
            wd.WayPriority := MaxInt;
            wd.TransStart := iStart + lineStart;
            wd.TransEnd := iStart + lineEnd;
            wd.LineWidth := GetProjectedWidth(AProjection,
                                           AProjection.MillimetersToPixels(coastlineLine.DisplayWidthMM),
                                           coastlineLine.Width);
            wd.IsStartClosed := False;
            wd.IsEndClosed := False;

            FWayDataList.Append(wd);
          end;

          lineStart := lineEnd+1;
        end;
      end;
    end;

    areaData.Ref.Invalidate();
    areaData.TransStart := iStart;
    areaData.TransEnd := iEnd;

    DrawArea(AProjection, AParameter, areaData);

    {$ifdef DEBUG_GROUNDTILES}
    cc := areaData.BoundingBox.GetCenter();

    x := (cc.Lon + 180) / tile.CellWidth;
    y := (cc.Lat + 90) / tile.CellHeight;

    sLabel := IntToStr(tile.XRel) + ',' + IntToStr(tile.YRel);

    lon := (x * tile.CellWidth + tile.CellWidth / 2) - 180.0;
    lat := (y * tile.CellHeight + tile.CellHeight / 2) - 90.0;

    AProjection.GeoToPixel(GeoCoord(lat, lon), px, py);

    if FindGeoCoordInArray(drawnLabels, GeoCoord(y, x)) <> -1 then
      Continue;

    labelBox.Priority := 0;
    labelBox.Alpha := FDebugLabel.TextColor.A;
    labelBox.FontSize := FDebugLabel.Size;
    labelBox.Style := FDebugLabel;
    labelBox.text=label;

    vect.Append(labelBox);
    RegisterRegularLabel(AProjection, AParameter, vect, Vertex2D(px, py), -1);

    AppendGeoCoordToArray(drawnLabels, GeoCoord(y, x));
    {$endif}
  end;
end;

procedure TMapPainter.DrawOSMTileGrids(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  OsmSubTileLine, OsmTileLine: TLineStyle;
  Magnification: TMagnification;
begin
  OsmSubTileLine := FStyleConfig.GetOSMSubTileBorderLineStyle(AProjection);

  if Assigned(OsmSubTileLine) then
  begin
    Magnification.Assign(AProjection.Magnification);
    Magnification.Inc();

    DrawOSMTileGrid(AProjection, AParameter, Magnification, OsmSubTileLine);
  end;

  OsmTileLine := FStyleConfig.GetOSMTileBorderLineStyle(AProjection);

  if Assigned(OsmTileLine) then
  begin
    Magnification.Assign(AProjection.Magnification);

    DrawOSMTileGrid(AProjection, AParameter, Magnification, OsmTileLine);
  end;
end;

procedure TMapPainter.DrawAreas(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  for i := 0 to FAreaDataList.Count-1 do
  begin
    FCurItemIndex := i;
    DrawArea(AProjection, AParameter, FAreaDataList.PItems[i]^);
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw areas: ' + IntToStr(FAreaDataList.Count) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.DrawWays(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  for i := 0 to FWayDataList.Count-1 do
  begin
    FCurItemIndex := i;
    DrawWay(FStyleConfig, AProjection, AParameter, FWayDataList.Items[i]);
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw ways: ' + IntToStr(FWayDataList.Count) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.DrawWayDecorations(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i, n: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  n := 0;
  for i := 0 to FWayPathDataList.Count-1 do
  begin
    FCurItemIndex := i;
    if DrawWayDecoration(FStyleConfig, AProjection, AParameter, FWayPathDataList.Items[i]) then
      Inc(n);
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw way decorations: ' + IntToStr(n) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.DrawWayContourLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i, n: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  n := 0;
  for i := 0 to FWayPathDataList.Count-1 do
  begin
    FCurItemIndex := i;
    if not FWayPathDataList.Items[i].pDrawOptions^.IsTextLabelVisible then
      Continue;

    if DrawWayContourLabel(FStyleConfig, AProjection, AParameter, FWayPathDataList.Items[i]) then
      Inc(n);
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw way contour labels: ' + IntToStr(n) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.PrepareAreaLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i, n: Integer;
  pAreaData: ^TAreaData;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  n := 0;
  for i := 0 to FAreaDataList.Count-1 do
  begin
    FCurItemIndex := i;
    pAreaData := FAreaDataList.PItems[i];
    if Assigned(pAreaData^.pBuffer) and pAreaData^.pDrawOptions^.IsTextLabelVisible then
    begin
      PrepareAreaLabel(FStyleConfig, AProjection, AParameter, pAreaData^);
      Inc(n);
    end;
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Prepare area labels: ' + IntToStr(n) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.DrawAreaBorderLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i, n: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  n := 0;
  for i := 0 to FAreaDataList.Count-1 do
  begin
    FCurItemIndex := i;
    if Assigned(FAreaDataList.PItems[i]^.pBuffer) and FAreaDataList.PItems[i]^.pDrawOptions^.IsBorderLabelVisible then
    begin
      if DrawAreaBorderLabel(FStyleConfig, AProjection, AParameter, FAreaDataList.PItems[i]^) then
        Inc(n);
    end;
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw area border labels: ' + IntToStr(n) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.DrawAreaBorderSymbols(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
  i, n: Integer;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  n := 0;
  for i := 0 to FAreaDataList.Count-1 do
  begin
    FCurItemIndex := i;
    if Assigned(FAreaDataList.PItems[i]^.pBuffer) then
    begin
      if DrawAreaBorderSymbol(FStyleConfig, AProjection, AParameter, FAreaDataList.PItems[i]^) then
        Inc(n);
    end;
  end;

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Draw area border symbols: ' + IntToStr(n) + ' (pcs) ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.PrepareNodeLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  Timer: TStopClock;
begin
  if AParameter.IsDebugPerformance then
    Timer.Init();

  PrepareNodes(FStyleConfig, AProjection, AParameter, AData);

  if AParameter.IsDebugPerformance then
  begin
    Timer.Stop();
    if Timer.IsSignificant() and Assigned(OnLog) then
      OnLog('Prepare node labels: ' + Timer.ResultString() + ' (s)');
  end;
end;

procedure TMapPainter.Postrender(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
begin
  AfterDrawing(FStyleConfig, AProjection, AParameter, AData);
end;

function TMapPainter.IsVisibleArea(const AProjection: TProjection;
  const ABoundingBox: TGeoBox; APixelOffset: TReal): Boolean;
var
  x, y, xMin, xMax, yMin, yMax: TReal;
begin
  Result := False;
  if (not ABoundingBox.Valid) then
    Exit;

  AProjection.GeoToPixel(ABoundingBox.MinCoord, x, y);

  xMin := x;
  xMax := x;
  yMin := y;
  yMax := y;

  AProjection.GeoToPixel(ABoundingBox.MaxCoord, x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  AProjection.GeoToPixel(GeoCoord(ABoundingBox.MinCoord.Lat, ABoundingBox.MaxCoord.Lon),
                         x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  AProjection.GeoToPixel(GeoCoord(ABoundingBox.MaxCoord.Lat, ABoundingBox.MinCoord.Lon),
                         x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  xMin := xMin - APixelOffset;
  xMax := xMax + APixelOffset;
  yMin := yMin - APixelOffset;
  yMax := yMax + APixelOffset;

  if ((xMax-xMin <= FAreaMinDimension) and (yMax-yMin <= FAreaMinDimension)) then
    Exit;

  Result := not ((xMin >= AProjection.Width)
              or (yMin >= AProjection.Height)
              or (xMax < 0)
              or (yMax < 0));
end;

function TMapPainter.IsVisibleWay(const AProjection: TProjection;
  const ABoundingBox: TGeoBox; APixelOffset: TReal): Boolean;
var
  x, y, xMin, xMax, yMin, yMax: TReal;
begin
  Result := False;
  if (not ABoundingBox.Valid) then
    Exit;

  AProjection.GeoToPixel(ABoundingBox.MinCoord, x, y);

  xMin := x;
  xMax := x;
  yMin := y;
  yMax := y;

  AProjection.GeoToPixel(ABoundingBox.MaxCoord, x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  AProjection.GeoToPixel(GeoCoord(ABoundingBox.MinCoord.Lat, ABoundingBox.MaxCoord.Lon),
                         x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  AProjection.GeoToPixel(GeoCoord(ABoundingBox.MaxCoord.Lat, ABoundingBox.MinCoord.Lon),
                         x, y);

  xMin := min(xMin, x);
  xMax := max(xMax, x);
  yMin := min(yMin, y);
  yMax := max(yMax, y);

  xMin := xMin - APixelOffset;
  xMax := xMax + APixelOffset;
  yMin := yMin - APixelOffset;
  yMax := yMax + APixelOffset;

  Result := not ((xMin >= AProjection.Width)
              or (yMin >= AProjection.Height)
              or (xMax < 0)
              or (yMax < 0));
end;

procedure TMapPainter.Transform(const AProjection: TProjection;
  const AParameter: TMapParameter; const ACoord: TGeoPoint; var X, Y: TReal);
begin
  AProjection.GeoToPixel(ACoord, X, Y);
end;

function TMapPainter.GetProjectedWidth(const AProjection: TProjection;
  AMinPixel, AWidth: TReal): TReal;
begin
  Result := AWidth / AProjection.PixelSize;

  if (Result < AMinPixel) then
    Result := AMinPixel;
end;

function TMapPainter.GetProjectedWidth(const AProjection: TProjection;
  AWidth: TReal): TReal;
begin
  Result := AWidth / AProjection.PixelSize;
end;

procedure TMapPainter.AfterPreprocessing(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
begin
  // if not used
end;

procedure TMapPainter.AfterDrawing(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
begin
  // if not used
end;

function TMapPainter.GetProposedLabelWidth(const AParameter: TMapParameter;
  AAverageCharWidth: TReal; AObjectWidth: TReal; AStringLength: Integer): TReal;
begin
  // If there is just a few characters (less than LabelLineMinCharCount)
  // we should not wrap the words at all.
  if (AStringLength > AParameter.LabelLineMinCharCount) then
  begin
    if (AObjectWidth > 0) and (AParameter.LabelLineFitToArea) then
      Result := min(AObjectWidth, AParameter.LabelLineFitToWidth)
    else
      Result := AParameter.LabelLineFitToWidth;

    Result := min(Result, AParameter.LabelLineMaxCharCount * AAverageCharWidth);
    Result := max(Result, AParameter.LabelLineMinCharCount * AAverageCharWidth);
  end
  else
    Result := AParameter.LabelLineMaxCharCount * AAverageCharWidth;
end;

procedure TMapPainter.DrawWay(AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TWayData);
var
  Color: TMapColor;
  StartCap, EndCap: TLineCapStyle;
begin
  Color := AData.LineStyle.LineColor;

  if AData.IsStartClosed then
    StartCap := AData.LineStyle.EndCap
  else
    StartCap := AData.LineStyle.JoinCap;

  if AData.IsEndClosed then
    EndCap := AData.LineStyle.EndCap
  else
    EndCap := AData.LineStyle.JoinCap;

  if AData.LineStyle.HasDashes() and AData.LineStyle.GapColor.IsVisible() then
  begin
    // Draw the background of a dashed line
    DrawPath(AProjection,
             AParameter,
             AData.LineStyle.GapColor,
             AData.LineWidth,
             FEmptyDash,
             StartCap, EndCap,
             AData.TransStart, AData.TransEnd);
  end;

  DrawPath(AProjection,
           AParameter,
           Color,
           AData.LineWidth,
           AData.LineStyle.Dash,
           StartCap, EndCap,
           AData.TransStart, AData.TransEnd);
end;

constructor TMapPainter.Create(const AStyleConfig: TStyleConfig);
begin
  inherited Create();
  FTextStyles := TTextStyleList.Create();
  FLineStyles := TLineStyleList.Create();
  FBorderStyles := TBorderStyleList.Create();

  FStyleConfig := AStyleConfig;
  FTransBuffer.Init();
  FNameReader.Init(AStyleConfig.TypeConfig, ftName);
  FNameAltReader.Init(AStyleConfig.TypeConfig, ftNameAlt);
  FRefReader.Init(AStyleConfig.TypeConfig, ftRef);
  FLayerReader.Init(AStyleConfig.TypeConfig, ftLayer);
  FWidthReader.Init(AStyleConfig.TypeConfig, ftWidth);
  FAddressReader.Init(AStyleConfig.TypeConfig, ftAddress);
  FLanesReader.Init(AStyleConfig.TypeConfig, ftLanes);
  FAccessReader.Init(AStyleConfig.TypeConfig, ftAccess);

  //_LogDebug('TMapPainter.Create()');

  SetLength(FTunnelDash, 2);
  FTunnelDash[0] := 0.4;
  FTunnelDash[1] := 0.4;

  FAreaMarkStyle := TFillStyle.Create();
  FAreaMarkStyle.FillColor.Init(1.0, 0.0, 0.0, 0.5);

  FLandFill := TFillStyle.Create();
  FLandFill.FillColor.Init(241.0/255, 238.0/255, 233.0/255, 1);

  FSeaFill := TFillStyle.Create();
  FSeaFill.FillColor.Init(181.0/255, 208.0/255, 208.0/255, 1);

  FDebugLabel := TTextStyle.Create();
  FDebugLabel.Style := tssNormal;
  FDebugLabel.Priority := 0;
  FDebugLabel.TextColor.Init(0, 0, 0, 0.9);
  FDebugLabel.SizeMM := 0.7;

  {
  FStepMethods[rsInitialize] := @InitializeRender;
  FStepMethods[rsDumpStatistics] := @DumpDataStatistics;
  FStepMethods[rsPreprocessData] := @PreprocessData;
  FStepMethods[rsPrerender] := @Prerender;
  FStepMethods[rsDrawGroundTiles] := @DrawGroundTiles;
  FStepMethods[rsDrawOSMTileGrids] := @DrawOSMTileGrids;
  FStepMethods[rsDrawAreas] := @DrawAreas;
  FStepMethods[rsDrawWays] := @DrawWays;
  FStepMethods[rsDrawWayDecorations] := @DrawWayDecorations;
  FStepMethods[rsDrawWayContourLabels] := @DrawWayContourLabels;
  FStepMethods[rsPrepareAreaLabels] := @PrepareAreaLabels;
  FStepMethods[rsDrawAreaBorderLabels] := @DrawAreaBorderLabels;
  FStepMethods[rsDrawAreaBorderSymbols] := @DrawAreaBorderSymbols;
  FStepMethods[rsPrepareNodeLabels] := @PrepareNodeLabels;
  FStepMethods[rsDrawLabels] := @DrawLabels;
  FStepMethods[rsPostrender] := @Postrender;
  }

end;

destructor TMapPainter.Destroy;
begin
  //_LogDebug('TMapPainter.Destroy()');
  FreeAndNil(FDebugLabel);
  FreeAndNil(FSeaFill);
  FreeAndNil(FLandFill);

  FreeAndNil(FLineStyles);
  FreeAndNil(FTextStyles);
  FreeAndNil(FBorderStyles);
  inherited Destroy;
end;

function TMapPainter.GetStateStr(): string;
begin
  if FIsBusy then
  begin
    Result := 'Step: ' + RenderStepsNames[FCurStep];
    case FCurStep of
      rsDrawAreas,
      rsDrawWays,
      rsDrawWayDecorations,
      rsDrawWayContourLabels,
      rsPrepareAreaLabels,
      rsDrawAreaBorderLabels,
      rsDrawAreaBorderSymbols,
      rsPrepareNodeLabels,
      rsDrawLabels: Result := Result + '  Item: ' + IntToStr(FCurItemIndex);

      rsPreprocessData:
      begin
        Result := Result + '  Item (' + FCurItemDesc + '): ' + IntToStr(FCurItemIndex);
      end;
    end;

  end
  else
    Result := '';
end;

procedure TMapPainter.CheckDebug(AMapData: TMapData);
var
  TmpArea: TMapArea;
  i: Integer;
begin
  // debug
  for i := 0 to AMapData.AreaList.Count-1 do
  begin
    TmpArea := AMapData.AreaList[i];
    if Length(TmpArea.Rings) = 0 then
    begin
      Sleep(1);
    end;
  end;
end;

function TMapPainter.Draw(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData;
  AStartStep: TRenderSteps; AEndStep: TRenderSteps): Boolean;
var
  Step: TRenderSteps;
begin
  Result := False;
  Assert(AStartStep >= Low(TRenderSteps));
  Assert(AStartStep <= High(TRenderSteps));
  FIsBusy := True;

  CheckDebug(AData);

  for Step := AStartStep to AEndStep do
  begin
    FCurStep := Step;
    //Assert(FStepMethods[Step] <> nil);

    //FStepMethods[Step](AProjection, AParameter, AData);

    case Step of
      rsInitialize: InitializeRender(AProjection, AParameter, AData);
      rsDumpStatistics: DumpDataStatistics(AProjection, AParameter, AData);
      rsPreprocessData: PreprocessData(AProjection, AParameter, AData);
      rsPrerender: Prerender(AProjection, AParameter, AData);
      rsDrawGroundTiles: DrawGroundTiles(AProjection, AParameter, AData);
      //rsDrawOSMTileGrids: DrawOSMTileGrids(AProjection, AParameter, AData);
      rsDrawAreas: DrawAreas(AProjection, AParameter, AData);
      rsDrawWays: DrawWays(AProjection, AParameter, AData);
      rsDrawWayDecorations: DrawWayDecorations(AProjection, AParameter, AData);
      rsDrawWayContourLabels: DrawWayContourLabels(AProjection, AParameter, AData);
      rsPrepareAreaLabels: PrepareAreaLabels(AProjection, AParameter, AData);
      rsDrawAreaBorderLabels: DrawAreaBorderLabels(AProjection, AParameter, AData);
      rsDrawAreaBorderSymbols: DrawAreaBorderSymbols(AProjection, AParameter, AData);
      rsPrepareNodeLabels: PrepareNodeLabels(AProjection, AParameter, AData);
      rsDrawLabels: DrawLabels(AProjection, AParameter, AData);
      rsPostrender: Postrender(AProjection, AParameter, AData);
    end;

    if AParameter.IsAborted then
    begin
      FIsBusy := False;
      Exit;
    end;
  end;

  FIsBusy := False;
  Result := True;
end;

function TMapPainter.DrawMap(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData): Boolean;
begin
  Result := Draw(AProjection, AParameter, AData, Low(TRenderSteps), High(TRenderSteps));
end;

{ TAreaDataList }

function TAreaDataList.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TAreaDataList.SetCapacity(AValue: Integer);
var
  i: Integer;
begin
  // removed positions
  i := FCount-1;
  while i >= AValue do
  begin
    Dispose(FItems[i]);
    Dec(i);
  end;

  SetLength(FItems, AValue);

  // added positions
  i := FCount;
  while i < AValue do
  begin
    New(FItems[i]);
    Inc(i);
  end;

  if FCount > AValue then
    FCount := AValue;
end;

function TAreaDataList.GetPItem(AIndex: Integer): PAreaData;
begin
  Result := FItems[AIndex];
end;

procedure TAreaDataList.Clear();
begin
  FCount := 0;
end;

function TAreaDataList.Append(const AItem: TAreaData): Integer;
begin
  Result := FCount;
  if FCount >= Capacity then
  begin
    //SetLength(FItems, (FCount * 3 div 2) + 1);
    SetCapacity((FCount * 3 div 2) + 1);
  end;
  FItems[FCount]^ := AItem;
  Inc(FCount);
end;

{function CompareAreas(const Area1, Area2): Integer;
begin
  Result := CompareAreaData(PAreaData(Area1)^, PAreaData(Area2)^);
end; }

procedure TAreaDataList.QSort(L, R: Integer);
var
  I, J : Longint;
  P, Q : PAreaData;
begin
 repeat
   I := L;
   J := R;
   P := FItems[ (L + R) div 2 ];
   repeat
     while CompareAreaData(P^, FItems[i]^) > 0 do
       I := I + 1;
     while CompareAreaData(P^, FItems[J]^) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FItems[I];
       FItems[I] := FItems[J];
       FItems[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QSort(L, J);
     L := I;
   end
   else
   begin
     if I < R then
       QSort(I, R);
     R := J;
   end;
 until L >= R;
end;

procedure TAreaDataList.Sort(ASorter: TAreaDataCompare);
begin
  //Assert(False, 'not implemented');
  { TODO : implement }
  //TList.Sort();
  //AnySort(FItems, FCount, SizeOf(PAreaData), @CompareAreas);
 if FCount > 1 then
   QSort(0, FCount-1);
end;

{ TWayPathDataList }

function TWayPathDataList.GetCapacity: Integer;
begin
  Result := Length(Items);
end;

procedure TWayPathDataList.SetCapacity(AValue: Integer);
begin
  SetLength(Items, AValue);
  if FCount > AValue then
    FCount := AValue;
end;

procedure TWayPathDataList.Clear();
begin
  FCount := 0;
end;

function TWayPathDataList.Append(const AItem: TWayPathData): Integer;
begin
  Result := FCount;
  if FCount >= Capacity then
  begin
    SetLength(Items, (FCount * 3 div 2) + 1);
  end;
  Items[FCount] := AItem;
  Inc(FCount);
end;

{ TWayDataList }

function TWayDataList.GetCapacity: Integer;
begin
  Result := Length(Items);
end;

procedure TWayDataList.SetCapacity(AValue: Integer);
begin
  SetLength(Items, AValue);
  if FCount > AValue then
    FCount := AValue;
end;

procedure TWayDataList.Clear();
begin
  FCount := 0;
end;

function TWayDataList.Append(const AItem: TWayData): Integer;
begin
  Result := FCount;
  if FCount >= Capacity then
  begin
    SetLength(Items, (FCount * 3 div 2) + 1);
  end;
  Items[FCount] := AItem;
  Inc(FCount);
end;

procedure TWayDataList.Sort(ASorter: TWayDataCompare);
begin
  //Assert(False, 'not implemented');
  { TODO : implement }
end;

{ TMapPainterBatch }

function TMapPainterBatch.BatchPaintInternal(const AProjection: TProjection;
  const AParameter: TMapParameter): Boolean;
var
  Step: TRenderSteps;
  i: Integer;
  Painter: TMapPainter;
begin
  Result := True;
  for Step := Low(Step) to High(Step) do
  begin
    for i := 0 to FData.Count-1 do
    begin
      Painter := FPainters.Items[i];
      Result := Result and Painter.Draw(AProjection, AParameter, FData[i], Step, Step);
    end;
  end;
end;

constructor TMapPainterBatch.Create;
begin
  inherited Create();
  FData := TMapDataList.Create();
  FPainters := TMapPainterList.Create();
end;

destructor TMapPainterBatch.Destroy;
begin
  FreeAndNil(FPainters);
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TMapPainterBatch.AddData(var AData: TMapData; APainter: TMapPainter);
begin
  FData.Add(AData);
  FPainters.Add(APainter);
end;

{ TMapPainterNoOp }

function TMapPainterNoOp.HasIcon(AStyleConfig: TStyleConfig;
  AProjection: TProjection; AParameter: TMapParameter; AStyle: TIconStyle): Boolean;
begin
  Result := False;
end;

function TMapPainterNoOp.GetFontHeight(const AProjection: TProjection;
  const AParameter: TMapParameter; AFontSize: TReal): TReal;
const
  // Height of the font in pixel in relation to the given fontSize
  FONT_HEIGHT_FACTOR = 10.0;
begin
  Result := FONT_HEIGHT_FACTOR * AFontSize;
end;

procedure TMapPainterNoOp.DrawGround(const AProjection: TProjection;
  const AParameter: TMapParameter; const AStyle: TFillStyle);
begin
  // no code
end;

procedure TMapPainterNoOp.RegisterRegularLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabels: TLabelDataList;
  const APosition: TVertex2D; AObjectWidth: TReal);
begin
  // no code
end;

procedure TMapPainterNoOp.RegisterContourLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabel: TPathLabelData;
  const ALabelPath: TLabelPath);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawIcon(AStyle: TIconStyle; ACenterX,
  ACenterY: TReal; AWidth, Aheight: TReal);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: TReal);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawPath(const AProjection: TProjection;
  const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
  const ADash: array of TReal; AStartCap: TLineCapStyle;
  AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawContourSymbol(const AProjection: TProjection;
  const AParameter: TMapParameter; const ASymbol: TMapSymbol; ASpace: TReal;
  ATransStart, ATransEnd: Integer);
begin
  // no code
end;

procedure TMapPainterNoOp.DrawArea(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TAreaData);
begin
  // no code
end;

constructor TMapPainterNoOp.Create(const AStyleConfig: TStyleConfig);
begin
  inherited Create(AStyleConfig);
end;

function TMapPainterNoOp.DrawMap(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData): Boolean;
begin
  Result := inherited DrawMap(AProjection, AParameter, AData);
end;


end.

