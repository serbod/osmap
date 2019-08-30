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
  OsMap parameters
MapParameter:
  MapParameter ...
*)
unit OsMapParameters;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, OsMapGeometry, OsMapTransform, OsMapUtils;

type
  TMapIconMode = (
    imFixedSizePixmap,  // raster icons should be used, iconPixelSize will be used for rendering
    imScaledPixmap,     // raster icons should be used, icons will be scaled to iconSize
    imOriginalPixmap,   // raster icons should be used, icons will keep dimensions of original image
    imScalable          // vector icons should be used, icons will be scaled to iconSize
  );

  TMapPatternMode = (
    pmOriginalPixmap,   // raster pattern should be used, it will keep dimensions of original image
    pmScalable          // vector pattern should be used, it will be scaled to patternSize
  );

  { Collection of Parameter that parametrize and influence drawing of the map. }

  { TMapParameter }

  TMapParameter = class
  private
    FFontName: string;                  // Name of the font to use
    FFontSize: Double;                  // Metric size of base font (aka font size 100%) in millimeter

    FIconPaths: array of string;        // List of paths to search for images for icons
    FPatternPaths: array of string;     // List of paths to search for images for patterns

    FLineMinWidthPixel: Double;         // Minimum width of an line to be drawn
    FAreaMinDimensionMM: Double;        // Minimum dimension (either width or height) of an area in mm

    FOptimizeWayNodes: TTransOptimizeMethod;  // Try to reduce the number of nodes for
    FOptimizeAreaNodes: TTransOptimizeMethod; // Try to reduce the number of nodes for
    FOptimizeErrorToleranceMm: Double;  // The maximum error to allow when optimizing lines, in mm
    FDrawFadings: Boolean;              // Draw label fadings (default: true)
    FDrawWaysWithFixedWidth: Boolean;   // Draw ways using the size of the style sheet, if if the way has a width explicitly given
    FDrawWaysBorder: Boolean;           // Draw border for ways

    // Node and area labels, icons
    FLabelLineMinCharCount: Integer;    // Labels will be _never_ word wrapped if they are shorter then the given characters
    FLabelLineMaxCharCount: Integer;    // Labels will be word wrapped if they are longer then the given characters
    FLabelLineFitToArea: Boolean;       // Labels will be word wrapped to fit object area
    FLabelLineFitToWidth: Double;       // Labels will be word wrapped to fit given width in pixels
    FLabelIncludesAddress: Boolean;

    FLabelPadding: Double;              // Space around point labels in mm (default 1).
    FPlateLabelPadding: Double;         // Space around plates in mm (default 5).
    FOverlayLabelPadding: Double;       // Space around overlay labels in mm (default 6).

    FIconMode: TMapIconMode;            // Mode of icons, it controls what type of files would be loaded and how icon dimensions will be calculated
    FIconSize: Double;                  // Size of icons in mm (default 3.7)
    FIconPixelSize: Double;             // Size of icons in px (default 14)
    FIconPadding: Double;               // Space around icons and symbols in mm (default 1).

    FPatternMode: TMapPatternMode;      // Mode of pattern, it controls what type of files would be loaded and how pattern geometry will be canculated
    FPatternSize: Double;               // Size of pattern image in mm (default 3.7)

    FDropNotVisiblePointLabels: Boolean; // Point labels that are not visible, are clipped during label positioning phase

  private
    // Contour labels
    FContourLabelOffset: Double;        // Offset in mm for beginning and end of an contour label in relation to contour begin and end
    FContourLabelSpace: Double;         // Space in mm between repetitive labels on the same contour
    FContourLabelPadding: Double;       // Space around contour labels in mm (default 1).

    FRenderBackground: Boolean;         // Render any background features, else render like the background should be transparent
    FRenderSeaLand: Boolean;            // Rendering of sea/land tiles
    FRenderUnknowns: Boolean;           // Unknown areas are not rendered (transparent)

    FDebugData: Boolean;                // Print out some performance relvant information about the data
    FDebugPerformance: Boolean;         // Print out some performance information

    FWarnObjectCountLimit: Integer;     // Limit for objects/type. If limit is reached a warning is created
    FWarnCoordCountLimit: Integer;      // Limit for coords/type. If limit is reached a warning is created

    FShowAltLanguage: Boolean;          // if true, display alternative language (needs support by style sheet and import)

    // -> TStyleConfig
    //std::vector<FillStyleProcessorRef > fillProcessors;            //!< List of processors for FillStyles for types

    FBreaker: ^TBreakerObject;          //!< Breaker to abort processing on external request

    function GetIsAborted: Boolean;
  public
    // -> TStyleConfig
    //procedure RegisterFillStyleProcessor();
    //function GetFillStyleProcessor(ATypeIndex: Integer): TFillStyleProcessor;
    procedure AfterConstruction; override;

    procedure ResetToDefaults();

    // Name of the font to use
    property FontName: string read FFontName write FFontName;
    // Metric size of base font (aka font size 100%) in millimeter
    property FontSize: Double read FFontSize write FFontSize;

    // Minimum width of an line to be drawn, default 0.2
    property LineMinWidthPixel: Double read FLineMinWidthPixel write FLineMinWidthPixel;
    // Minimum dimension (either width or height) of an area in mm, default 2.0
    property AreaMinDimensionMM: Double read FAreaMinDimensionMM write FAreaMinDimensionMM;

    // Try to reduce the number of nodes for nodes, default: tomNone
    property OptimizeWayNodes: TTransOptimizeMethod read FOptimizeWayNodes write FOptimizeWayNodes;
    // Try to reduce the number of nodes for areas, default: tomNone
    property OptimizeAreaNodes: TTransOptimizeMethod read FOptimizeAreaNodes write FOptimizeAreaNodes;
    // The maximum error to allow when optimizing lines, in mm
    property IsOptimizeErrorToleranceMm: Double read FOptimizeErrorToleranceMm;
    // Draw label fadings (default: true)
    property IsDrawFadings: Boolean read FDrawFadings;
    // Draw ways using the size of the style sheet, if if the way has a width explicitly given
    property IsDrawWaysWithFixedWidth: Boolean read FDrawWaysWithFixedWidth;
    // Draw border for ways
    property IsDrawWaysBorder: Boolean read FDrawWaysBorder write FDrawWaysBorder;

    // Node and area labels, icons

    // Labels will be _never_ word wrapped if they are shorter then the given characters
    property LabelLineMinCharCount: Integer read FLabelLineMinCharCount;
    // Labels will be word wrapped if they are longer then the given characters
    property LabelLineMaxCharCount: Integer read FLabelLineMaxCharCount;
    // Labels will be word wrapped to fit object area
    property LabelLineFitToArea: Boolean read FLabelLineFitToArea;
    // Labels will be word wrapped to fit given width in pixels
    property LabelLineFitToWidth: Double read FLabelLineFitToWidth;
    // Label includes address
    property LabelIncludesAddress: Boolean read FLabelIncludesAddress write FLabelIncludesAddress;


    // Space around point labels in mm (default 1).
    property LabelPadding: Double read FLabelPadding;
    // Space around plates in mm (default 5).
    property PlateLabelPadding: Double read FPlateLabelPadding;
    // Space around overlay labels in mm (default 6).
    property OverlayLabelPadding: Double read FOverlayLabelPadding;

    // Mode of icons, it controls what type of files would be loaded and how icon dimensions will be calculated
    property IconMode: TMapIconMode read FIconMode;
    // Size of icons in mm (default 3.7)
    property IconSize: Double read FIconSize;
    // Size of icons in px (default 14)
    property IconPixelSize: Double read FIconPixelSize;
    // Space around icons and symbols in mm (default 1).
    property IconPadding: Double read FIconPadding;

    // Mode of pattern, it controls what type of files would be loaded and how pattern geometry will be canculated
    property PatternMode: TMapPatternMode read FPatternMode;
    // Size of pattern image in mm (default 3.7)
    property PatternSize: Double read FPatternSize;

    // Point labels that are not visible, are clipped during label positioning phase
    property IsDropNotVisiblePointLabels: Boolean read FDropNotVisiblePointLabels;

    // Offset in mm for beginning and end of an contour label in relation to contour begin and end
    property ContourLabelOffset: Double read FContourLabelOffset;
    // Space in mm between repetitive labels on the same contour
    property ContourLabelSpace: Double read FContourLabelSpace;
    // Space around contour labels in mm (default 1).
    property ContourLabelPadding: Double read FContourLabelPadding;

    // Render any background features, else render like the background should be transparent
    property IsRenderBackground: Boolean read FRenderBackground;
    // Rendering of sea/land tiles
    property IsRenderSeaLand: Boolean read FRenderSeaLand;
    // Unknown areas are not rendered (transparent)
    property IsRenderUnknowns: Boolean read FRenderUnknowns;

    // Print out some performance relvant information about the data
    property IsDebugData: Boolean read FDebugData write FDebugData;
    // Print out some performance information
    property IsDebugPerformance: Boolean read FDebugPerformance write FDebugPerformance;

    // Limit for objects/type. If limit is reached a warning is created
    property WarningObjectCountLimit: Integer read FWarnObjectCountLimit;
    // Limit for coords/type. If limit is reached a warning is created
    property WarningCoordCountLimit: Integer read FWarnCoordCountLimit;

    // if true, display alternative language (needs support by style sheet and import)
    property IsShowAltLanguage: Boolean read FShowAltLanguage;

    property IsAborted: Boolean read GetIsAborted;
  end;

implementation

{ TMapParameter }

function TMapParameter.GetIsAborted: Boolean;
begin
  if Assigned(FBreaker) then
    Result := FBreaker^.IsAborted
  else
    Result := False;
end;

procedure TMapParameter.AfterConstruction;
begin
  inherited AfterConstruction;
  ResetToDefaults();
end;

procedure TMapParameter.ResetToDefaults();
begin
  FFontName := 'Tahoma';              // Name of the font to use
  FFontSize := 8;                     // Metric size of base font (aka font size 100%) in millimeter

  FLineMinWidthPixel := 0.2;          // Minimum width of an line to be drawn
  FAreaMinDimensionMM := 2.0;         // Minimum dimension (either width or height) of an area in mm

  FOptimizeWayNodes := tomNone;       // Try to reduce the number of nodes for ways
  FOptimizeAreaNodes := tomNone;      // Try to reduce the number of nodes for areas
  FOptimizeErrorToleranceMm := 0.5;   // The maximum error to allow when optimizing lines, in mm
  FDrawFadings := True;               // Draw label fadings (default: true)
  FDrawWaysWithFixedWidth := False;   // Draw ways using the size of the style sheet, if if the way has a width explicitly given
  FDrawWaysBorder := True;

  // Node and area labels, icons
  FLabelLineMinCharCount := 15;       // Labels will be _never_ word wrapped if they are shorter then the given characters
  FLabelLineMaxCharCount := 1000;     // Labels will be word wrapped if they are longer then the given characters
  FLabelLineFitToArea := True;        // Labels will be word wrapped to fit object area
  FLabelLineFitToWidth := 8000;       // Labels will be word wrapped to fit given width in pixels

  FLabelPadding := 1.0;               // Space around point labels in mm (default 1).
  FPlateLabelPadding := 5.0;          // Space around plates in mm (default 5).
  FOverlayLabelPadding := 6.0;        // Space around overlay labels in mm (default 6).

  FIconMode := imFixedSizePixmap;     // Mode of icons, it controls what type of files would be loaded and how icon dimensions will be calculated
  FIconSize := 3.7;                   // Size of icons in mm (default 3.7)
  FIconPixelSize := 14;               // Size of icons in px (default 14)
  FIconPadding := 1;                  // Space around icons and symbols in mm (default 1).

  FPatternMode := pmOriginalPixmap;   // Mode of pattern, it controls what type of files would be loaded and how pattern geometry will be canculated
  FPatternSize := 3.7;                // Size of pattern image in mm (default 3.7)

  FDropNotVisiblePointLabels := True; // Point labels that are not visible, are clipped during label positioning phase

  // Contour labels
  FContourLabelOffset := 5.0;         // Offset in mm for beginning and end of an contour label in relation to contour begin and end
  FContourLabelSpace := 30.0;         // Space in mm between repetitive labels on the same contour
  FContourLabelPadding := 1.0;        // Space around contour labels in mm (default 1).

  FRenderBackground := True;          // Render any background features, else render like the background should be transparent
  FRenderSeaLand := False;            // Rendering of sea/land tiles
  FRenderUnknowns := False;           // Unknown areas are not rendered (transparent)

  FDebugData := False;                // Print out some performance relvant information about the data
  FDebugPerformance := False;         // Print out some performance information

  FWarnObjectCountLimit := 0;         // Limit for objects/type. If limit is reached a warning is created
  FWarnCoordCountLimit := 0;          // Limit for coords/type. If limit is reached a warning is created

  FShowAltLanguage := False;          // if true, display alternative language (needs support by style sheet and import)
end;

end.

