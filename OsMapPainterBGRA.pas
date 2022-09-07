unit OsMapPainterBGRA;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Types, Classes, SysUtils, Graphics,
  OsMapPainter, OsMapLabels, OsMapParameters, OsMapStyles,
  OsMapTypes, OsMapTransform, OsMapGeometry, OsMapStyleConfig, OsMapProjection,
  OsMapObjects,
  BGRABitmap, BGRABitmapTypes;

type
  //TAggPixelFormat = TAggPixelFormat;

  TNativeGlyph = record
    X: TReal;
    Y: TReal;
    //AggGlyph: TAggGlyphCache;
  end;

  TNativeGlyphArray = array of TNativeGlyph;

  {TNativeLabel = record
    Text: string;
    Glyphs: TNativeGlyphArray;
  end; }

  { TMapPainterBGRA }

  TMapPainterBGRA = class(TMapPainter)
  private
    FBitmap: TBGRABitmap;
    FLabelLayouter: TLabelLayouter;
    FMutex: TRTLCriticalSection;

    FCanvas: TCanvas;
    FFontSize: TReal;
    FIsAlienBitmap: Boolean;
    FIsDrawingWayBorder: Boolean;

    function GetBGRAPixel(const AColor: TMapColor): TBGRAPixel;
    procedure SetBitmap(AValue: TBGRABitmap);

    procedure SetFont(AProjection: TProjection; AParameter: TMapParameter;
      ASize: TReal); // agg::glyph_rendering ren_type = agg::glyph_ren_native_gray8

    procedure GetTextDimension(const AText: string; out AWidth, AHeight: TReal);

    procedure DrawText(X, Y: TReal; const AText: string);

    //procedure DrawGlyph(X, Y: TReal; const AGlyph: TAggGlyphCache);

    procedure DrawGlyphVector(X, ABaselineY: TReal; const AGlyphs: TNativeGlyphArray);

    procedure DrawGlyphs(AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TPathTextStyle;
      const AGlyphs: TMapGlyphArray);

    function GetBorderColor(ABorderStyle: TBorderStyle): TBGRAPixel;
    function GetFillColor(AFillStyle: TFillStyle): TBGRAPixel;

    { ex-DrawFill(), set fill and border style }
    procedure DrawFillStyle(AProjection: TProjection; AParameter: TMapParameter;
      AFillStyle: TFillStyle; ABorderStyle: TBorderStyle);

  protected
    function HasIcon(AStyleConfig: TStyleConfig; AProjection: TProjection;
      AParameter: TMapParameter; AStyle: TIconStyle): Boolean; override;

    function GetFontHeight(const AProjection: TProjection;
      const AParameter: TMapParameter; AFontSize: TReal): TReal; override;

    procedure DrawGround(const AProjection: TProjection;
      const AParameter: TMapParameter; const AStyle: TFillStyle); override;

    procedure DrawIcon(AStyle: TIconStyle; ACenterX, ACenterY: TReal;
      AWidth, Aheight: TReal); override;

    procedure DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: TReal); override;

    procedure DrawLabel(AProjection: TProjection;
      AParameter: TMapParameter;
      X, Y: TReal;
      const AMapLabel: TMapLabel;
      const ALabel: TLabelData);

    procedure DrawPath(const AProjection: TProjection;
      const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
      const ADash: array of TReal; AStartCap: TLineCapStyle;
      AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer); override;

    { Register regular label with given text at the given pixel coordinate
      in a style defined by the given LabelStyle. }
    procedure RegisterRegularLabel(const AProjection: TProjection;
      const AParameter: TMapParameter; const ALabels: TLabelDataList;
      const APosition: TVertex2D; AObjectWidth: TReal); override;

    { Register contour label }
    procedure RegisterContourLabel(const AProjection: TProjection;
      const AParameter: TMapParameter; const ALabel: TPathLabelData;
      const ALabelPath: TLabelPath); override;

    procedure DrawLabels(const AProjection: TProjection;
      const AParameter: TMapParameter; const AData: TMapData); override;

    procedure BeforeDrawing(const AStyleConfig: TStyleConfig;
      const AProjection: TProjection; const AParameter: TMapParameter;
      const AData: TMapData); override;

    procedure DrawContourSymbol(const AProjection: TProjection;
      const AParameter: TMapParameter; const ASymbol: TMapSymbol;
      ASpace: TReal; ATransStart, ATransEnd: Integer); override;

    procedure DrawArea(const AProjection: TProjection;
      const AParameter: TMapParameter; const AAreaData: TAreaData); override;

  protected
    { draw native label }
    {procedure DrawLabelNative(AProjection: TProjection;
      AParameter: TMapParameter;
      const ALabelRectangle: TDoubleRectangle;
      const ALabel: TLabelData;
      const ALayout: TNativeLabel); }

    { draw native glyphs }
    {procedure DrawGlyphsNative(AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TPathTextStyle;
      const AGlyphs: TNativeGlyphArray); }

    { glyph bounding box relative to its base point }
    //procedure OnGlyphBoundingBoxHandler(const AGlyph: TMapGlyph; out ARect: TDoubleRectangle);

    { layout text for label }
    procedure OnTextLayoutHandler(out ALabel: TMapLabel;
      AProjection: TProjection;
      AParameter: TMapParameter;
      const AText: string;
      AFontSize, AObjectWidth: TReal;
      AEnableWrapping: Boolean = False;
      AContourLabel: Boolean = False);
  public
    constructor Create(const AStyleConfig: TStyleConfig; ACanvas: TCanvas);
    destructor Destroy; override;

    function DrawMap(AProjection: TProjection;
      AParameter: TMapParameter;
      AData: TMapData): Boolean; override;

    //property Agg2D: TAgg2D read FAgg2D write SetAgg2d;
    property Bitmap: TBGRABitmap read FBitmap;
  end;

implementation

uses Math, LazUTF8;

{ TMapPainterBGRA }

{function TMapPainterBGRA.GetAggRgba8(const AColor: TMapColor): TAggRgba8;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgbaDouble(AColor.R, AColor.G, AColor.B, AColor.A);
  Result := AggColor.Rgba8;
end; }

function TMapPainterBGRA.GetBGRAPixel(const AColor: TMapColor): TBGRAPixel;
var
  bR, bG, bB, bA: Byte;
begin
  AColor.ToBytes(bR, bG, bB, bA);
  Result.FromRGB(bR, bG, bB, bA);
end;

procedure TMapPainterBGRA.SetBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then Exit;
  if not FIsAlienBitmap then
  begin
    FreeAndNil(FBitmap);
    FIsAlienBitmap := True;
  end;
  FBitmap := AValue;
end;

procedure TMapPainterBGRA.SetFont(AProjection: TProjection;
  AParameter: TMapParameter; ASize: TReal);
begin
  try
    Bitmap.FontName := AParameter.FontName;
    Bitmap.FontHeight := Round(ASize);
    Bitmap.FontAntialias := True;

    FFontSize := ASize;
  except
    WriteLn('Cannot load font: ', AParameter.FontName);
  end;
end;

procedure TMapPainterBGRA.GetTextDimension(const AText: string; out AWidth,
  AHeight: TReal);
var
  SizeRec: TSize;
begin
  SizeRec := Bitmap.TextSize(AText);
  AWidth := SizeRec.cx;
  AHeight := SizeRec.cy;
end;

procedure TMapPainterBGRA.DrawText(X, Y: TReal; const AText: string);
var
  ws: WideString;
begin
  //s := UTF8ToWinCP(AText);
  //s := UTF8ToSys(AText);
  //s := UTF8ToConsole(AText);
  //ws := UTF8ToUTF16(AText);
  Bitmap.TextOut(X, Y, AText, nil, False);
end;

{procedure TMapPainterBGRA.DrawGlyph(X, Y: TReal; const AGlyph: TAggGlyphCache);
begin
  //
end; }

procedure TMapPainterBGRA.DrawGlyphVector(X, ABaselineY: TReal;
  const AGlyphs: TNativeGlyphArray);
begin
  //
end;

procedure TMapPainterBGRA.DrawGlyphs(AProjection: TProjection;
  AParameter: TMapParameter;
  AStyle: TPathTextStyle;
  const AGlyphs: TMapGlyphArray);
var
  //matrix: TAggTransAffine;
  layoutGlyph: TMapGlyph;
  //i: Integer;
  TextColor: TMapColor;
  cPen: TBGRAPixel;
  Angle: Integer;
begin
  Assert(Assigned(AStyle));
  TextColor.Init(0, 0, 0, 1);
  cPen := GetBGRAPixel(TextColor);
  //FAgg2D.FillColor := GetAggRgba8(AStyle.TextColor);
  //Bitmap.FillColor := GetAggRgba8(TextColor);
  //FAgg2D.NoLine();
  //FAgg2D.FlipText := True;
  //FAgg2D.LineColor := GetAggRgba8(AStyle.TextColor);

  //matrix := TAggTransAffine.Create();

  //for i := 0 to Length(AGlyphs)-1 do
  for layoutGlyph in AGlyphs do
  begin
    //layoutGlyph := AGlyphs[i];
    // contour labels should always use outline rendering
    //Assert(TNativeGlyph(layoutGlyph.Glyph).AggGlyph.DataType = gdOutline);
    if Bitmap.FontHeight <> Round(layoutGlyph.Height) then
      Bitmap.FontHeight := Round(layoutGlyph.Height);

    Angle := -Round(radtodeg(layoutGlyph.Angle) * 10);
    Bitmap.TextOutAngle(
      layoutGlyph.Position.X,
      layoutGlyph.Position.Y - layoutGlyph.Height,
      Angle,
      layoutGlyph.TextChar,
      cPen,
      taLeftJustify
      );
  end;
end;

function TMapPainterBGRA.GetBorderColor(ABorderStyle: TBorderStyle): TBGRAPixel;
begin
  if Assigned(ABorderStyle) and ABorderStyle.Color.IsVisible then
  begin
    Result := GetBGRAPixel(ABorderStyle.Color);
  end
  else
    Result.FromRGB(0,0,0,0);
end;

function TMapPainterBGRA.GetFillColor(AFillStyle: TFillStyle): TBGRAPixel;
begin
  if Assigned(AFillStyle) and AFillStyle.FillColor.IsVisible then
  begin
    Result := GetBGRAPixel(AFillStyle.FillColor);
  end
  else
    Result.FromRGB(0,0,0,0);
end;

procedure TMapPainterBGRA.DrawFillStyle(AProjection: TProjection;
  AParameter: TMapParameter; AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
begin

end;

{procedure TMapPainterBGRA.DrawFillStyle(AProjection: TProjection;
  AParameter: TMapParameter;
  AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
var
  borderWidth: TReal;
  i: Integer;
  LineWidth: Single;
  cPen, cFill: TBGRAPixel;
begin
  if Assigned(AFillStyle) and AFillStyle.FillColor.IsVisible then
  begin
    cFill := GetBGRAPixel(AFillStyle.FillColor);
  end
  else
    cFill.FromRGB(0,0,0,0);

  //Bitmap.PenStyle := psClear;

  //FAgg2D.RemoveAllDashes();

  if Assigned(ABorderStyle) then
  begin
    borderWidth := AProjection.ConvertWidthToPixel(ABorderStyle.Width);

    if (borderWidth >= AParameter.LineMinWidthPixel) then
    begin
      //Bitmap.Pen. Color := GetAggRgba8(ABorderStyle.Color);

      if Length(ABorderStyle.Dash) = 0 then
      begin
        LineWidth := borderWidth;
        FAgg2D.LineCap := lcRound;
      end
      else
      begin
        LineWidth := borderWidth;
        FAgg2D.LineCap := lcButt;
        i := 0;
        while i < Length(ABorderStyle.Dash)-1 do
        begin
          FAgg2D.AddDash(ABorderStyle.Dash[i] * borderWidth,
                         ABorderStyle.Dash[i+1] * borderWidth);
          Inc(i, 2);
        end;
      end;
    end;
  end;
end; }

function TMapPainterBGRA.HasIcon(AStyleConfig: TStyleConfig;
  AProjection: TProjection; AParameter: TMapParameter; AStyle: TIconStyle): Boolean;
begin
  Result := False;
end;

function TMapPainterBGRA.GetFontHeight(const AProjection: TProjection;
  const AParameter: TMapParameter; AFontSize: TReal): TReal;
begin
  if (FFontSize <> 0) then
    Result := Bitmap.FontHeight / FFontSize * AFontSize
  else
    Result := Bitmap.FontHeight;
end;

procedure TMapPainterBGRA.DrawGround(const AProjection: TProjection;
  const AParameter: TMapParameter; const AStyle: TFillStyle);
begin
  Bitmap.Rectangle(0, 0, AProjection.Width, AProjection.Height, GetBGRAPixel(AStyle.FillColor));
end;

procedure TMapPainterBGRA.DrawIcon(AStyle: TIconStyle; ACenterX,
  ACenterY: TReal; AWidth, Aheight: TReal);
begin
  //
end;

procedure TMapPainterBGRA.DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: TReal);
var
  minX, minY, maxX, maxY, centerX, centerY: TReal;
  Primitive: TDrawPrimitive;
  Polygon: TPolygonPrimitive;
  TmpRectangle: TRectanglePrimitive;
  TmpCircle: TCirclePrimitive;
  fillStyle: TFillStyle;
  borderStyle: TBorderStyle;
  i: Integer;
  Pixel: TVertex2D;
  xPos, yPos, width, height, radius: TReal;

  points: array of TPointF;
  cPen, cFill: TBGRAPixel;
  PenWidth: Single;
begin
  ASymbol.GetBoundingBox(minX, minY, maxX, maxY);

  centerX := (minX + maxX) / 2;
  centerY := (minY + maxY) / 2;

  for Primitive in ASymbol.Primitives do
  begin
    if (Primitive is TPolygonPrimitive) then
    begin
      Polygon := (Primitive as TPolygonPrimitive);
      fillStyle := polygon.FillStyle;
      borderStyle := polygon.BorderStyle;

      SetLength(points, Length(Polygon.Coords));
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);
      cPen :=  GetBorderColor(borderStyle);
      cFill := GetFillColor(fillStyle);
      PenWidth := AProjection.MillimetersToPixels(borderStyle.WidthMM);

      for i := 0 to Length(Polygon.Coords)-1 do
      begin
        Pixel := Polygon.Coords[i];
        points[i].x := X + AProjection.MillimetersToPixels(Pixel.X - centerX);
        Points[i].y := Y + AProjection.MillimetersToPixels(Pixel.Y - centerY);
      end;
      Bitmap.DrawPolygonAntialias(points, cPen, PenWidth, cFill);
    end
    else if (Primitive is TRectanglePrimitive) then
    begin
      TmpRectangle := (Primitive as TRectanglePrimitive);
      fillStyle := TmpRectangle.FillStyle;
      borderStyle := TmpRectangle.BorderStyle;
      xPos := x + AProjection.MillimetersToPixels(TmpRectangle.TopLeft.X - centerX);
      yPos := y + AProjection.MillimetersToPixels(TmpRectangle.TopLeft.Y - centerY);
      width := AProjection.MillimetersToPixels(TmpRectangle.Width);
      height := AProjection.MillimetersToPixels(TmpRectangle.Height);

      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);
      cPen :=  GetBorderColor(borderStyle);
      cFill := GetFillColor(fillStyle);
      PenWidth := AProjection.MillimetersToPixels(borderStyle.WidthMM);

      Bitmap.RectangleAntialias(xPos, yPos, xPos + width, yPos + height, cPen, PenWidth, cFill);
    end
    else if (Primitive is TCirclePrimitive) then
    begin
      TmpCircle := (Primitive as TCirclePrimitive);
      fillStyle := TmpCircle.FillStyle;
      borderStyle := TmpCircle.BorderStyle;
      radius := AProjection.MillimetersToPixels(TmpCircle.Radius);

      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);
      cPen :=  GetBorderColor(borderStyle);
      cFill := GetFillColor(fillStyle);
      PenWidth := AProjection.MillimetersToPixels(borderStyle.WidthMM);

      Bitmap.EllipseAntialias(
        X + AProjection.MillimetersToPixels(TmpCircle.Center.X - centerX),
        Y + AProjection.MillimetersToPixels(TmpCircle.Center.Y - centerY),
        radius,
        radius,
        cPen, PenWidth, cFill);
    end;
  end;
end;

procedure TMapPainterBGRA.DrawLabel(AProjection: TProjection;
  AParameter: TMapParameter; X, Y: TReal; const AMapLabel: TMapLabel;
  const ALabel: TLabelData);
var
  TmpStyle: TTextStyle;
  TextColor: TMapColor;
  cPen, cFill: TBGRAPixel;
begin
  if (ALabel.Style is TTextStyle) then
  begin
    TmpStyle := (ALabel.Style as TTextStyle);

    if (TmpStyle.Style = tssNormal) then
    begin
      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      cFill := GetBGRAPixel(TextColor);
    end
    else if (TmpStyle.Style = tssEmphasize) then
    begin
      // draw white border around text
      TextColor.Init(1, 1, 1, ALabel.Alpha);
      cPen := GetBGRAPixel(TextColor);

      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      cFill := GetBGRAPixel(TextColor);


      {DrawGlyphVector(X-1, Y + AMapLabel.Height, AMapLabel.glyphs);
      DrawGlyphVector(X+1, Y + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y-1 + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y+1 + AMapLabel.Height, layout.glyphs);}

    end;
    Bitmap.FontHeight := Round(ALabel.FontSize);
    Bitmap.TextOut(X, Y, AMapLabel.Text, cFill);

    {DrawGlyphVector(labelRectangle.x,
                    labelRectangle.y + labelRectangle.height,
                    layout.glyphs); }
  end;
end;

procedure TMapPainterBGRA.DrawPath(const AProjection: TProjection;
  const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
  const ADash: array of TReal; AStartCap: TLineCapStyle;
  AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer);
var
  i, n: Integer;
  Pixel: TVertex2D;

  points: array of TPointF;
  cPen, cFill: TBGRAPixel;
  PenWidth: Single;
begin
  if AParameter.IsDrawWaysBorder and (not FIsDrawingWayBorder) then
  begin
    FIsDrawingWayBorder := True;
    DrawPath(AProjection, AParameter, AColor.Darken(0.3),
      AWidth, ADash, AStartCap, AEndCap, ATransStart, ATransEnd);

    AWidth := AWidth - 0.5;
    FIsDrawingWayBorder := False;
  end;
  // !!
  if AWidth = 0 then AWidth := 0.5;

  //FAgg2D.ResetPath();
  cPen := GetBGRAPixel(AColor);
  Bitmap.PenStyle := psSolid;
  PenWidth := AWidth;

  {if (AStartCap = capButt) or (AEndCap = capButt) then
    FAgg2D.LineCap := lcButt
  else if (AStartCap = capSquare) or (AEndCap = capSquare) then
    FAgg2D.LineCap := lcSquare
  else
    FAgg2D.LineCap := lcRound;  }

  if Length(ADash) <> 0 then
  begin
    Bitmap.PenStyle := psDash;
    {i := 0;
    while i < Length(ADash)-1 do
    begin
      FAgg2D.AddDash(ADash[i] * AWidth, ADash[i+1] * AWidth);
      Inc(i, 2);
    end; }
  end;

  n := ATransEnd - ATransStart + 1;
  SetLength(points, n);
  for i := ATransStart to ATransEnd do
  begin
    Pixel := FTransBuffer.Buffer[i];
    points[i-ATransStart] := PointF(Pixel.X, Pixel.Y);
    {if (i = ATransStart) then
    begin
      FAgg2D.MoveTo(Pixel.X, Pixel.Y);
    end
    else
    begin
      FAgg2D.LineTo(Pixel.X, Pixel.Y);
    end;}
  end;
  // dpfStrokeOnly
  Bitmap.DrawPolyLineAntialias(points, cPen, PenWidth);

  // TODO: End point caps "dots"
end;

procedure TMapPainterBGRA.RegisterRegularLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabels: TLabelDataList;
  const APosition: TVertex2D; AObjectWidth: TReal);
begin
  FLabelLayouter.RegisterLabel(AProjection, AParameter, APosition, ALabels, AObjectWidth);
end;

procedure TMapPainterBGRA.RegisterContourLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabel: TPathLabelData;
  const ALabelPath: TLabelPath);
begin
  FLabelLayouter.RegisterContourLabel(AProjection, AParameter, ALabel, ALabelPath);
end;

procedure TMapPainterBGRA.DrawLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  TextLabel: TTextLabel;
  el: TTextLabelElement;
  OverlayElements, TextElements: TTextLabelElementList;
  elementRectangle: TDoubleRectangle;
  ConLabel: TContourLabel;
begin
  FLabelLayouter.Layout(AProjection, AParameter);
  //FLabelLayouter.DrawLabels(AProjection, AParameter);
  OverlayElements.Clear();
  TextElements.Clear();
  // draw symbols and icons first, then standard labels and then overlays

  for TextLabel in FLabelLayouter.TextLabels do
  begin
    if TextLabel.Priority < 0 then
      Continue;
    for el in TextLabel.Elements.Items do
    begin
      if (el.LabelData.DataType = ldtText) then
        elementRectangle.Init(el.x, el.y, el.MapLabel.Width, el.MapLabel.Height)
      else
        elementRectangle.Init(el.x, el.y, el.LabelData.IconWidth, el.LabelData.IconHeight);

      if (not FLabelLayouter.VisibleViewport.Intersects(elementRectangle)) then
        Continue;

      if (el.LabelData.DataType = ldtSymbol) then
      begin
        DrawSymbol(AProjection, AParameter,
          el.LabelData.IconStyle.Symbol,
          el.X + el.LabelData.IconWidth / 2,
          el.Y + el.LabelData.IconHeight / 2);
      end
      else if (el.LabelData.DataType = ldtIcon) then
      begin
        DrawIcon(el.LabelData.IconStyle,
          el.X + el.LabelData.IconWidth / 2,
          el.Y + el.LabelData.IconHeight / 2,
          el.LabelData.IconWidth, el.LabelData.IconHeight);
      end
      else
      begin
        // postpone text elements
        if (FLabelLayouter.IsOverlay(el.LabelData)) then
          OverlayElements.Add(el)
        else
          TextElements.Add(el);
      end;
    end;
  end;

  // draw postponed text elements
  for el in TextElements.Items do
  begin
    DrawLabel(AProjection, AParameter, el.X, el.Y, el.MapLabel, el.LabelData);
  end;

  for el in OverlayElements.Items do
  begin
    DrawLabel(AProjection, AParameter, el.X, el.Y, el.MapLabel, el.LabelData);
  end;

  for ConLabel in FLabelLayouter.ContourLabels do
  begin
    if ConLabel.Priority < 0 then
      Continue;
    DrawGlyphs(AProjection, AParameter, ConLabel.Style, ConLabel.Glyphs);
  end;
  FLabelLayouter.Reset();
end;

procedure TMapPainterBGRA.BeforeDrawing(const AStyleConfig: TStyleConfig;
  const AProjection: TProjection; const AParameter: TMapParameter;
  const AData: TMapData);
var
  V: TDoubleRectangle;
begin
  V.Init(0, 0, AProjection.Width, AProjection.Height);
  FLabelLayouter.SetViewport(V);
  if AParameter.IsDropNotVisiblePointLabels then
    FLabelLayouter.SetLayoutOverlap(0)
  else
    FLabelLayouter.SetLayoutOverlap(1);
end;

procedure TMapPainterBGRA.DrawContourSymbol(const AProjection: TProjection;
  const AParameter: TMapParameter; const ASymbol: TMapSymbol; ASpace: TReal;
  ATransStart, ATransEnd: Integer);
begin
  //
end;

procedure TMapPainterBGRA.DrawArea(const AProjection: TProjection;
  const AParameter: TMapParameter; const AAreaData: TAreaData);
var
  i, n: Integer;
  data: TPolyData;
  Pixel: TVertex2D;

  points: array of TPointF;
  cPen, cFill: TBGRAPixel;
  PenWidth: Single;
begin
  //FAgg2D.ResetPath();
  DrawFillStyle(AProjection, AParameter, AAreaData.FillStyle, AAreaData.BorderStyle);
  cPen :=  GetBorderColor(AAreaData.BorderStyle);
  cFill := GetFillColor(AAreaData.FillStyle);
  PenWidth := AProjection.MillimetersToPixels(AAreaData.BorderStyle.WidthMM);

  {if Length(AAreaData.Clippings) = 0 then
    FAgg2D.FillEvenOdd := True
  else
    FAgg2D.FillEvenOdd := False; // NonZero (Winding) rule }

  n := AAreaData.TransEnd - AAreaData.TransStart + 1;
  SetLength(points, n);

  for i := AAreaData.TransStart to AAreaData.TransEnd do
  begin
    Pixel := FTransBuffer.Buffer[i];
    points[i - AAreaData.TransStart] := PointF(Pixel.X, Pixel.Y);
  end;

  //FAgg2D.DrawPath(dpfFillAndStroke);
  Bitmap.DrawPolygonAntialias(points, cPen, PenWidth, cFill);

  if Length(AAreaData.Clippings) <> 0 then
  begin
    for data in AAreaData.Clippings do
    begin
      n := data.TransEnd - data.TransStart + 1;
      SetLength(points, n);

      for i := data.TransStart to data.TransEnd do
      begin
        points[i - data.TransStart] := PointF(FTransBuffer.Buffer[i].X, FTransBuffer.Buffer[i].Y);
      end;
      Bitmap.DrawPolygonAntialias(points, cPen, PenWidth, cFill);
    end;
  end;

  //FAgg2D.DrawPath(dpfFillAndStroke);

  // center point
  Pixel := FTransBuffer.Polylabel(AAreaData.TransStart, AAreaData.TransEnd);
  Bitmap.Ellipse(Pixel.X, Pixel.Y, 2, 2, cPen, PenWidth, TDrawMode.dmLinearBlend);
end;

{procedure TMapPainterBGRA.DrawLabelNative(AProjection: TProjection;
  AParameter: TMapParameter; const ALabelRectangle: TDoubleRectangle;
  const ALabel: TLabelData; const ALayout: TNativeLabel);
begin

end;

procedure TMapPainterBGRA.DrawGlyphsNative(AProjection: TProjection;
  AParameter: TMapParameter; AStyle: TPathTextStyle;
  const AGlyphs: TNativeGlyphArray);
begin

end; }

{
procedure TMapPainterBGRA.OnGlyphBoundingBoxHandler(const AGlyph: TMapGlyph;
  out ARect: TDoubleRectangle);
begin
  //Assert(False);
  //ARect.Init(AGlyph.AggGlyph.Bounds.X1, AGlyph.AggGlyph.Bounds.Y1,
  //            AGlyph.AggGlyph.Bounds.X2, AGlyph.AggGlyph.Bounds.Y2);
  ARect.X := AGlyph.Position.X;
  ARect.Y := AGlyph.Position.Y;
  ARect.Width := AGlyph.Width;
  ARect.Height := AGlyph.Height;
end;
}

procedure TMapPainterBGRA.OnTextLayoutHandler(out ALabel: TMapLabel;
  AProjection: TProjection; AParameter: TMapParameter; const AText: string;
  AFontSize, AObjectWidth: TReal; AEnableWrapping: Boolean;
  AContourLabel: Boolean);
var
  x, y, w, h, cw: TReal;
  sz: TSize;
  i: Integer;
  ws: WideString;
begin
  //Assert(False, 'not needed');

  //SetFont(AProjection, AParameter, AFontSize);
  Bitmap.FontHeight := Round(AFontSize);

  {if AContourLabel then
    agg::glyph_ren_outline
  else
    agg::glyph_ren_native_gray8;}

  //fontCacheManager->reset_last_glyph();
  h := Round(AFontSize);
  x := 0;
  y := 0;
  w := 0;
  y := Bitmap.FontVerticalAnchorOffset;

  ws := UTF8ToUTF16(AText);
  if AContourLabel then
  begin
    SetLength(ALabel.Glyphs, Length(ws));
    for i := 1 to Length(ws) do
    begin
      // get single character glyph bounds
      sz := Bitmap.TextSize(ws[i]);
      cw := sz.cx;
      if cw < 1 then
        cw := 1;
      //ALabel.Glyphs[i-1].Angle:=;

      ALabel.Glyphs[i-1].Position.X := x;
      ALabel.Glyphs[i-1].Position.Y := y;
      ALabel.Glyphs[i-1].Width := cw;
      ALabel.Glyphs[i-1].Height := sz.cy; // h;
      ALabel.Glyphs[i-1].TextChar := ws[i];

      w := w + cw;

      // increment pen position
      x := x + cw;
      //y += glyph->advance_y;
    end;
  end
  else
  begin
    w := Bitmap.TextSize(ws).cx;
  end;

  //ALabel.pLabel := Addr(FNativeLabel);
  ALabel.pLabel := nil;
  ALabel.Height := h;
  ALabel.Width := w;
  ALabel.FontSize := AFontSize;
  ALabel.Text := AText;
end;

constructor TMapPainterBGRA.Create(const AStyleConfig: TStyleConfig; ACanvas: TCanvas);
var
  x, y: Integer;
begin
  inherited Create(AStyleConfig);

  FCanvas := ACanvas;

  x := FCanvas.Width;
  y := FCanvas.Height;
  FBitmap := TBGRABitmap.Create(x, y);
  FLabelLayouter := TLabelLayouter.Create();
  //FLabelLayouter.OnGlyphBoundingBox := @OnGlyphBoundingBoxHandler;
  FLabelLayouter.OnTextLayout := @OnTextLayoutHandler;

  InitCriticalSection(FMutex);
  FFontSize := 0;
end;

destructor TMapPainterBGRA.Destroy;
begin
  DoneCriticalsection(FMutex);
  FreeAndNil(FLabelLayouter);
  if not FIsAlienBitmap then
  begin
    FreeAndNil(FBitmap);
  end;
  inherited Destroy;
end;

function TMapPainterBGRA.DrawMap(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData): Boolean;
begin
  EnterCriticalsection(FMutex);
  try
    //FAgg2D.ClearAll(255, 255, 255);
    FBitmap.Fill(TBGRAPixel.New(255, 255, 255));

    if FFontSize = 0 then
      SetFont(AProjection, AParameter, AParameter.FontSize);

    Result := inherited DrawMap(AProjection, AParameter, AData);

    if Result then
      FBitmap.Draw(FCanvas, 0, 0, True);
  finally
    LeaveCriticalsection(FMutex);
  end;
end;


end.
