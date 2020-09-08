unit OsMapPainterAgg;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, OsMapPainter, OsMapLabels, OsMapParameters, OsMapStyles,
  OsMapTypes, OsMapTransform, OsMapGeometry, OsMapStyleConfig, OsMapProjection,
  OsMapObjects,
  Agg2D, AggFontCacheManager, AggPathStorage, AggColor, AggBasics, AggTransAffine;

type
  //TAggPixelFormat = TAggPixelFormat;

  TNativeGlyph = record
    X: TReal;
    Y: TReal;
    AggGlyph: TAggGlyphCache;
  end;

  TNativeGlyphArray = array of TNativeGlyph;

  {TNativeLabel = record
    Text: string;
    Glyphs: TNativeGlyphArray;
  end; }

  { TMapPainterAgg }

  TMapPainterAgg = class(TMapPainter)
  private
    FAgg2D: TAgg2D;
    FLabelLayouter: TLabelLayouter;
    FMutex: TRTLCriticalSection;

    FFontSize: TReal;
    FIsAlienAgg: Boolean;
    FIsDrawingWayBorder: Boolean;

    function GetAggRgba8(const AColor: TMapColor): TAggRgba8;
    procedure SetAgg2d(AValue: TAgg2D);

    procedure SetFont(AProjection: TProjection; AParameter: TMapParameter;
      ASize: TReal); // agg::glyph_rendering ren_type = agg::glyph_ren_native_gray8

    procedure GetTextDimension(const AText: string; out AWidth, AHeight: TReal);

    //procedure DrawText(X, Y: TReal; const AText: string);

    procedure DrawGlyph(X, Y: TReal; const AGlyph: TAggGlyphCache);

    procedure DrawGlyphVector(X, ABaselineY: TReal; const AGlyphs: TNativeGlyphArray);

    procedure DrawGlyphs(AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TPathTextStyle;
      const AGlyphs: TMapGlyphArray);

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
    constructor Create(const AStyleConfig: TStyleConfig);
    destructor Destroy; override;

    function DrawMap(AProjection: TProjection;
      AParameter: TMapParameter;
      AData: TMapData): Boolean; override;

    property Agg2D: TAgg2D read FAgg2D write SetAgg2d;
  end;

implementation

uses Math, LazUTF8;

{ TMapPainterAgg }

function TMapPainterAgg.GetAggRgba8(const AColor: TMapColor): TAggRgba8;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgbaDouble(AColor.R, AColor.G, AColor.B, AColor.A);
  Result := AggColor.Rgba8;
end;

procedure TMapPainterAgg.SetAgg2d(AValue: TAgg2D);
begin
  if FAgg2D = AValue then Exit;
  if not FIsAlienAgg then
  begin
    FreeAndNil(FAgg2D);
    FIsAlienAgg := True;
  end;
  FAgg2D := AValue;
end;

procedure TMapPainterAgg.SetFont(AProjection: TProjection;
  AParameter: TMapParameter; ASize: TReal);
begin
  try
    // some bitmap format need to flip text upside-down
    FAgg2D.FlipText := True;
    if FFontSize <> ASize then
    begin
      FAgg2D.Font(PAnsiChar(AParameter.FontName), ASize, False, False, fcVector);
      FFontSize := ASize;
    end;
  except
    WriteLn('Cannot load font: ', AParameter.FontName);
  end;
end;

procedure TMapPainterAgg.GetTextDimension(const AText: string; out AWidth,
  AHeight: TReal);
begin
  AWidth := FAgg2D.TextWidth(AText);
  AHeight := FAgg2D.FontHeight;
end;

{procedure TMapPainterAgg.DrawText(X, Y: TReal; const AText: string);
var
  ws: WideString;
begin
  //s := UTF8ToWinCP(AText);
  //s := UTF8ToSys(AText);
  //s := UTF8ToConsole(AText);
  ws := UTF8ToUTF16(AText);
  FAgg2D.FlipText := True;
  FAgg2D.Text(X, Y, ws);
end; }

procedure TMapPainterAgg.DrawGlyph(X, Y: TReal; const AGlyph: TAggGlyphCache);
begin
  //
end;

procedure TMapPainterAgg.DrawGlyphVector(X, ABaselineY: TReal;
  const AGlyphs: TNativeGlyphArray);
begin
  //
end;

procedure TMapPainterAgg.DrawGlyphs(AProjection: TProjection;
  AParameter: TMapParameter;
  AStyle: TPathTextStyle;
  const AGlyphs: TMapGlyphArray);
var
  //matrix: TAggTransAffine;
  layoutGlyph: TMapGlyph;
  //i: Integer;
  TextColor: TMapColor;
  TmpFontSize: Single;
begin
  Assert(Assigned(AStyle));
  TextColor.Init(0, 0, 0, 1);
  //FAgg2D.FillColor := GetAggRgba8(AStyle.TextColor);
  FAgg2D.FillColor := GetAggRgba8(TextColor);
  FAgg2D.NoLine();
  //FAgg2D.FlipText := True;
  //FAgg2D.LineColor := GetAggRgba8(AStyle.TextColor);
  TmpFontSize := AProjection.MillimetersToPixels(AStyle.SizeMM);
  SetFont(AProjection, AParameter, TmpFontSize);

  //matrix := TAggTransAffine.Create();

  //for i := 0 to Length(AGlyphs)-1 do
  for layoutGlyph in AGlyphs do
  begin
    //layoutGlyph := AGlyphs[i];
    // contour labels should always use outline rendering
    //Assert(TNativeGlyph(layoutGlyph.Glyph).AggGlyph.DataType = gdOutline);

    {matrix.Reset();
    matrix.Rotate(layoutGlyph.Angle);
    matrix.Translate(layoutGlyph.Position.X, layoutGlyph.Position.Y);

    agg::conv_transform<AggTextCurveConverter> ftrans( *convTextCurves, matrix);

    rasterizer->reset();
    fontCacheManager->init_embedded_adaptors(layoutGlyph.glyph.aggGlyph,
                                             0, 0);
    rasterizer->add_path(ftrans);
    agg::render_scanlines( *rasterizer,
                          *scanlineP8,
                          *renderer_aa);   }

    //FAgg2D.FontHeight := layoutGlyph.Height;
    FAgg2D.TextAngle := layoutGlyph.Angle;
    FAgg2D.Text(layoutGlyph.Position.X,
      layoutGlyph.Position.Y,
      layoutGlyph.TextChar);
  end;
end;

procedure TMapPainterAgg.DrawFillStyle(AProjection: TProjection;
  AParameter: TMapParameter;
  AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
var
  borderWidth: TReal;
  i: Integer;
begin
  if Assigned(AFillStyle) and AFillStyle.FillColor.IsVisible then
  begin
    FAgg2D.FillColor := GetAggRgba8(AFillStyle.FillColor);
  end
  else
    FAgg2D.NoFill();

  FAgg2D.RemoveAllDashes();

  if Assigned(ABorderStyle) then
  begin
    borderWidth := AProjection.MillimetersToPixels(ABorderStyle.WidthMM);

    if (borderWidth >= AParameter.LineMinWidthPixel) then
    begin
      FAgg2D.LineColor := GetAggRgba8(ABorderStyle.Color);

      if Length(ABorderStyle.Dash) = 0 then
      begin
        FAgg2D.LineWidth := borderWidth;
        FAgg2D.LineCap := lcRound;
      end
      else
      begin
        FAgg2D.LineWidth := borderWidth;
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
end;

function TMapPainterAgg.HasIcon(AStyleConfig: TStyleConfig;
  AProjection: TProjection; AParameter: TMapParameter; AStyle: TIconStyle): Boolean;
begin
  Result := False;
end;

function TMapPainterAgg.GetFontHeight(const AProjection: TProjection;
  const AParameter: TMapParameter; AFontSize: TReal): TReal;
begin
  if (FFontSize <> 0) then
    Result := FAgg2D.FontHeight / FFontSize * AFontSize
  else
    Result := FAgg2D.FontHeight;
end;

procedure TMapPainterAgg.DrawGround(const AProjection: TProjection;
  const AParameter: TMapParameter; const AStyle: TFillStyle);
begin
  FAgg2D.ResetPath();
  FAgg2D.FillColor := GetAggRgba8(AStyle.FillColor);
  FAgg2D.FillEvenOdd := False; // FillNonZero

  FAgg2D.MoveTo(0, 0);
  FAgg2D.LineTo(AProjection.Width, 0);
  FAgg2D.LineTo(AProjection.Width, AProjection.Height);
  FAgg2D.LineTo(0, AProjection.Height);
  FAgg2D.ClosePolygon();
  FAgg2D.DrawPath(dpfFillOnly);
end;

procedure TMapPainterAgg.DrawIcon(AStyle: TIconStyle; ACenterX,
  ACenterY: TReal; AWidth, Aheight: TReal);
begin
  //
end;

procedure TMapPainterAgg.DrawSymbol(AProjection: TProjection;
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

      FAgg2D.ResetPath();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);

      for i := 0 to Length(Polygon.Coords)-1 do
      begin
        Pixel := Polygon.Coords[i];
        if i = 0 then
        begin
          FAgg2D.MoveTo(X + AProjection.MillimetersToPixels(Pixel.X - centerX),
                        Y + AProjection.MillimetersToPixels(Pixel.Y - centerY));
        end
        else
        begin
          FAgg2D.MoveTo(X + AProjection.MillimetersToPixels(Pixel.X - centerX),
                        Y + AProjection.MillimetersToPixels(Pixel.Y - centerY));
        end;
      end;
      FAgg2D.ClosePolygon();
      FAgg2D.DrawPath(dpfFillAndStroke);
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

      FAgg2D.ResetPath();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);

      FAgg2D.MoveTo(xPos, yPos);
      FAgg2D.LineTo(xPos + width, yPos);
      FAgg2D.LineTo(xPos + width, yPos + height);
      FAgg2D.LineTo(xPos, yPos + height);

      FAgg2D.ClosePolygon();
      FAgg2D.DrawPath(dpfFillAndStroke);
    end
    else if (Primitive is TCirclePrimitive) then
    begin
      TmpCircle := (Primitive as TCirclePrimitive);
      fillStyle := TmpCircle.FillStyle;
      borderStyle := TmpCircle.BorderStyle;
      radius := AProjection.MillimetersToPixels(TmpCircle.Radius);

      FAgg2D.ResetPath();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);


      FAgg2D.Ellipse(X + AProjection.MillimetersToPixels(TmpCircle.Center.X - centerX),
                     Y + AProjection.MillimetersToPixels(TmpCircle.Center.Y - centerY),
                     radius, radius);

      FAgg2D.DrawPath(dpfFillAndStroke);
    end;
  end;
end;

procedure TMapPainterAgg.DrawLabel(AProjection: TProjection;
  AParameter: TMapParameter; X, Y: TReal; const AMapLabel: TMapLabel;
  const ALabel: TLabelData);
var
  TmpStyle: TTextStyle;
  TextColor: TMapColor;
begin
  if (ALabel.Style is TTextStyle) then
  begin
    TmpStyle := (ALabel.Style as TTextStyle);

    if (TmpStyle.Style = tssNormal) then
    begin
      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      //FAgg2D.LineColor := GetAggRgba8(TextColor);
      FAgg2D.FillColor := GetAggRgba8(TextColor);
      FAgg2D.NoLine();
    end
    else if (TmpStyle.Style = tssEmphasize) then
    begin
      // draw white border around text
      TextColor.Init(1, 1, 1, ALabel.Alpha);
      FAgg2D.LineColor := GetAggRgba8(TextColor);

      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      FAgg2D.FillColor := GetAggRgba8(TextColor);


      {DrawGlyphVector(X-1, Y + AMapLabel.Height, AMapLabel.glyphs);
      DrawGlyphVector(X+1, Y + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y-1 + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y+1 + AMapLabel.Height, layout.glyphs);}

    end;
    //FAgg2D.FontHeight := ALabel.FontSize;
    SetFont(AProjection, AParameter, ALabel.FontSize);
    FAgg2D.FlipText := True;
    FAgg2D.TextAngle := 0;
    FAgg2D.Text(X, Y, UTF8ToUTF16(AMapLabel.Text));

    {DrawGlyphVector(labelRectangle.x,
                    labelRectangle.y + labelRectangle.height,
                    layout.glyphs); }
  end;
end;

procedure TMapPainterAgg.DrawPath(const AProjection: TProjection;
  const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
  const ADash: array of TReal; AStartCap: TLineCapStyle;
  AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer);
var
  i: Integer;
  Pixel: TVertex2D;
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

  FAgg2D.ResetPath();
  FAgg2D.LineColor := GetAggRgba8(AColor);
  FAgg2D.RemoveAllDashes();
  FAgg2D.LineWidth := AWidth;

  if (AStartCap = capButt) or (AEndCap = capButt) then
    FAgg2D.LineCap := lcButt
  else if (AStartCap = capSquare) or (AEndCap = capSquare) then
    FAgg2D.LineCap := lcSquare
  else
    FAgg2D.LineCap := lcRound;

  if Length(ADash) <> 0 then
  begin
    i := 0;
    while i < Length(ADash)-1 do
    begin
      FAgg2D.AddDash(ADash[i] * AWidth, ADash[i+1] * AWidth);
      Inc(i, 2);
    end;
  end;

  for i := ATransStart to ATransEnd do
  begin
    if (i = ATransStart) then
    begin
      Pixel := FTransBuffer.Buffer[i];
      FAgg2D.MoveTo(Pixel.X, Pixel.Y);
    end
    else
    begin
      Pixel := FTransBuffer.Buffer[i];
      FAgg2D.LineTo(Pixel.X, Pixel.Y);
    end;
  end;
  FAgg2D.DrawPath(dpfStrokeOnly);

  // TODO: End point caps "dots"
end;

procedure TMapPainterAgg.RegisterRegularLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabels: TLabelDataList;
  const APosition: TVertex2D; AObjectWidth: TReal);
begin
  FLabelLayouter.RegisterLabel(AProjection, AParameter, APosition, ALabels, AObjectWidth);
end;

procedure TMapPainterAgg.RegisterContourLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabel: TPathLabelData;
  const ALabelPath: TLabelPath);
begin
  FLabelLayouter.RegisterContourLabel(AProjection, AParameter, ALabel, ALabelPath);
end;

procedure TMapPainterAgg.DrawLabels(const AProjection: TProjection;
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

procedure TMapPainterAgg.BeforeDrawing(const AStyleConfig: TStyleConfig;
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

procedure TMapPainterAgg.DrawContourSymbol(const AProjection: TProjection;
  const AParameter: TMapParameter; const ASymbol: TMapSymbol; ASpace: TReal;
  ATransStart, ATransEnd: Integer);
begin
  //
end;

procedure TMapPainterAgg.DrawArea(const AProjection: TProjection;
  const AParameter: TMapParameter; const AAreaData: TAreaData);
var
  i, n: Integer;
  data: TPolyData;
  Pixel: TVertex2D;
begin
  FAgg2D.ResetPath();
  DrawFillStyle(AProjection, AParameter, AAreaData.FillStyle, AAreaData.BorderStyle);

  if Length(AAreaData.Clippings) = 0 then
    FAgg2D.FillEvenOdd := True
  else
    FAgg2D.FillEvenOdd := False; // NonZero (Winding) rule

  Pixel := FTransBuffer.Buffer[AAreaData.TransStart];
  FAgg2D.MoveTo(Pixel.X, Pixel.Y);

  for i := AAreaData.TransStart+1 to AAreaData.TransEnd do
  begin
    Pixel := FTransBuffer.Buffer[i];
    FAgg2D.LineTo(Pixel.X, Pixel.Y);
  end;
  FAgg2D.ClosePolygon();

  if Length(AAreaData.Clippings) <> 0 then
  begin
    for data in AAreaData.Clippings do
    begin
      // to draw hole in polygon, inner path must be reverse-ordered
      //n := data.TransEnd;
      n := data.TransStart;
      FAgg2D.MoveTo(FTransBuffer.Buffer[n].X,
                     FTransBuffer.Buffer[n].Y);
      //while n >= data.TransStart do
      while n <= data.TransEnd do
      begin
        FAgg2D.LineTo(FTransBuffer.Buffer[n].X,
                      FTransBuffer.Buffer[n].Y);
        //Dec(n);
        Inc(n);
      end;
      FAgg2D.ClosePolygon();
    end;
  end;

  FAgg2D.DrawPath(dpfFillAndStroke);

  // center point
  Pixel := FTransBuffer.Polylabel(AAreaData.TransStart, AAreaData.TransEnd);
  FAgg2D.Circle(Pixel.X, Pixel.Y, 2);
end;

{procedure TMapPainterAgg.DrawLabelNative(AProjection: TProjection;
  AParameter: TMapParameter; const ALabelRectangle: TDoubleRectangle;
  const ALabel: TLabelData; const ALayout: TNativeLabel);
begin

end;

procedure TMapPainterAgg.DrawGlyphsNative(AProjection: TProjection;
  AParameter: TMapParameter; AStyle: TPathTextStyle;
  const AGlyphs: TNativeGlyphArray);
begin

end; }

{
procedure TMapPainterAgg.OnGlyphBoundingBoxHandler(const AGlyph: TMapGlyph;
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

procedure TMapPainterAgg.OnTextLayoutHandler(out ALabel: TMapLabel;
  AProjection: TProjection; AParameter: TMapParameter; const AText: string;
  AFontSize, AObjectWidth: TReal; AEnableWrapping: Boolean;
  AContourLabel: Boolean);
var
  x, y, w, h, cw: TReal;
  i: Integer;
  ws: WideString;
begin
  //Assert(False, 'not needed');

  SetFont(AProjection, AParameter, AFontSize);
  //FAgg2D.FontHeight := AFontSize;

  {if AContourLabel then
    agg::glyph_ren_outline
  else
    agg::glyph_ren_native_gray8;}

  //fontCacheManager->reset_last_glyph();
  x := 0;
  y := 0;
  w := 0;
  h := FAgg2D.FontHeight;

  ws := UTF8ToUTF16(AText);
  if AContourLabel then
  begin
    SetLength(ALabel.Glyphs, Length(ws));
    for i := 1 to Length(ws) do
    begin
      // get single character glyph bounds
      //const agg::glyph_cache *glyph = fontCacheManager->glyph(i);
      //fontCacheManager->add_kerning(&x, &y);
      //label.glyphs.emplace_back(std::move(MapPainterAgg::NativeGlyph{x, y, glyph}));
      cw := FAgg2D.TextWidth(ws[i]);

      ALabel.Glyphs[i-1].Position.X := x;
      ALabel.Glyphs[i-1].Position.Y := y;
      ALabel.Glyphs[i-1].Width := cw;
      ALabel.Glyphs[i-1].Height := h;
      ALabel.Glyphs[i-1].TextChar := ws[i];

      w := w + cw;

      // increment pen position
      x := x + cw;
      //y += glyph->advance_y;
    end;
  end
  else
  begin
    w := FAgg2D.TextWidth(ws);
  end;

  //ALabel.pLabel := Addr(FNativeLabel);
  ALabel.pLabel := nil;
  ALabel.Height := h;
  ALabel.Width := w;
  ALabel.FontSize := AFontSize;
  ALabel.Text := AText;
end;

constructor TMapPainterAgg.Create(const AStyleConfig: TStyleConfig);
begin
  inherited Create(AStyleConfig);

  FAgg2D := TAgg2D.Create();
  FLabelLayouter := TLabelLayouter.Create();
  //FLabelLayouter.OnGlyphBoundingBox := @OnGlyphBoundingBoxHandler;
  FLabelLayouter.OnTextLayout := @OnTextLayoutHandler;

  InitCriticalSection(FMutex);
  FFontSize := 0;
end;

destructor TMapPainterAgg.Destroy;
begin
  DoneCriticalsection(FMutex);
  FreeAndNil(FLabelLayouter);
  if not FIsAlienAgg then
  begin
    FreeAndNil(FAgg2D);
  end;
  inherited Destroy;
end;

function TMapPainterAgg.DrawMap(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData): Boolean;
begin
  CheckDebug(AData);
  EnterCriticalsection(FMutex);
  try
    FAgg2D.ClearAll(255, 255, 255);
    CheckDebug(AData);

    if FFontSize = 0 then
      SetFont(AProjection, AParameter, AParameter.FontSize);

    Result := inherited DrawMap(AProjection, AParameter, AData);
  finally
    LeaveCriticalsection(FMutex);
  end;
end;


end.

