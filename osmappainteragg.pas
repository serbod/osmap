unit OsMapPainterAgg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsMapPainter, OsMapLabels, OsMapParameters, OsMapStyles,
  OsMapTypes, OsMapTransform, OsMapGeometry, OsMapStyleConfig, OsMapProjection,
  Agg2D, AggFontCacheManager, AggPathStorage, AggColor, AggBasics, AggTransAffine;

type
  //TAggPixelFormat = TAggPixelFormat;

  TNativeGlyph = record
    X: Double;
    Y: Double;
    AggGlyph: TAggGlyphCache;
  end;

  TNativeGlyphArray = array of TNativeGlyph;

  TNativeLabel = record
    Text: string;
    Glyphs: TNativeGlyphArray;
  end;

  { TMapPainterAgg }

  TMapPainterAgg = class(TMapPainter)
  private
    FAgg2D: TAgg2D;
    FLabelLayouter: TLabelLayouter;
    FNativeLabel: TNativeLabel;
    FMutex: TRTLCriticalSection;

    FFontSize: Double;
    FIsAlienAgg: Boolean;
    FIsDrawingWayBorder: Boolean;

    function GetAggRgba8(AColor: TMapColor): TAggRgba8;
    procedure SetAgg2d(AValue: TAgg2D);

    procedure SetFont(AProjection: TProjection; AParameter: TMapParameter;
      ASize: Double); // agg::glyph_rendering ren_type = agg::glyph_ren_native_gray8

    procedure GetTextDimension(const AText: string; out AWidth, AHeight: Double);

    procedure DrawText(X, Y: Double; const AText: string);

    procedure DrawGlyph(X, Y: Double; const AGlyph: TAggGlyphCache);

    procedure DrawGlyphVector(X, ABaselineY: Double; const AGlyphs: TNativeGlyphArray);

    procedure DrawGlyphs(AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TPathTextStyle;
      AGlyphs: TMapGlyphArray);

    { ex-DrawFill(), set fill and border style }
    procedure DrawFillStyle(AProjection: TProjection; AParameter: TMapParameter;
      AFillStyle: TFillStyle; ABorderStyle: TBorderStyle);

  protected
    function HasIcon(AStyleConfig: TStyleConfig; AProjection: TProjection;
      AParameter: TMapParameter; AStyle: TIconStyle): Boolean; override;

    function GetFontHeight(const AProjection: TProjection;
      const AParameter: TMapParameter; AFontSize: Double): Double; override;

    procedure DrawGround(const AProjection: TProjection;
      const AParameter: TMapParameter; const AStyle: TFillStyle); override;

    procedure DrawIcon(AStyle: TIconStyle; ACenterX, ACenterY: Double;
      AWidth, Aheight: Double); override;

    procedure DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: Double); override;

    procedure DrawLabel(AProjection: TProjection;
      AParameter: TMapParameter;
      X, Y: Double;
      const AMapLabel: TMapLabel;
      const ALabel: TLabelData);

    procedure DrawPath(const AProjection: TProjection;
      const AParameter: TMapParameter; const AColor: TMapColor; AWidth: Double;
      const ADash: array of Double; AStartCap: TLineCapStyle;
      AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer); override;

    { Register regular label with given text at the given pixel coordinate
      in a style defined by the given LabelStyle. }
    procedure RegisterRegularLabel(const AProjection: TProjection;
      const AParameter: TMapParameter; const ALabels: TLabelDataList;
      const APosition: TVertex2D; AObjectWidth: Double); override;

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
      ASpace: Double; ATransStart, ATransEnd: Integer); override;

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
    procedure OnGlyphBoundingBoxHandler(const AGlyph: TMapGlyph; out ARect: TDoubleRectangle);

    { layout text for label }
    procedure OnTextLayoutHandler(out ALabel: TMapLabel;
      AProjection: TProjection;
      AParameter: TMapParameter;
      const AText: string;
      AFontSize, AObjectWidth: Double;
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

function TMapPainterAgg.GetAggRgba8(AColor: TMapColor): TAggRgba8;
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
  AParameter: TMapParameter; ASize: Double);
begin
  try
    // some bitmap format need to flip text upside-down
    FAgg2D.FlipText := True;
    FAgg2D.Font(PAnsiChar(AParameter.FontName), ASize, False, False, fcVector);
    FFontSize := ASize;
  except
    WriteLn('Cannot load font: ', AParameter.FontName);
  end;
end;

procedure TMapPainterAgg.GetTextDimension(const AText: string; out AWidth,
  AHeight: Double);
begin
  AWidth := FAgg2D.TextWidth(AText);
  AHeight := FAgg2D.FontHeight;
end;

procedure TMapPainterAgg.DrawText(X, Y: Double; const AText: string);
var
  ws: WideString;
begin
  //s := UTF8ToWinCP(AText);
  //s := UTF8ToSys(AText);
  //s := UTF8ToConsole(AText);
  ws := UTF8ToUTF16(AText);
  FAgg2D.Text(X, Y, ws);
end;

procedure TMapPainterAgg.DrawGlyph(X, Y: Double; const AGlyph: TAggGlyphCache);
begin
  //
end;

procedure TMapPainterAgg.DrawGlyphVector(X, ABaselineY: Double;
  const AGlyphs: TNativeGlyphArray);
begin
  //
end;

procedure TMapPainterAgg.DrawGlyphs(AProjection: TProjection;
  AParameter: TMapParameter; AStyle: TPathTextStyle; AGlyphs: TMapGlyphArray);
var
  //matrix: TAggTransAffine;
  layoutGlyph: TMapGlyph;
begin
  FAgg2D.LineColor := GetAggRgba8(AStyle.TextColor);

  //matrix := TAggTransAffine.Create();

  for layoutGlyph in AGlyphs do
  begin
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
  borderWidth, X, Y: Double;
  i: Integer;
begin
  if Assigned(AFillStyle) and AFillStyle.FillColor.IsVisible then
  begin
    FAgg2D.FillColor := GetAggRgba8(AFillStyle.FillColor);
  end;

  FAgg2D.RemoveAllDashes();

  if Assigned(ABorderStyle) then
  begin
    borderWidth := AProjection.ConvertWidthToPixel(ABorderStyle.Width);

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
  const AParameter: TMapParameter; AFontSize: Double): Double;
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
  ACenterY: Double; AWidth, Aheight: Double);
begin
  //
end;

procedure TMapPainterAgg.DrawSymbol(AProjection: TProjection;
      AParameter: TMapParameter;
      ASymbol: TMapSymbol;
      X, Y: Double);
var
  minX, minY, maxX, maxY, centerX, centerY: Double;
  Primitive: TDrawPrimitive;
  Polygon: TPolygonPrimitive;
  TmpRectangle: TRectanglePrimitive;
  TmpCircle: TCirclePrimitive;
  fillStyle: TFillStyle;
  borderStyle: TBorderStyle;
  i: Integer;
  Pixel: TVertex2D;
  xPos, yPos, width, height, radius: Double;
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
          FAgg2D.MoveTo(X + AProjection.ConvertWidthToPixel(Pixel.X - centerX),
                        Y + AProjection.ConvertWidthToPixel(Pixel.Y - centerY));
        end
        else
        begin
          FAgg2D.MoveTo(X + AProjection.ConvertWidthToPixel(Pixel.X - centerX),
                        Y + AProjection.ConvertWidthToPixel(Pixel.Y - centerY));
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
      xPos := x + AProjection.ConvertWidthToPixel(TmpRectangle.TopLeft.X - centerX);
      yPos := y + AProjection.ConvertWidthToPixel(TmpRectangle.TopLeft.Y - centerY);
      width := AProjection.ConvertWidthToPixel(TmpRectangle.Width);
      height := AProjection.ConvertWidthToPixel(TmpRectangle.Height);

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
      radius := AProjection.ConvertWidthToPixel(TmpCircle.Radius);

      FAgg2D.ResetPath();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);


      FAgg2D.Ellipse(X + AProjection.ConvertWidthToPixel(TmpCircle.Center.X - centerX),
                     Y + AProjection.ConvertWidthToPixel(TmpCircle.Center.Y - centerY),
                     radius, radius);

      FAgg2D.DrawPath(dpfFillAndStroke);
    end;
  end;
end;

procedure TMapPainterAgg.DrawLabel(AProjection: TProjection;
  AParameter: TMapParameter; X, Y: Double; const AMapLabel: TMapLabel;
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
    FAgg2D.FontHeight := ALabel.FontSize;
    FAgg2D.Text(X, Y, UTF8ToUTF16(AMapLabel.Text));

    {DrawGlyphVector(labelRectangle.x,
                    labelRectangle.y + labelRectangle.height,
                    layout.glyphs); }
  end;
end;

procedure TMapPainterAgg.DrawPath(const AProjection: TProjection;
  const AParameter: TMapParameter; const AColor: TMapColor; AWidth: Double;
  const ADash: array of Double; AStartCap: TLineCapStyle;
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
      {if FTransBuffer.Buffer[i].DistanceTo(Pixel) > 500 then
      begin
        FAgg2D.ResetPath();
        Exit;
      end;}
      Pixel := FTransBuffer.Buffer[i];
      FAgg2D.LineTo(Pixel.X, Pixel.Y);
    end;
  end;
  FAgg2D.DrawPath(dpfStrokeOnly);

  // TODO: End point caps "dots"
end;

procedure TMapPainterAgg.RegisterRegularLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabels: TLabelDataList;
  const APosition: TVertex2D; AObjectWidth: Double);
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
begin
  FLabelLayouter.Layout(AProjection, AParameter);
  FLabelLayouter.DrawLabels(AProjection, AParameter);
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
  const AParameter: TMapParameter; const ASymbol: TMapSymbol; ASpace: Double;
  ATransStart, ATransEnd: Integer);
begin
  //
end;

procedure TMapPainterAgg.DrawArea(const AProjection: TProjection;
  const AParameter: TMapParameter; const AAreaData: TAreaData);
var
  i: Integer;
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
    {if FTransBuffer.Buffer.Buffer[i].DistanceTo(Pixel) > 20 then
    begin
      //Assert(false);
      FAgg2D.ResetPath();
      Exit;
    end; }
    Pixel := FTransBuffer.Buffer[i];
    FAgg2D.LineTo(Pixel.X, Pixel.Y);
  end;
  FAgg2D.ClosePolygon();

  if Length(AAreaData.Clippings) <> 0 then
  begin
    for data in AAreaData.Clippings do
    begin
      FAgg2D.MoveTo(FTransBuffer.Buffer[data.TransStart].X,
                     FTransBuffer.Buffer[data.TransStart].Y);
      for i := data.TransStart+1 to data.TransEnd do
      begin
        FAgg2D.LineTo(FTransBuffer.Buffer[i].X,
                      FTransBuffer.Buffer[i].Y);
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

end;  }

procedure TMapPainterAgg.OnGlyphBoundingBoxHandler(const AGlyph: TMapGlyph;
  out ARect: TDoubleRectangle);
begin
  Assert(False);
  //AGlyph.Position;
  //ARect.Init(AGlyph.AggGlyph.Bounds.X1, AGlyph.AggGlyph.Bounds.Y1,
  //            AGlyph.AggGlyph.Bounds.X2, AGlyph.AggGlyph.Bounds.Y2);
end;

procedure TMapPainterAgg.OnTextLayoutHandler(out ALabel: TMapLabel;
  AProjection: TProjection; AParameter: TMapParameter; const AText: string;
  AFontSize, AObjectWidth: Double; AEnableWrapping: Boolean;
  AContourLabel: Boolean);
var
  x, y, w, h, cw: Double;
  i: Integer;
  ws: WideString;
begin
  //Assert(False, 'not needed');
  FNativeLabel.Text := AText;

  //SetFont(AProjection, AParameter, AFontSize);
  FAgg2D.FontHeight := AFontSize;

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
    SetLength(FNativeLabel.Glyphs, Length(ws));
    for i := 1 to Length(ws) do
    begin
      //const agg::glyph_cache *glyph = fontCacheManager->glyph(i);
      //fontCacheManager->add_kerning(&x, &y);
      //label.glyphs.emplace_back(std::move(MapPainterAgg::NativeGlyph{x, y, glyph}));
      cw := FAgg2D.TextWidth(ws[i]);

      FNativeLabel.Glyphs[i-1].X := x;
      FNativeLabel.Glyphs[i-1].Y := y;

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

  ALabel.pLabel := Addr(FNativeLabel);
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
  FLabelLayouter.OnDrawGlyphs := @DrawGlyphs;
  FLabelLayouter.OnDrawIcon := @DrawIcon;
  FLabelLayouter.OnDrawLabel := @DrawLabel;
  FLabelLayouter.OnDrawSymbol := @DrawSymbol;
  FLabelLayouter.OnGlyphBoundingBox := @OnGlyphBoundingBoxHandler;
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
  EnterCriticalsection(FMutex);
  try
    FAgg2D.ClearAll(255, 255, 255);

    if FFontSize = 0 then
      SetFont(AProjection, AParameter, AParameter.FontSize);

    Result := inherited DrawMap(AProjection, AParameter, AData);
  finally
    LeaveCriticalsection(FMutex);
  end;
end;


end.

