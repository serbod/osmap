{ Delphi FMX only }
unit OsMapPainterCanvas;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$ZEROBASEDSTRINGS OFF}
{$endif}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$ifndef FPC}
  UITypes, FMX.Graphics, Types, FMX.Types, Math.Vectors,
  {$endif}
  OsMapPainter, OsMapLabels, OsMapParameters, OsMapStyles,
  OsMapTypes, OsMapTransform, OsMapGeometry, OsMapStyleConfig, OsMapProjection,
  OsMapObjects;

type
  TNativeGlyph = record
    X: TReal;
    Y: TReal;
    //AggGlyph: TAggGlyphCache;
    Glyph: Char;
  end;

  TNativeGlyphArray = array of TNativeGlyph;

  {TNativeLabel = record
    Text: string;
    Glyphs: TNativeGlyphArray;
  end; }

  { TMapPainterCanvas }

  TMapPainterCanvas = class(TMapPainter)
  private
    FCanvas: TCanvas;
    FLabelLayouter: TLabelLayouter;
    FMutex: TCriticalSection;

    FFontSize: TReal;
    FIsAlienAgg: Boolean;
    FIsDrawingWayBorder: Boolean;

    function GetAlphaColor(const AColor: TMapColor): TAlphaColor;
    procedure SetCanvas(AValue: TCanvas);

    procedure SetFont(AProjection: TProjection; AParameter: TMapParameter;
      ASize: TReal); // agg::glyph_rendering ren_type = agg::glyph_ren_native_gray8

    procedure GetTextDimension(const AText: string; out AWidth, AHeight: TReal);

    //procedure DrawText(X, Y: TReal; const AText: string);

    //procedure DrawGlyph(X, Y: TReal; const AGlyph: TAggGlyphCache);

    procedure DrawGlyphVector(X, ABaselineY: TReal; const AGlyphs: TNativeGlyphArray);

    procedure DrawGlyphs(AProjection: TProjection;
      AParameter: TMapParameter;
      AStyle: TPathTextStyle;
      const AGlyphs: TMapGlyphArray);

    {procedure DrawTextPath(AProjection: TProjection;
      AParameter: TMapParameter;
      const AContourLabel: TContourLabel); }

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

    property Canvas: TCanvas read FCanvas write SetCanvas;
  end;

implementation

uses Math;

procedure AddDash(var ADashArray: TDashArray; ALine, ASpace: Single);
var
  n: Integer;
begin
  n := Length(ADashArray);
  SetLength(ADashArray, n + 2);
  ADashArray[n] := ALine;
  ADashArray[n+1] := ASpace;
end;

{ TMapPainterCanvas }

function TMapPainterCanvas.GetAlphaColor(const AColor: TMapColor): TAlphaColor;
begin
  TAlphaColorRec(Result).R := Round(AColor.R * 255);
  TAlphaColorRec(Result).G := Round(AColor.G * 255);
  TAlphaColorRec(Result).B := Round(AColor.B * 255);
  TAlphaColorRec(Result).A := Round(AColor.A * 255);
end;

procedure TMapPainterCanvas.SetCanvas(AValue: TCanvas);
begin
  if FCanvas = AValue then Exit;
  if not FIsAlienAgg then
  begin
    //FreeAndNil(FCanvas);
    FIsAlienAgg := True;
  end;
  FCanvas := AValue;
end;

procedure TMapPainterCanvas.SetFont(AProjection: TProjection;
  AParameter: TMapParameter; ASize: TReal);
begin
  try
    FCanvas.Font.Family := AParameter.FontName;
    FCanvas.Font.Size := ASize;
    FFontSize := ASize;
  except
    WriteLn('Cannot load font: ', AParameter.FontName);
  end;
end;

procedure TMapPainterCanvas.GetTextDimension(const AText: string; out AWidth,
  AHeight: TReal);
begin
  AWidth := FCanvas.TextWidth(AText);
  AHeight := FCanvas.TextHeight(AText);
end;

{procedure TMapPainterCanvas.DrawText(X, Y: TReal; const AText: string);
var
  ws: WideString;
begin
  //s := UTF8ToWinCP(AText);
  //s := UTF8ToSys(AText);
  //s := UTF8ToConsole(AText);
  ws := UTF8ToUTF16(AText);

  FCanvas.FillText  Text(X, Y, ws);
end; }

procedure TMapPainterCanvas.DrawGlyphVector(X, ABaselineY: TReal;
  const AGlyphs: TNativeGlyphArray);
begin
  //
end;

procedure TMapPainterCanvas.DrawGlyphs(AProjection: TProjection;
  AParameter: TMapParameter;
  AStyle: TPathTextStyle;
  const AGlyphs: TMapGlyphArray);
var
  //matrix: TAggTransAffine;
  SaveMatrix: TMatrix;
  m: TMatrix;
  layoutGlyph: TMapGlyph;
  //i: Integer;
  TextColor: TMapColor;
  r: TRectF;
  pd: TPathData;
begin
  Assert(Assigned(AStyle));
  TextColor.Init(0, 0, 0, 1);
  //FAgg2D.FillColor := GetAggRgba8(AStyle.TextColor);
  FCanvas.BeginScene();
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Thickness := 1;
  FCanvas.Fill.Color := GetAlphaColor(TextColor);
  FCanvas.Fill.Kind := TBrushKind.Solid;
  //FCanvas.NoLine();
  //FAgg2D.FlipText := True;
  //FAgg2D.LineColor := GetAggRgba8(AStyle.TextColor);

  //matrix := TAggTransAffine.Create();
  pd := TPathData.Create();

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

    //r.Left := layoutGlyph.Position.X;
    //r.Top := layoutGlyph.Position.Y;
    //r.Width := layoutGlyph.Width + 5;
    //r.Height := layoutGlyph.Height + 5;
    r.Left := 0;
    r.Top := -layoutGlyph.Height;
    r.Width := layoutGlyph.Width;
    r.Height := layoutGlyph.Height;

    {SaveMatrix := FCanvas.Matrix;
    m := TMatrix.CreateRotation(layoutGlyph.Angle);
    m.m31 := layoutGlyph.Position.X;
    m.m32 := layoutGlyph.Position.Y;
    FCanvas.SetMatrix(m);


    FCanvas.FillText(r,
      layoutGlyph.TextChar,
      False, 1,
      [TFillTextFlag.RightToLeft],
      TTextAlign.Center,
      TTextAlign.Center);

    FCanvas.SetMatrix(SaveMatrix);}

    {TextAngle := layoutGlyph.Angle;
    FCanvas.Text(layoutGlyph.Position.X,
      layoutGlyph.Position.Y,
      layoutGlyph.TextChar); }

    pd.Clear();
    FCanvas.TextToPath(pd, r, layoutGlyph.TextChar, False, TTextAlign.Center, TTextAlign.Center);
    m := TMatrix.CreateRotation(layoutGlyph.Angle);
    m.m31 := layoutGlyph.Position.X;
    m.m32 := layoutGlyph.Position.Y;
    pd.ApplyMatrix(m);
    FCanvas.FillPath(pd, 1);
  end;
  pd.Free();
  FCanvas.EndScene();
end;

procedure TMapPainterCanvas.DrawFillStyle(AProjection: TProjection;
  AParameter: TMapParameter;
  AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
var
  borderWidth: TReal;
  i: Integer;
  da: TDashArray;
begin
  // BeginScene/EndScane not needed, because called outside
  if Assigned(AFillStyle) and AFillStyle.FillColor.IsVisible then
  begin
    FCanvas.Fill.Color := GetAlphaColor(AFillStyle.FillColor);
    FCanvas.Fill.Kind := TBrushKind.Solid;
  end
  else
  begin
    FCanvas.Fill.Kind := TBrushKind.None;
  end;

  //FCanvas.RemoveAllDashes();
  SetLength(da, 0);

  if Assigned(ABorderStyle) then
  begin
    borderWidth := AProjection.ConvertWidthToPixel(ABorderStyle.Width);

    if (borderWidth >= AParameter.LineMinWidthPixel) then
    begin
      FCanvas.Stroke.Color := GetAlphaColor(ABorderStyle.Color);
      FCanvas.Stroke.Kind := TBrushKind.Solid;

      if Length(ABorderStyle.Dash) = 0 then
      begin
        FCanvas.Stroke.Thickness := borderWidth;
        FCanvas.Stroke.Cap := TStrokeCap.Round;
        FCanvas.Stroke.Dash := TStrokeDash.Solid;
      end
      else
      begin
        FCanvas.Stroke.Thickness := borderWidth;
        FCanvas.Stroke.Cap := TStrokeCap.Flat;
        i := 0;
        while i < Length(ABorderStyle.Dash)-1 do
        begin
          AddDash(da,
            ABorderStyle.Dash[i] * borderWidth,
            ABorderStyle.Dash[i+1] * borderWidth);
          Inc(i, 2);
        end;

        FCanvas.Stroke.SetCustomDash(da, FCanvas.Stroke.DashOffset);
      end;
    end
    else
    begin
      // no border
      FCanvas.Stroke.Kind := TBrushKind.None;
    end;
  end
  else
  begin
    // no border
    FCanvas.Stroke.Kind := TBrushKind.None;
  end;
end;

function TMapPainterCanvas.HasIcon(AStyleConfig: TStyleConfig;
  AProjection: TProjection; AParameter: TMapParameter; AStyle: TIconStyle): Boolean;
begin
  Result := False;
end;

function TMapPainterCanvas.GetFontHeight(const AProjection: TProjection;
  const AParameter: TMapParameter; AFontSize: TReal): TReal;
begin
  if (FFontSize <> 0) then
    Result := FCanvas.Font.Size / FFontSize * AFontSize
  else
    Result := FCanvas.Font.Size;
end;

procedure TMapPainterCanvas.DrawGround(const AProjection: TProjection;
  const AParameter: TMapParameter; const AStyle: TFillStyle);
var
  pd: TPathData;
begin
  // no need BeginScene/EndScene because called outside
  pd := TPathData.Create();
  pd.Clear();

  FCanvas.Fill.Color := GetAlphaColor(AStyle.FillColor);
  FCanvas.Fill.Kind := TBrushKind.Solid;
  //FCanvas.FillEvenOdd := False; // FillNonZero

  pd.MoveTo(TPointF.Create(0, 0));
  pd.LineTo(TPointF.Create(AProjection.Width, 0));
  pd.LineTo(TPointF.Create(AProjection.Width, AProjection.Height));
  pd.LineTo(TPointF.Create(0, AProjection.Height));
  pd.ClosePath();
  FCanvas.FillPath(pd, 1);
  //FCanvas.DrawPath(pd, 1);
end;

procedure TMapPainterCanvas.DrawIcon(AStyle: TIconStyle; ACenterX,
  ACenterY: TReal; AWidth, Aheight: TReal);
begin
  //
end;

procedure TMapPainterCanvas.DrawSymbol(AProjection: TProjection;
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
  pd: TPathData;
  r: TRectF;
begin
  // no need BeginScene/EndScene because called outside
  pd := TPathData.Create();
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

      pd.Clear();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);

      for i := 0 to Length(Polygon.Coords)-1 do
      begin
        Pixel := Polygon.Coords[i];
        if i = 0 then
        begin
          pd.MoveTo(TPointF.Create(X + AProjection.ConvertWidthToPixel(Pixel.X - centerX),
                        Y + AProjection.ConvertWidthToPixel(Pixel.Y - centerY)));
        end
        else
        begin
          pd.MoveTo(TPointF.Create(X + AProjection.ConvertWidthToPixel(Pixel.X - centerX),
                        Y + AProjection.ConvertWidthToPixel(Pixel.Y - centerY)));
        end;
      end;
      pd.ClosePath();
      FCanvas.DrawPath(pd, 1); // dpfFillAndStroke
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

      pd.Clear();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);

      pd.MoveTo(TPointF.Create(xPos, yPos));
      pd.LineTo(TPointF.Create(xPos + width, yPos));
      pd.LineTo(TPointF.Create(xPos + width, yPos + height));
      pd.LineTo(TPointF.Create(xPos, yPos + height));

      pd.ClosePath();
      FCanvas.DrawPath(pd, 1); // dpfFillAndStroke
    end
    else if (Primitive is TCirclePrimitive) then
    begin
      TmpCircle := (Primitive as TCirclePrimitive);
      fillStyle := TmpCircle.FillStyle;
      borderStyle := TmpCircle.BorderStyle;
      radius := AProjection.ConvertWidthToPixel(TmpCircle.Radius);

      //pd.Clear();
      { set fill style }
      DrawFillStyle(AProjection, AParameter, fillStyle, borderStyle);

      r.Left := X;
      r.Top := Y;
      r.Width := radius * 2;
      r.Height := radius * 2;
      FCanvas.DrawEllipse(r, 1);
    end;
  end;
end;

{procedure TMapPainterCanvas.DrawTextPath(AProjection: TProjection;
  AParameter: TMapParameter;
  const AContourLabel: TContourLabel);
var
  TextColor: TMapColor;
  PathData: TPathData;
  r: TRectF;
  tp: TPointF;
  //i: Integer;
  PathPoint: TVertex2D;
  IsFirstPoint: Boolean;
begin
  TextColor := AContourLabel.Style.TextColor;
  FCanvas.Fill.Color := GetAlphaColor(TextColor);
  FCanvas.Font.Size := AContourLabel.Style.Size;

  PathData := TPathData.Create();

  IsFirstPoint := True;
  for PathPoint in AContourLabel.Path do
  begin
    if IsFirstPoint then
    begin
      IsFirstPoint := False;
      PathData.MoveTo(TPointF.Create(PathPoint.X, PathPoint.Y));
    end
    else
    begin
      PathData.LineTo(TPointF.Create(PathPoint.X, PathPoint.Y));
    end;
  end;

  r := PathData.GetBounds;

  FCanvas.TextToPath(PathData, r, AContourLabel.Text, False, TTextAlign.Leading)

end;  }

procedure TMapPainterCanvas.DrawLabel(AProjection: TProjection;
  AParameter: TMapParameter; X, Y: TReal; const AMapLabel: TMapLabel;
  const ALabel: TLabelData);
var
  TmpStyle: TTextStyle;
  TextColor: TMapColor;
  r: TRectF;
begin
  // no need BeginScene/EndScene because called outside
  if (ALabel.Style is TTextStyle) then
  begin
    TmpStyle := (ALabel.Style as TTextStyle);

    if (TmpStyle.Style = tssNormal) then
    begin
      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      //FAgg2D.LineColor := GetAggRgba8(TextColor);
      FCanvas.Fill.Color := GetAlphaColor(TextColor);
      //FCanvas.NoLine();
    end
    else if (TmpStyle.Style = tssEmphasize) then
    begin
      // draw white border around text
      TextColor.Init(1, 1, 1, ALabel.Alpha);
      FCanvas.Stroke.Color := GetAlphaColor(TextColor);

      TextColor := TmpStyle.TextColor;
      //TextColor.A := ALabel.Alpha;
      FCanvas.Fill.Color := GetAlphaColor(TextColor);


      {DrawGlyphVector(X-1, Y + AMapLabel.Height, AMapLabel.glyphs);
      DrawGlyphVector(X+1, Y + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y-1 + AMapLabel.Height, layout.glyphs);
      DrawGlyphVector(X, Y+1 + AMapLabel.Height, layout.glyphs);}

    end;
    FCanvas.Font.Size := ALabel.FontSize;
    //FCanvas.FlipText := True;
    //FCanvas.TextAngle := 0;
    //FCanvas.Text(X, Y, UTF8ToUTF16(AMapLabel.Text));

    r.Left := X;
    r.Top := Y;
    r.Width := AMapLabel.Width + 10;
    r.Height := AMapLabel.Height + 10;
    FCanvas.FillText(r,
      AMapLabel.Text,
      False, 1, [],
      TTextAlign.Leading,
      TTextAlign.Leading);

    {DrawGlyphVector(labelRectangle.x,
                    labelRectangle.y + labelRectangle.height,
                    layout.glyphs); }
  end;
end;

procedure TMapPainterCanvas.DrawPath(const AProjection: TProjection;
  const AParameter: TMapParameter; const AColor: TMapColor; AWidth: TReal;
  const ADash: array of TReal; AStartCap: TLineCapStyle;
  AEndCap: TLineCapStyle; ATransStart, ATransEnd: Integer);
var
  i: Integer;
  Pixel: TVertex2D;
  pd: TPathData;
  da: TDashArray;
  PrevPt, Pt: TPointF;
begin
  // no need BeginScene/EndScene because called outside
  pd := TPathData.Create();
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

  //FCanvas.RemoveAllDashes();
  pd.Clear();
  FCanvas.Stroke.Color := GetAlphaColor(AColor);
  FCanvas.Stroke.Thickness := AWidth;
  FCanvas.Stroke.Join := TStrokeJoin.Round;
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  // dpfStrokeOnly
  FCanvas.Fill.Kind := TBrushKind.None;

  if (AStartCap = capButt) or (AEndCap = capButt) then
    FCanvas.Stroke.Cap := TStrokeCap.Flat
  else if (AStartCap = capSquare) or (AEndCap = capSquare) then
    FCanvas.Stroke.Cap := TStrokeCap.Flat
  else
    FCanvas.Stroke.Cap := TStrokeCap.Round;

  if Length(ADash) <> 0 then
  begin
    i := 0;
    while i < Length(ADash)-1 do
    begin
      AddDash(da, ADash[i] * AWidth, ADash[i+1] * AWidth);
      Inc(i, 2);
    end;
    FCanvas.Stroke.SetCustomDash(da, FCanvas.Stroke.DashOffset);
  end
  else
  begin
    // no dashes
    FCanvas.Stroke.Dash := TStrokeDash.Solid;
  end;

  // on Android DrawPath() works incorrect
  // maybe do DrawLines over first and last path segment?

  {if (ATransEnd - ATransStart) = 1 then
  begin
    // simple line
    Pixel := FTransBuffer.Buffer[ATransStart];
    PrevPt := TPointF.Create(Pixel.X, Pixel.Y);
    Pixel := FTransBuffer.Buffer[ATransEnd];
    Pt := TPointF.Create(Pixel.X, Pixel.Y);
    FCanvas.DrawLine(PrevPt, Pt, 1);
  end
  else}
  begin
    for i := ATransStart to ATransEnd do
    begin
      if (i = ATransStart) then
      begin
        Pixel := FTransBuffer.Buffer[i];
        PrevPt := TPointF.Create(Pixel.X, Pixel.Y);
        //pd.MoveTo(TPointF.Create(Pixel.X, Pixel.Y));
      end
      else
      begin
        Pixel := FTransBuffer.Buffer[i];
        //pd.LineTo(TPointF.Create(Pixel.X, Pixel.Y));
        Pt := TPointF.Create(Pixel.X, Pixel.Y);
        FCanvas.DrawLine(PrevPt, Pt, 1);
        PrevPt := Pt;
      end;
    end;
    //FCanvas.DrawPath(pd, 1);
  end;

  // TODO: End point caps "dots"
end;

procedure TMapPainterCanvas.RegisterRegularLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabels: TLabelDataList;
  const APosition: TVertex2D; AObjectWidth: TReal);
begin
  FLabelLayouter.RegisterLabel(AProjection, AParameter, APosition, ALabels, AObjectWidth);
end;

procedure TMapPainterCanvas.RegisterContourLabel(const AProjection: TProjection;
  const AParameter: TMapParameter; const ALabel: TPathLabelData;
  const ALabelPath: TLabelPath);
begin
  FLabelLayouter.RegisterContourLabel(AProjection, AParameter, ALabel, ALabelPath);
end;

procedure TMapPainterCanvas.DrawLabels(const AProjection: TProjection;
  const AParameter: TMapParameter; const AData: TMapData);
var
  TextLabel: TTextLabel;
  el: TTextLabelElement;
  OverlayElements, TextElements: TTextLabelElementList;
  elementRectangle: TDoubleRectangle;
  ConLabel: TContourLabel;
begin
  FCanvas.BeginScene();
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
  FCanvas.EndScene();
end;

procedure TMapPainterCanvas.BeforeDrawing(const AStyleConfig: TStyleConfig;
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

procedure TMapPainterCanvas.DrawContourSymbol(const AProjection: TProjection;
  const AParameter: TMapParameter; const ASymbol: TMapSymbol; ASpace: TReal;
  ATransStart, ATransEnd: Integer);
begin
  //
end;

procedure TMapPainterCanvas.DrawArea(const AProjection: TProjection;
  const AParameter: TMapParameter; const AAreaData: TAreaData);
var
  i: Integer;
  data: TPolyData;
  Pixel: TVertex2D;
  pd: TPathData;
begin
  // no need BeginScene/EndScene because called outside
  pd := TPathData.Create();
  pd.Clear();
  DrawFillStyle(AProjection, AParameter, AAreaData.FillStyle, AAreaData.BorderStyle);

  {if Length(AAreaData.Clippings) = 0 then
    FCanvas.FillEvenOdd := True
  else
    FCanvas.FillEvenOdd := False; // NonZero (Winding) rule  }

  Pixel := FTransBuffer.Buffer[AAreaData.TransStart];
  pd.MoveTo(TPointF.Create(Pixel.X, Pixel.Y));

  for i := AAreaData.TransStart+1 to AAreaData.TransEnd do
  begin
    Pixel := FTransBuffer.Buffer[i];
    pd.LineTo(TPointF.Create(Pixel.X, Pixel.Y));
  end;
  pd.ClosePath();

  if Length(AAreaData.Clippings) <> 0 then
  begin
    for data in AAreaData.Clippings do
    begin
      pd.MoveTo(TPointF.Create(FTransBuffer.Buffer[data.TransStart].X,
                     FTransBuffer.Buffer[data.TransStart].Y));
      for i := data.TransStart+1 to data.TransEnd do
      begin
        pd.LineTo(TPointF.Create(FTransBuffer.Buffer[i].X,
                      FTransBuffer.Buffer[i].Y));
      end;
      pd.ClosePath();
    end;
  end;

  // dpfFillAndStroke
  FCanvas.FillPath(pd, 1);
  FCanvas.DrawPath(pd, 1);

  // center point
  //Pixel := FTransBuffer.Polylabel(AAreaData.TransStart, AAreaData.TransEnd);
  //FCanvas.Circle(Pixel.X, Pixel.Y, 2);
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

procedure TMapPainterCanvas.OnTextLayoutHandler(out ALabel: TMapLabel;
  AProjection: TProjection; AParameter: TMapParameter; const AText: string;
  AFontSize, AObjectWidth: TReal; AEnableWrapping: Boolean;
  AContourLabel: Boolean);
var
  x, y, w, h, cw: TReal;
  i: Integer;
  //ws: WideString;
  ws: string;
begin
  //Assert(False, 'not needed');

  //SetFont(AProjection, AParameter, AFontSize);
  FCanvas.Font.Size := AFontSize;

  {if AContourLabel then
    agg::glyph_ren_outline
  else
    agg::glyph_ren_native_gray8;}

  //fontCacheManager->reset_last_glyph();
  x := 0;
  y := 0;
  w := 0;
  h := FCanvas.Font.Size;

  //ws := UTF8ToUTF16(AText);
  ws := AText;
  if AContourLabel then
  begin
    SetLength(ALabel.Glyphs, Length(ws));
    for i := 1 to Length(ws) do
    begin
      // get single character glyph bounds
      //const agg::glyph_cache *glyph = fontCacheManager->glyph(i);
      //fontCacheManager->add_kerning(&x, &y);
      //label.glyphs.emplace_back(std::move(MapPainterAgg::NativeGlyph{x, y, glyph}));
      cw := FCanvas.TextWidth(ws[i]);

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
    w := FCanvas.TextWidth(ws);
  end;

  //ALabel.pLabel := Addr(FNativeLabel);
  ALabel.pLabel := nil;
  ALabel.Height := h;
  ALabel.Width := w;
  ALabel.FontSize := AFontSize;
  ALabel.Text := AText;
end;

constructor TMapPainterCanvas.Create(const AStyleConfig: TStyleConfig; ACanvas: TCanvas);
begin
  inherited Create(AStyleConfig);

  FCanvas := ACanvas;
  FLabelLayouter := TLabelLayouter.Create();
  //FLabelLayouter.OnGlyphBoundingBox := @OnGlyphBoundingBoxHandler;
  FLabelLayouter.OnTextLayout := OnTextLayoutHandler;

  FMutex := TCriticalSection.Create();
  FFontSize := 0;
end;

destructor TMapPainterCanvas.Destroy;
begin
  FreeAndNil(FMutex);
  FreeAndNil(FLabelLayouter);
  {if not FIsAlienAgg then
  begin
    FreeAndNil(FCanvas);
  end; }
  FCanvas := nil;
  inherited Destroy;
end;

function TMapPainterCanvas.DrawMap(AProjection: TProjection;
  AParameter: TMapParameter; AData: TMapData): Boolean;
begin
  //FMutex.Enter();
  try
    FCanvas.BeginScene();
    //FCanvas.ClearAll(255, 255, 255);
    FCanvas.Clear($FFFFFFFF);

    if FFontSize = 0 then
      SetFont(AProjection, AParameter, AParameter.FontSize);

    Result := inherited DrawMap(AProjection, AParameter, AData);
    //Sleep(1);
  except
    // FMutex.Leave();
    Result := False;
  end;
  FCanvas.EndScene();
end;


end.

