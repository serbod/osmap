unit PainterBgraForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  OsMapTypes, OsMapGeometry, OsMapManager,
  BGRABitmap, BGRABitmapTypes, Types, Math;

type

  { TFormPainterBGRA }

  TFormPainterBGRA = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private

  public
    Bitmap: TBGRABitmap;
  end;

var
  FormPainterBGRA: TFormPainterBGRA;

implementation

{$R *.lfm}

uses MainForm;

{ TFormPainterBGRA }

procedure TFormPainterBGRA.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssShift in Shift then
  begin
    Form1.ShowObjectsAt(Form1.fseLon.Value, Form1.fseLat.Value);
  end
  else
    Form1.btnShowClick(nil);
end;

procedure TFormPainterBGRA.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Coord: TGeoPoint;
begin
  if Form1.MapManager.MapProjection.IsValid() then
  begin
    Form1.MapManager.MapProjection.PixelToGeo(X, Y, Coord);
    Form1.fseLat.Value := Coord.Lat;
    Form1.fseLon.Value := Coord.Lon;
  end;
end;

procedure TFormPainterBGRA.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    Form1.seMagLevel.Value := Form1.seMagLevel.Value + 1
  else
    Form1.seMagLevel.Value := Form1.seMagLevel.Value - 1;
end;

procedure TFormPainterBGRA.FormPaint(Sender: TObject);
begin
  if Assigned(Bitmap) then
    Bitmap.Draw(Canvas, 0, 0, True);
end;

procedure TFormPainterBGRA.FormResize(Sender: TObject);
begin
  if Assigned(Bitmap) then
    Bitmap.SetSize(Width, Height);
end;

procedure TFormPainterBGRA.MenuItem1Click(Sender: TObject);
var
  bmp: TBGRABitmap;
  cPen, cFill: TBGRAPixel;
  w: Single;
  points: array of TPointF;
begin
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, ColorToBGRA(clBtnFace));
  cPen := ColorToBGRA(clWindowText);
  cFill := ColorToBGRA(clWindow);
  w := 4; // line width

  SetLength(points, 5);
  points[0].SetLocation(80, 80);
  points[1].SetLocation(180, 80);
  points[2].SetLocation(180, 180);
  points[3].SetLocation(100, 200);
  points[4].SetLocation(80, 180);

  bmp.PenStyle := psDashDot;

  bmp.DrawPolygonAntialias(points, cPen, w, cFill);

  bmp.Draw(Canvas, 0, 0, True);
  bmp.Free();
end;

procedure TFormPainterBGRA.MenuItem2Click(Sender: TObject);
var
  bmp: TBGRABitmap;
  cPen, cFill: TBGRAPixel;
  i, ang: Integer;
  w, deg: Single;
  sz: TSize;
  ab: TAffineBox;
  pt, pt2, pt3: TPointF;
begin
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, ColorToBGRA(clBtnFace));
  cPen := ColorToBGRA(clWindowText);
  cFill := ColorToBGRA(clWindow);
  w := 4; // line width
  bmp.FontHeight := 20;
  bmp.FontAntialias := True;

  // -----
  ang := 0;
  pt.SetLocation(100, 200);
  bmp.DrawPixel(Round(pt.x), Round(pt.y), cPen);
  for i := 0 to 7 do
  begin
    sz := bmp.TextSize('Testing');
    deg := DegToRad(ang / 10);

    pt3.x := (sz.cx / 2);
    pt3.y := (-sz.cy / 2);

    pt2.x := pt.x - pt3.x - (pt3.x * cos(deg)) - (pt3.y * sin(deg));
    pt2.y := pt.y - sz.cy + (pt3.x * sin(deg)) + (pt3.y * cos(deg));

    bmp.FontOrientation := ang;
    bmp.TextOut(pt2.x, pt2.y, 'Testing', cPen, taLeftJustify);
    //bmp.TextOutAngle(pt.x, pt.y, ang, 'Testing', cPen, taLeftJustify);
    //ab := bmp.TextAffineBox('Testing');
    //bmp.RectangleAntialias(pt.x, pt.y, pt.x + sz.cx, pt.y + sz.cy, cPen, 2);
    Inc(ang, 450);
  end;

  // -----
  {ang := 0;
  bmp.DrawPixel(300, 200, cPen);
  for i := 0 to 7 do
  begin
    bmp.TextOutAngle(300, 200, ang, 'Testing', cPen, taCenter);
    Inc(ang, 450);
  end; }

  bmp.Draw(Canvas, 0, 0, True);
  bmp.Free();
end;

end.

