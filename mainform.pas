unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Agg2DControl, OsMapPainter, OsMapPainterAgg, OsMapProjection,
  OsMapStyleConfig, OsMapObjTypes, OsMapManager, OsMapFormatMp, OsMapTypes,
  OsMapGeometry, Types, Agg2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnShow: TButton;
    Agg2DControl1: TAgg2DControl;
    btnTest: TButton;
    edSearch: TEdit;
    fseLat: TFloatSpinEdit;
    fseLon: TFloatSpinEdit;
    lbState: TLabel;
    panBottom: TPanel;
    seMagLevel: TSpinEdit;
    Timer100ms: TTimer;
    procedure Agg2DControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Agg2DControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnShowClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Timer100msTimer(Sender: TObject);
  private
    FMapManager: TMapManager;
    FMapPainter: TMapPainterAgg;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math;

{ TForm1 }

procedure TForm1.btnStartClick(Sender: TObject);
var
  Bitmap: TBitmap;

begin
  ImportFromMpFile(FMapManager, 'minsk.mp');
  ImportFromMpFile(FMapManager, 'minsk_obl.mp');

  {Bitmap := Image1.Picture.Bitmap;
  Bitmap.PixelFormat := TPixelFormat.pf32bit;

  FMapPainter.Agg2D.Attach(Bitmap.ScanLine[Bitmap.Height - 1], Bitmap.Width, Bitmap.Height,
    -Bitmap.Width * 4); }

  btnStart.Enabled := False;
  btnShow.Enabled := True;
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  vg: TAgg2D;
  x, y: Double;
  pt: TGeoPoint;
begin
  {
  vg := Agg2DControl1.Agg2D;

  // init font
  vg.Font('Tahoma', 40, True, False, fcVector, 0);
  vg.FlipText := True;

  vg.ClearAll(255, 255, 255);
  //vg.NoFill();
  vg.FillColor.Initialize($FF, $00, $00, 255);
  vg.LineColor.Initialize($FF, $FF, $FF, 255);

  x := 40;
  y := 70;
  // letters
  vg.Text(x, y, 'T');

  x := x + 40;
  vg.TextAngle := degtorad(10);
  vg.Text(x, y, 'E');

  x := x + 40;
  vg.TextAngle := degtorad(20);
  vg.Text(x, y, 'S');

  x := x + 40;
  vg.TextAngle := degtorad(30);
  vg.Text(x, y, 'T');

  Agg2DControl1.Invalidate();  }


  if FMapManager.MapGeocoder.GetStreetStart(edSearch.Text, pt) then
  begin
    fseLat.Value := pt.Lat;
    fseLon.Value := pt.Lon;
    btnShowClick(nil);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMapManager := TMapManager.Create(Self);
  FMapPainter := TMapPainterAgg.Create(FMapManager.MapStyleConfig);
  FMapPainter.Agg2D := Agg2DControl1.Agg2D;
  FMapManager.MapPainter := FMapPainter;

  Agg2DControl1.DoubleBuffered := True;
  btnStartClick(nil);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    seMagLevel.Value := seMagLevel.Value + 1
  else
    seMagLevel.Value := seMagLevel.Value - 1;
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  lbState.Caption := FMapManager.MapPainter.GetStateStr();
  Agg2DControl1.Invalidate();
end;

procedure TForm1.btnShowClick(Sender: TObject);
var
  Coord: TGeoPoint;
  MagLev: Byte;
begin
  Coord.Lat := fseLat.Value; // 53.89579
  Coord.Lon := fseLon.Value; // 27.54783
  //Maglev := MAG_LEVEL_DETAIL;
  Maglev := Byte(seMagLevel.Value);
  FMapManager.MapProjection.Setup(Coord, 0, Magnification(MagLev), Screen.PixelsPerInch,
    Agg2DControl1.Width, Agg2DControl1.Height);

  FMapManager.Render();

  //Agg2DControl1.Invalidate();
end;

procedure TForm1.Agg2DControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Coord: TGeoPoint;
begin
  if FMapManager.MapProjection.IsValid() then
  begin
    FMapManager.MapProjection.PixelToGeo(X, Y, Coord);
    fseLat.Value := Coord.Lat;
    fseLon.Value := Coord.Lon;
  end;
end;

procedure TForm1.Agg2DControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  btnShowClick(nil);
end;

end.

