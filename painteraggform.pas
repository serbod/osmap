unit PainterAggForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Math,
  Agg2D, Agg2DControl,
  OsMapTypes, OsMapGeometry, OsMapManager, Types;

type

  { TFormPainterAgg }

  TFormPainterAgg = class(TForm)
    Agg2DControl1: TAgg2DControl;
    miTest1: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure Agg2DControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Agg2DControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Agg2DControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure miTest1Click(Sender: TObject);
  private

  public
    procedure AggTest1();
  end;

var
  FormPainterAgg: TFormPainterAgg;

implementation

{$R *.lfm}

uses
  MainForm;

{ TFormPainterAgg }

procedure TFormPainterAgg.Agg2DControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssShift in Shift then
  begin
    Form1.ShowObjectsAt(Form1.fseLon.Value, Form1.fseLat.Value);
  end
  else
    Form1.btnShowClick(nil);
end;

procedure TFormPainterAgg.Agg2DControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
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

procedure TFormPainterAgg.Agg2DControl1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Form1.FormMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TFormPainterAgg.FormCreate(Sender: TObject);
begin
  Agg2DControl1.DoubleBuffered := True;
end;

procedure TFormPainterAgg.miTest1Click(Sender: TObject);
begin
  AggTest1();
end;

procedure TFormPainterAgg.AggTest1();
var
  vg: TAgg2D;
  x, y: TReal;
begin
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

  Agg2DControl1.Invalidate();
end;

end.

