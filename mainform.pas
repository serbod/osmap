unit MainForm;

{$mode objfpc}{$H+}

{$define AggPainter}
{//$define BGRAPainter}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus, Types, OsMapPainter, OsMapProjection,
  OsMapStyleConfig, OsMapObjTypes, OsMapManager, OsMapFormatMp, OsMapTypes,
  OsMapGeometry
  {$ifdef AggPainter}
  , OsMapPainterAgg, PainterAggForm
  {$endif}
  {$ifdef BGRAPainter}
  , OsMapPainterBGRA, PainterBgraForm
  {$endif}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnShow: TButton;
    btnTest: TButton;
    edSearch: TEdit;
    fseLat: TFloatSpinEdit;
    fseLon: TFloatSpinEdit;
    lbState: TLabel;
    miLoadMapFiles: TMenuItem;
    miSaveMapFiles: TMenuItem;
    panBottom: TPanel;
    pmMain: TPopupMenu;
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
    procedure miLoadMapFilesClick(Sender: TObject);
    procedure miSaveMapFilesClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
  private
    FMapProjection: TMercatorProjection;
    FMapManager: TMapManager;

    {$ifdef AggPainter}
    FMapPainter: TMapPainterAgg;
    FPainterForm: TFormPainterAgg;
    {$endif}
    {$ifdef BGRAPainter}
    FMapPainter: TMapPainterBGRA;
    FPainterForm: TFormPainterBGRA;
    {$endif}

    Fsl: TStringList;
  public
    procedure ShowObjectsAt(ALon, ALat: Real);

    property MapManager: TMapManager read FMapManager;
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
  ImportFromMpFile(FMapManager, 'minsk.mp');   // 26212 ways
  ImportFromMpFile(FMapManager, 'minsk_obl.mp');
  ImportFromMpFile(FMapManager, 'brest.mp');
  ImportFromMpFile(FMapManager, 'gomel.mp');
  ImportFromMpFile(FMapManager, 'grodno.mp');
  ImportFromMpFile(FMapManager, 'mogilev.mp');
  ImportFromMpFile(FMapManager, 'vitebsk.mp');

  {Bitmap := Image1.Picture.Bitmap;
  Bitmap.PixelFormat := TPixelFormat.pf32bit;

  FMapPainter.Agg2D.Attach(Bitmap.ScanLine[Bitmap.Height - 1], Bitmap.Width, Bitmap.Height,
    -Bitmap.Width * 4); }

  btnStart.Enabled := False;
  btnShow.Enabled := True;
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  pt: TGeoPoint;
begin
  if FMapManager.MapGeocoder.GetStreetStart(edSearch.Text, pt) then
  begin
    fseLat.Value := pt.Lat;
    fseLon.Value := pt.Lon;
    btnShowClick(nil);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef AggPainter}
  FPainterForm := TFormPainterAgg.Create(Self);
  {$endif}
  {$ifdef BGRAPainter}
  FPainterForm := TFormPainterBGRA.Create(Self);
  {$endif}
  FPainterForm.Parent := Self;
  FPainterForm.Align := alClient;
  FPainterForm.BorderStyle := bsNone;
  FPainterForm.Visible := True;

  FMapProjection := TMercatorProjection.Create();

  FMapManager := TMapManager.Create(Self);
  FMapManager.MapProjection := FMapProjection;
  FMapManager.InitTypesFromIni('style.ini');

  {$ifdef AggPainter}
  FMapPainter := TMapPainterAgg.Create(FMapManager.MapStyleConfig);
  FMapPainter.Agg2D := FPainterForm.Agg2DControl1.Agg2D;
  {$endif}
  {$ifdef BGRAPainter}
  FMapPainter := TMapPainterBGRA.Create(FMapManager.MapStyleConfig, FPainterForm.Canvas);
  FPainterForm.Bitmap := FMapPainter.Bitmap;
  {$endif}

  FMapManager.MapPainter := FMapPainter;


  Fsl := TStringList.Create();
  //btnStartClick(nil);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    seMagLevel.Value := seMagLevel.Value + 1
  else
    seMagLevel.Value := seMagLevel.Value - 1;
end;

procedure TForm1.miLoadMapFilesClick(Sender: TObject);
begin
  MapManager.LoadWaysFromFile(MapManager.WaysFileName);
  MapManager.LoadAreasFromFile(MapManager.AreasFileName);
  btnStart.Enabled := False;
  btnShow.Enabled := True;
  //ShowMessage('Loaded!');
end;

procedure TForm1.miSaveMapFilesClick(Sender: TObject);
begin
  MapManager.SaveWaysToFile(MapManager.WaysFileName);
  MapManager.SaveAreasToFile(MapManager.AreasFileName);
  ShowMessage('Saved!');
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
begin
  lbState.Caption := FMapManager.MapPainter.GetStateStr();
  {$ifdef AggPainter}
  FPainterForm.Agg2DControl1.Invalidate();
  {$endif}
  {$ifdef BGRAPainter}
  {$endif}
end;

procedure TForm1.ShowObjectsAt(ALon, ALat: Real);
var
  i: Integer;
  mpt: TGeoPoint;
  s: string;
begin
  FSl.Clear();
  mpt.Init(ALat, ALon);
  for i := 0 to FMapManager.MapData.AreaList.Count-1 do
  begin
    if FMapManager.MapData.AreaList.Items[i].Rings[0].IsPointInside(mpt) then
    begin
      Fsl.Add(FMapManager.MapData.AreaList.Items[i].GetType().TypeName);
    end;
  end;

  s := FSl.Text;
  ShowMessage(s);
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
  //{$ifdef AggPainter}
  FMapManager.MapProjection.Setup(Coord, 0, Magnification(MagLev), Screen.PixelsPerInch,
    FPainterForm.Width, FPainterForm.Height);
  //{$endif}

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
  if ssShift in Shift then
  begin
    ShowObjectsAt(fseLon.Value, fseLat.Value);
  end
  else
    btnShowClick(nil);
end;

end.

