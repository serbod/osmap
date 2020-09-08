unit TileServerMainForm;

{$define AggPainter}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lNetComponents, OsMapPainter, OsMapProjection, OsMapStyleConfig,
  OsMapObjTypes, OsMapManager, OsMapFormatMp, OsMapTypes, OsMapGeometry,
  RFUtils
  {$ifdef AggPainter}
  , Agg2DGraphics, Agg2D, Agg2DControl, OsMapPainterAgg, fphttpserver
  {$endif}
  ;


type

  { TFormMain }

  TFormMain = class(TForm)
    FPHttpServer1: TFPHttpServer;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPHttpServer1Request(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  private
    FMapProjection: TTileProjection;
    FMapManager: TMapManager;

    FBmp: TBitmap;
    FImg: TAgg2DImage;
    FPng: TPortableNetworkGraphic;

    FMemStream: TMemoryStream;
    FAgg2D: TAgg2DGraphics;
    FMapPainter: TMapPainterAgg;
  public
    procedure LoadMaps();

    procedure Test();

    function RenderTile(Z, X, Y: Integer; AStream: TStream): Boolean;

    property MapManager: TMapManager read FMapManager;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FMapProjection := TTileProjection.Create();

  FMapManager := TMapManager.Create(Self);
  FMapManager.MapProjection := FMapProjection;
  FMapManager.InitTypesFromIni('style.ini');

  FBmp := TBitmap.Create();
  FBmp.PixelFormat := pf32bit;
  FBmp.Width := 256;
  FBmp.Height := 256;
  FBmp.Clear;

  FPng := TPortableNetworkGraphic.Create();
  FMemStream := TMemoryStream.Create();

  FAgg2D := TAgg2DGraphics.Create();
  FAgg2D.Attach(FBmp);

  //{$ifdef AggPainter}
  FMapPainter := TMapPainterAgg.Create(FMapManager.MapStyleConfig);
  //FMapPainter.Agg2D := Agg2DControl1.Agg2D;
  FMapPainter.Agg2D := FAgg2D;
  //{$endif}

  FMapManager.MapPainter := FMapPainter;


  LoadMaps();

  //Test();

  FPHttpServer1.Port := 5080;
  FPHttpServer1.Active := True;
  //FPHttpServer1.Active := ;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FMapManager.MapPainter := nil;
  FreeAndNil(FMapPainter);
  FreeAndNil(FAgg2D);
  FreeAndNil(FMemStream);
  FreeAndNil(FPng);
  FreeAndNil(FBmp);
  FreeAndNil(FMapManager);
  FreeAndNil(FMapProjection);
end;

procedure TFormMain.FPHttpServer1Request(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  X, Y, Z: Integer;
  ss, s: string;
begin
  ss := ARequest.URL;
  s := ExtractFirstWord(ss, '/'); // none
  s := ExtractFirstWord(ss, '/'); // zoom level
  Z := StrToIntDef(s, 0);
  s := ExtractFirstWord(ss, '/'); // X
  X := StrToIntDef(s, 0);
  s := ExtractFirstWord(ss, '.'); // Y
  Y := StrToIntDef(s, 0);

  if (Z > 0) and (Z < 20)
  and (X > 0) and (Y > 0) then
  begin
    RenderTile(Z, X, Y, FMemStream);

    AResponse.URL := ARequest.URL;
    AResponse.ContentStream := FMemStream;
    AResponse.ContentType := 'image/png';
    AResponse.ContentLength := AResponse.ContentStream.Size;
    AResponse.SendContent();
  end
  else
  begin
    AResponse.Code := 404;
  end;
end;

procedure TFormMain.LoadMaps();
begin
  ImportFromMpFile(FMapManager, 'minsk.mp');   // 26212 ways
  ImportFromMpFile(FMapManager, 'minsk_obl.mp');
  ImportFromMpFile(FMapManager, 'brest.mp');
  ImportFromMpFile(FMapManager, 'gomel.mp');
  ImportFromMpFile(FMapManager, 'grodno.mp');
  ImportFromMpFile(FMapManager, 'mogilev.mp');
  ImportFromMpFile(FMapManager, 'vitebsk.mp');

  MapManager.SortAreas();

end;

procedure TFormMain.Test();
begin
  RenderTile(16, 37780, 21069, FMemStream);
  Image1.Picture.Assign(FPng);
end;

function TFormMain.RenderTile(Z, X, Y: Integer; AStream: TStream): Boolean;
var
  TileXY: TOSMTileXY;

begin
  TileXY.Init(X, Y);
  FMapProjection.Setup(TileXY, Magnification(Byte(Z)), 96, 256, 256);
  //FMapManager.MapProjection := FMapProjection;
  FMapManager.Render();

  FPng.Assign(FBmp);
  //FPng.LoadFromDevice(Agg2DControl1.Canvas.Handle);
  //Assign(Agg2DControl1.Canvas);

  AStream.Size := 0;
  FPng.SaveToStream(AStream);
  Result := True;
end;

end.

