(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2019  Sergey Bodrov

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
unit OsMapManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OsMapPainter, OsMapProjection, OsMapStyleConfig, OsMapTypes, OsMapObjTypes,
  OsMapParameters, OsMapStyles, OsMapObjects, OsMapGeocoder;

type
  TMapManager = class;

  { TMapRenderThread }

  TMapRenderThread = class(TThread)
  protected
    procedure Execute; override;
  public
    // assigned
    MapManager: TMapManager;
  end;

  { TMapManager }

  TMapManager = class(TComponent)
  private
    // created
    FMapData: TMapData;
    FMapProjection: TMercatorProjection;
    FMapParameter: TMapParameter;
    FMapTypeConfig: TTypeConfig;
    FMapStyleConfig: TStyleConfig;
    FMapRenderThread: TMapRenderThread;
    FMapGeocoder: TMapGeocoder;
    // assigned
    FMapPainter: TMapPainter;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure InitTypes();

    procedure Render();

    property MapData: TMapData read FMapData;
    property MapProjection: TMercatorProjection read FMapProjection;
    property MapParameter: TMapParameter read FMapParameter;
    property MapTypeConfig: TTypeConfig read FMapTypeConfig;
    property MapStyleConfig: TStyleConfig read FMapStyleConfig;
    property MapGeocoder: TMapGeocoder read FMapGeocoder;

    property MapPainter: TMapPainter read FMapPainter write FMapPainter;
  end;

implementation

uses OsMapUtils;

{ TMapManager }

procedure TMapManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FMapProjection := TMercatorProjection.Create();
  FMapParameter := TMapParameter.Create();
  FMapTypeConfig := TTypeConfig.Create();
  FMapStyleConfig := TStyleConfig.Create(FMapTypeConfig);
  FMapData := TMapData.Create();
  FMapRenderThread := TMapRenderThread.Create(True);
  FMapGeocoder := TMapGeocoder.Create();
  FMapGeocoder.MapData := FMapData;
  FMapGeocoder.ValStorage := GlobalFeatureValueStorage();

  //FMapPainter := TMapPainterAgg.Create(FMapStyleConfig);

  InitTypes();
end;

procedure TMapManager.BeforeDestruction;
begin
  FreeAndNil(FMapGeocoder);
  FreeAndNil(FMapRenderThread);
  FreeAndNil(FMapData);
  FreeAndNil(FMapStyleConfig);
  FreeAndNil(FMapTypeConfig);
  FreeAndNil(FMapParameter);
  FreeAndNil(FMapProjection);
  inherited BeforeDestruction;
end;

procedure TMapManager.InitTypes();
var
  TmpType: TTypeInfo;
  TmpStyle: TStyle;
begin
  // ======
  TmpType := TTypeInfo.Create('highway_trunk');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(250, 177, 153, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 2;
  (TmpStyle as TLineStyle).Priority := 4;
  // lanes feature must be = 2+
  //(TmpStyle as TLineStyle).OffsetRel := lorLaneDivider;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_primary');
  TmpType.CanBeWay := True;
  TmpType.AddFeature(MapTypeConfig.GetFeature(ftName));
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(251, 213, 164, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 2;
  (TmpStyle as TLineStyle).Priority := 3;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  TmpStyle := TPathTextStyle.Create();
  with (TmpStyle as TPathTextStyle) do
  begin
    Name := TmpType.TypeName + '_PathText';
    Size := 4;
    FeatureType := ftName;
    //ScaleAndFadeMag.Level := 15;
    //IsAutoSize := True;
    TextColor.InitFromBytes(180, 180, 180, 255);
    Priority := 3;
  end;
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_secondary');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(242, 242, 187, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 1;
  (TmpStyle as TLineStyle).Priority := 1;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_service');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(254, 254, 254, 255);
  (TmpStyle as TLineStyle).Width := 1;
  (TmpStyle as TLineStyle).DisplayWidth := 1;
  (TmpStyle as TLineStyle).Priority := 0;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_residential');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(252, 252, 252, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 0;
  (TmpStyle as TLineStyle).Priority := 0;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_road');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(220, 220, 220, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 1;
  (TmpStyle as TLineStyle).Priority := 1;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_path');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(248, 146, 134, 255);
  (TmpStyle as TLineStyle).Width := 1;
  (TmpStyle as TLineStyle).DisplayWidth := 1;
  (TmpStyle as TLineStyle).AddDash(2, 2);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('railway_rail');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(111, 111, 111, 255);
  (TmpStyle as TLineStyle).GapColor.InitFromBytes(253, 253, 253, 255);
  (TmpStyle as TLineStyle).Width := 1;
  (TmpStyle as TLineStyle).DisplayWidth := 0;
  (TmpStyle as TLineStyle).AddDash(4, 4);
  (TmpStyle as TLineStyle).Priority := 3;
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('railway_tram');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(111, 111, 111, 255);
  (TmpStyle as TLineStyle).Width := 1;
  (TmpStyle as TLineStyle).DisplayWidth := 0;
  (TmpStyle as TLineStyle).Priority := 3;
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('highway_construction');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(251, 213, 164, 255);
  (TmpStyle as TLineStyle).GapColor.InitFromBytes(253, 253, 253, 255);
  (TmpStyle as TLineStyle).Width := 2;
  (TmpStyle as TLineStyle).DisplayWidth := 1;
  (TmpStyle as TLineStyle).AddDash(2, 2);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('water_way');
  TmpType.CanBeWay := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TLineStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Line';
  (TmpStyle as TLineStyle).LineColor.InitFromBytes(169, 210, 222, 255);
  (TmpStyle as TLineStyle).Width := 1;
  (TmpStyle as TLineStyle).DisplayWidth := 2;
  (TmpStyle as TLineStyle).Priority := 1;
  //(TmpStyle as TLineStyle).
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ====== 'building'
  TmpType := TTypeInfo.Create('building');
  TmpType.CanBeArea := True;
  TmpType.AddFeature(MapTypeConfig.GetFeature(ftName));
  TmpType.AddFeature(MapTypeConfig.GetFeature(ftAddress));
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(214, 208, 199, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  TmpStyle := TBorderStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Border';
  (TmpStyle as TBorderStyle).Width := 0.2;
  //(TmpStyle as TBorderStyle).Color.InitFromBytes(180, 180, 180, 255);
  (TmpStyle as TBorderStyle).Color.InitFromBytes(100, 100, 100, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  TmpStyle := TTextStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Text';
  (TmpStyle as TTextStyle).FeatureType := ftName;
  (TmpStyle as TTextStyle).Size := 5;
  (TmpStyle as TTextStyle).ScaleAndFadeMagLevel := 15;
  (TmpStyle as TTextStyle).IsAutoSize := True;
  (TmpStyle as TTextStyle).TextColor.InitFromBytes(180, 180, 180, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('building_apartments');
  TmpType.CanBeArea := True;
  TmpType.AddFeature(MapTypeConfig.GetFeature(ftName));
  TmpType.AddFeature(MapTypeConfig.GetFeature(ftAddress));
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(242, 213, 182, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('building_industrial');
  TmpType.CanBeArea := True;
  MapTypeConfig.RegisterType(TmpType);

  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(198, 198, 180, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  TmpStyle := TBorderStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Border';
  (TmpStyle as TBorderStyle).Width := 0.5;
  (TmpStyle as TBorderStyle).Color.InitFromBytes(190, 190, 178, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('grass');
  TmpType.CanBeArea := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(204, 234, 176, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('farmland');
  TmpType.CanBeArea := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(237, 239, 213, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('forest');
  TmpType.CanBeArea := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(172, 208, 158, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);

  // ======
  TmpType := TTypeInfo.Create('water');
  TmpType.CanBeArea := True;
  MapTypeConfig.RegisterType(TmpType);
  TmpStyle := TFillStyle.Create();
  TmpStyle.Name := TmpType.TypeName + '_Fill';
  (TmpStyle as TFillStyle).FillColor.InitFromBytes(169, 210, 222, 255);
  MapStyleConfig.AddStyle(TmpType, TmpStyle);


end;

procedure TMapManager.Render();
begin
  if FMapRenderThread.Suspended then
  begin
    FMapRenderThread.MapManager := Self;
    FMapRenderThread.Suspended := False;
  end;
end;

{ TMapRenderThread }

procedure TMapRenderThread.Execute;
begin
  while not Terminated do
  begin
    MapManager.MapPainter.DrawMap(MapManager.MapProjection,
      MapManager.MapParameter, MapManager.MapData);

    Suspended := True;
  end;
end;

end.

