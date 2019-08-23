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

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Types, Classes, SysUtils, SyncObjs,
  OsMapPainter, OsMapProjection, OsMapStyleConfig, OsMapTypes, OsMapObjTypes,
  OsMapParameters, OsMapStyles, OsMapObjects, OsMapGeocoder, IniFiles;

type
  TMapManager = class;
  {$ifdef FPC}
  TMapEvent = TSimpleEvent;
  {$else}
  TMapEvent = TEvent;
  {$endif}

  { TMapRenderThread }

  TMapRenderThread = class(TThread)
  protected
    FEvent: TMapEvent;
    procedure Execute; override;
  public
    // assigned
    MapManager: TMapManager;
    procedure Run();
  end;

  { TMapManager }

  TMapManager = class(TComponent)
  private
    // created
    FMapData: TMapData;
    FMapParameter: TMapParameter;
    FMapTypeConfig: TTypeConfig;
    FMapStyleConfig: TStyleConfig;
    FMapRenderThread: TMapRenderThread;
    FMapGeocoder: TMapGeocoder;
    // assigned
    FMapProjection: TMercatorProjection;
    FMapPainter: TMapPainter;

    FOnAfterRender: TNotifyEvent;
  public
    Busy: Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure InitTypes();
    procedure InitTypesFromIni(AFileName: string);

    procedure Render();

    // shared
    property MapData: TMapData read FMapData;
    property MapParameter: TMapParameter read FMapParameter;
    property MapTypeConfig: TTypeConfig read FMapTypeConfig;
    property MapStyleConfig: TStyleConfig read FMapStyleConfig;
    property MapGeocoder: TMapGeocoder read FMapGeocoder;

    property MapProjection: TMercatorProjection read FMapProjection write FMapProjection;
    property MapPainter: TMapPainter read FMapPainter write FMapPainter;

    property OnAfterRender: TNotifyEvent read FOnAfterRender write FOnAfterRender;
  end;

implementation

uses OsMapUtils;

{ TMapManager }

procedure TMapManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FormatSettings.DecimalSeparator := '.';
  FMapParameter := TMapParameter.Create();
  FMapTypeConfig := TTypeConfig.Create();
  FMapStyleConfig := TStyleConfig.Create(FMapTypeConfig);
  FMapData := TMapData.Create();
  FMapRenderThread := TMapRenderThread.Create(True);
  FMapGeocoder := TMapGeocoder.Create();
  FMapGeocoder.MapData := FMapData;
  FMapGeocoder.ValStorage := GlobalFeatureValueStorage();

  //FMapPainter := TMapPainterAgg.Create(FMapStyleConfig);

  //InitTypes();
end;

procedure TMapManager.BeforeDestruction;
begin
  FreeAndNil(FMapGeocoder);
  FreeAndNil(FMapRenderThread);
  FreeAndNil(FMapData);
  FreeAndNil(FMapStyleConfig);
  FreeAndNil(FMapTypeConfig);
  FreeAndNil(FMapParameter);
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

procedure TMapManager.InitTypesFromIni(AFileName: string);
var
  ini: TMemIniFile;

procedure _ReadColor(ASect, AName: string; var AMapColor: TMapColor);
var
  i: Integer;
  RGBA: array [0..3] of Byte;
  s, ss: string;
begin
  ss := ini.ReadString(ASect, AName, '');
  if ss = '' then
    Exit;

  if Copy(ss, 1, 1) = '#' then
    AMapColor.FromHexString(ss)
  else
  begin
    for i := 0 to 3 do
    begin
      s := Trim(Fetch(ss, ','));
      RGBA[i] := Byte(StrToIntDef(s, 0));
    end;
    AMapColor.InitFromBytes(RGBA[0], RGBA[1], RGBA[2], RGBA[3]);
  end;
end;

procedure _ReadDash(ASect, AName: string; AStyle: TLineStyle);
var
  n1, n2: Double;
  s, ss: string;
begin
  ss := ini.ReadString(ASect, AName, '');
  repeat
    s := Trim(Fetch(ss, ','));
    n1 := StrToFloatDef(s, 0);
    s := Trim(Fetch(ss, ','));
    n2 := StrToFloatDef(s, 0);

    if (n1 = 0) or (n2 = 0) then
      Break;

    AStyle.AddDash(n1, n2);
  until s = '';
end;

var
  slSections: TStringList;
  i: Integer;
  s, sSect, sValue: string;
  TmpType: TTypeInfo;
  TmpStyle: TStyle;
  TmpFeature: TFeature;
  ResStream: TResourceStream;
begin
  ResStream := nil;
  if Pos('_', AFileName) = 1 then
  begin
    // from resource
    ResStream := TResourceStream.Create(HInstance, AFileName, RT_RCDATA);
    ini := TMemIniFile.Create(ResStream);
  end
  else
  begin
    if not FileExists(AFileName) then
      Exit;
    ini := TMemIniFile.Create(AFileName);
  end;

  slSections := TStringList.Create();
  try
    ini.ReadSections(slSections);
    for i := 0 to slSections.Count-1 do
    begin
      sSect := slSections[i];
      // get type by name
      TmpType := MapTypeConfig.GetTypeInfo(sSect);
      if not Assigned(TmpType) or TmpType.IsIgnore then
      begin
        TmpType := TTypeInfo.Create(sSect);
        TmpType.CanBeArea := ini.ReadBool(sSect, 'CanBeArea', False);
        TmpType.CanBeWay := ini.ReadBool(sSect, 'CanBeWay', False);
        TmpType.CanBeNode := ini.ReadBool(sSect, 'CanBeNode', False);
        TmpType.CanBeRelation := ini.ReadBool(sSect, 'CanBeRelation', False);
        sValue := ini.ReadString(sSect, 'Features', '');
        repeat
          s := Trim(Fetch(sValue, ','));
          if s <> '' then
          begin
            TmpFeature := MapTypeConfig.GetFeatureByName(s);
            if Assigned(TmpFeature) then
              TmpType.AddFeature(TmpFeature);
          end;
        until s = '';
        MapTypeConfig.RegisterType(TmpType);
      end;

      // TFillStyle
      if ini.ValueExists(sSect, 'FillColor') then
      begin
        TmpStyle := TFillStyle.Create();
        TmpStyle.MinZoom := ini.ReadInteger(sSect, 'MinZoom', TmpStyle.MinZoom);
        TmpStyle.MaxZoom := ini.ReadInteger(sSect, 'MaxZoom', TmpStyle.MaxZoom);
        TmpStyle.Name := TmpType.TypeName + '_Fill';
        _ReadColor(sSect, 'FillColor', (TmpStyle as TFillStyle).FillColor);
        MapStyleConfig.AddStyle(TmpType, TmpStyle);
      end;

      // TBorderStyle
      if ini.ValueExists(sSect, 'BorderColor') then
      begin
        TmpStyle := TBorderStyle.Create();
        TmpStyle.MinZoom := ini.ReadInteger(sSect, 'MinZoom', TmpStyle.MinZoom);
        TmpStyle.MaxZoom := ini.ReadInteger(sSect, 'MaxZoom', TmpStyle.MaxZoom);
        TmpStyle.Name := TmpType.TypeName + '_Border';
        _ReadColor(sSect, 'BorderColor', (TmpStyle as TBorderStyle).Color);
        _ReadColor(sSect, 'BorderGapColor', (TmpStyle as TBorderStyle).GapColor);
        (TmpStyle as TBorderStyle).Width := ini.ReadFloat(sSect, 'BorderWidth', (TmpStyle as TBorderStyle).Width);
        MapStyleConfig.AddStyle(TmpType, TmpStyle);
      end;

      // TTextStyle
      if ini.ValueExists(sSect, 'TextColor')
      or ini.ValueExists(sSect, 'TextSize')
      then
      begin
        TmpStyle := TTextStyle.Create();
        TmpStyle.MinZoom := ini.ReadInteger(sSect, 'MinZoom', TmpStyle.MinZoom);
        TmpStyle.MaxZoom := ini.ReadInteger(sSect, 'MaxZoom', TmpStyle.MaxZoom);
        TmpStyle.Name := TmpType.TypeName + '_Text';
        (TmpStyle as TTextStyle).FeatureType := ftName;
        (TmpStyle as TTextStyle).Size := ini.ReadFloat(sSect, 'TextSize', (TmpStyle as TTextStyle).Size);
        (TmpStyle as TTextStyle).ScaleAndFadeMagLevel := ini.ReadInteger(sSect, 'TextScaleAndFadeMagLevel', (TmpStyle as TTextStyle).ScaleAndFadeMagLevel);
        (TmpStyle as TTextStyle).IsAutoSize := ini.ReadBool(sSect, 'TextAutoSize', (TmpStyle as TTextStyle).IsAutoSize);
        _ReadColor(sSect, 'TextColor', (TmpStyle as TTextStyle).TextColor);
        MapStyleConfig.AddStyle(TmpType, TmpStyle);
      end;

      // TLineStyle
      if ini.ValueExists(sSect, 'LineColor')
      or ini.ValueExists(sSect, 'LineWidth')
      then
      begin
        TmpStyle := TLineStyle.Create();
        TmpStyle.MinZoom := ini.ReadInteger(sSect, 'MinZoom', TmpStyle.MinZoom);
        TmpStyle.MaxZoom := ini.ReadInteger(sSect, 'MaxZoom', TmpStyle.MaxZoom);
        TmpStyle.Name := TmpType.TypeName + '_Line';
        _ReadColor(sSect, 'LineColor', (TmpStyle as TLineStyle).LineColor);
        _ReadColor(sSect, 'LineGapColor', (TmpStyle as TLineStyle).GapColor);
        (TmpStyle as TLineStyle).Width := ini.ReadFloat(sSect, 'LineWidth', (TmpStyle as TLineStyle).Width);
        (TmpStyle as TLineStyle).DisplayWidth := ini.ReadFloat(sSect, 'LineDisplayWidth', (TmpStyle as TLineStyle).DisplayWidth);
        (TmpStyle as TLineStyle).Priority := ini.ReadInteger(sSect, 'LinePriority', (TmpStyle as TLineStyle).Priority);
        _ReadDash(sSect, 'LineDash', (TmpStyle as TLineStyle));
        MapStyleConfig.AddStyle(TmpType, TmpStyle);
      end;

      // TPathTextStyle
      if ini.ValueExists(sSect, 'PathTextColor')
      or ini.ValueExists(sSect, 'PathTextSize')
      then
      begin
        TmpStyle := TPathTextStyle.Create();
        TmpStyle.MinZoom := ini.ReadInteger(sSect, 'MinZoom', TmpStyle.MinZoom);
        TmpStyle.MaxZoom := ini.ReadInteger(sSect, 'MaxZoom', TmpStyle.MaxZoom);
        with (TmpStyle as TPathTextStyle) do
        begin
          Name := TmpType.TypeName + '_PathText';
          FeatureType := ftName;
          Size := ini.ReadFloat(sSect, 'PathTextSize', Size);
          //ScaleAndFadeMagLevel := ini.ReadInteger(sSect, 'PathTextScaleAndFadeMagLevel', ScaleAndFadeMagLevel);
          //IsAutoSize := ini.ReadBool(sSect, 'PathTextAutoSize', IsAutoSize);
          _ReadColor(sSect, 'PathTextColor', TextColor);
          Priority := ini.ReadInteger(sSect, 'PathTextPriority', Priority);
        end;
        MapStyleConfig.AddStyle(TmpType, TmpStyle);
      end;
    end;
  finally
    ini.Free();
    slSections.Free();
    if Assigned(ResStream) then
      ResStream.Free();
  end;
end;

procedure TMapManager.Render();
begin
  {if FMapRenderThread.Suspended then
  begin
    FMapRenderThread.MapManager := Self;
    FMapRenderThread.Suspended := False;
  end;
  FMapRenderThread.Run();}

  if Assigned(MapProjection) and Assigned(MapPainter) then
  begin
    MapPainter.DrawMap(MapProjection, MapParameter, MapData);
  end;
  if Assigned(OnAfterRender) then
    OnAfterRender(Self);
end;

{ TMapRenderThread }

procedure TMapRenderThread.Execute;
begin
  FEvent := TMapEvent.Create();
  FEvent.ResetEvent();

  while (not Terminated) do
  begin
    if FEvent.WaitFor(10) = wrSignaled then
    begin
      MapManager.MapPainter.DrawMap(MapManager.MapProjection,
        MapManager.MapParameter, MapManager.MapData);
    //Suspended := True;
      FEvent.ResetEvent();
    end;
  end;
  FreeAndNil(FEvent);
end;

procedure TMapRenderThread.Run;
begin
  if Assigned(FEvent) then
    FEvent.SetEvent();
end;

end.

