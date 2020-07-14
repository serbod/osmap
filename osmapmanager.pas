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
  {$ifdef MSWINDOWS}Windows, {$else}Types, {$endif}
  Classes, SysUtils, SyncObjs,
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
    WaysFileName: string;
    AreasFileName: string;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    //procedure InitTypes();
    { Load map object types properties from .ini file.
      if AFileName begins with underscope "_" - load from named resource }
    procedure InitTypesFromIni(AFileName: string);

    procedure Render();

    procedure SaveWaysToFile(AFileName: string);
    procedure SaveAreasToFile(AFileName: string);

    procedure LoadWaysFromFile(AFileName: string);
    procedure LoadAreasFromFile(AFileName: string);

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

uses OsMapUtils, OsMapFiles;

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
  WaysFileName := 'ways.mbd';
  AreasFileName := 'areas.mbd';
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
  n1, n2: TReal;
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
    ResStream := TResourceStream.Create(HInstance, AFileName, Windows.RT_RCDATA);
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
        TmpType.ZOrder := ini.ReadInteger(sSect, 'ZOrder', 0);
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
        (TmpStyle as TBorderStyle).WidthMM := ini.ReadFloat(sSect, 'BorderWidth', (TmpStyle as TBorderStyle).WidthMM);
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
        (TmpStyle as TTextStyle).SizeMM := ini.ReadFloat(sSect, 'TextSize', (TmpStyle as TTextStyle).SizeMM);
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
        (TmpStyle as TLineStyle).DisplayWidthMM := ini.ReadFloat(sSect, 'LineDisplayWidth', (TmpStyle as TLineStyle).DisplayWidthMM);
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
          SizeMM := ini.ReadFloat(sSect, 'PathTextSize', SizeMM);
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

procedure TMapManager.SaveWaysToFile(AFileName: string);
var
  Writer: TFileWriter;
  TmpWay: TMapWay;
  n: UInt64;
begin
  Writer := TFileWriter.Create();
  try
    Writer.Open(AFileName);
    n := MapData.WayList.Count;
    Writer.WriteNumber(n);
    for TmpWay in MapData.WayList do
    begin
      TmpWay.Write(MapTypeConfig, Writer);
    end;
    Writer.Close();
  finally
    Writer.Free();
  end;
end;

procedure TMapManager.SaveAreasToFile(AFileName: string);
var
  Writer: TFileWriter;
  TmpArea: TMapArea;
  n: UInt64;
begin
  Writer := TFileWriter.Create();
  try
    Writer.Open(AFileName);
    n := MapData.AreaList.Count;
    Writer.WriteNumber(n);
    for TmpArea in MapData.AreaList do
    begin
      TmpArea.Write(MapTypeConfig, Writer);
    end;
    Writer.Close();
  finally
    Writer.Free();
  end;
end;

procedure TMapManager.LoadWaysFromFile(AFileName: string);
var
  Reader: TFileScanner;
  TmpWay: TMapWay;
  n: UInt64;
begin
  Reader := TFileScanner.Create();
  try
    Reader.Open(AFileName, fsmNormal, False);
    Reader.ReadNumber(n);

    while n > 0 do
    begin
      TmpWay := TMapWay.Create();
      TmpWay.Read(MapTypeConfig, Reader);
      MapData.WayList.Add(TmpWay);
    end;
    Reader.Close();
  finally
    Reader.Free();
  end;
end;

procedure TMapManager.LoadAreasFromFile(AFileName: string);
var
  Reader: TFileScanner;
  TmpArea: TMapArea;
  n: UInt64;
begin
  Reader := TFileScanner.Create();
  try
    Reader.Open(AFileName, fsmNormal, False);
    Reader.ReadNumber(n);

    while n > 0 do
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Read(MapTypeConfig, Reader);
      MapData.AreaList.Add(TmpArea);
    end;
    Reader.Close();
  finally
    Reader.Free();
  end;
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

