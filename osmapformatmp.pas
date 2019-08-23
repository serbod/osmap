unit OsMapFormatMp;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$ZEROBASEDSTRINGS OFF}
{$endif}

interface

uses
  Types, Classes, SysUtils, strutils,
  {$ifdef FPC}
  streamex,
  {$endif}
  OsMapManager, OsMapObjects, OsMapGeometry,
  OsMapTypes;

type

  { TMpFileImporter }

  TMpFileImporter = object
  private
    IsWay, IsPoint, IsArea: Boolean;
    FManager: TMapManager;
    LineNo: Integer;
    AreaCircleNo: Integer;

    TmpArea: TMapArea;
    TmpWay: TMapWay;
    procedure ParseLine(AText: string);
    procedure ParseKeyValue(const AKey, AValue: string);
    procedure ParseAreaType(const TypeId: Integer);
    procedure ParseWayType(const TypeId: Integer);
    procedure ParseLabel(const AValue: string);
    procedure ParseData0(AValue: string);
  public
    procedure Init();

    function Import(AManager: TMapManager; AFileName: string): Boolean;
  end;

function ImportFromMpFile(AManager: TMapManager; AFileName: string): Boolean;

implementation

{$ifdef FPC}
uses Math, LazUTF8;
{$endif}

function ImportFromMpFile(AManager: TMapManager; AFileName: string): Boolean;
var
  Importer: TMpFileImporter;
begin
  Result := Importer.Import(AManager, AFileName);
end;

{ TMpFileImporter }

procedure TMpFileImporter.Init();
begin
  IsArea := False;
  IsPoint := False;
  IsWay := False;

  TmpArea := nil;
  TmpWay := nil;
end;

procedure TMpFileImporter.ParseLine(AText: string);
var
  s, sKey, sValue: string;
  n: Integer;
begin
  s := Copy(AText, 1, 1);
  if s = '[' then
  begin
    if AText = '[POI]' then
    begin
      IsPoint := True;
    end
    else
    if AText = '[POLYLINE]' then
    begin
      IsWay := True;
    end
    else
    if AText = '[POLYGON]' then
    begin
      IsArea := True;
      AreaCircleNo := 0;
    end
    else
    if AText = '[END]' then
    begin
      if IsPoint then
      begin
        IsPoint := False;
      end;

      if IsWay then
      begin
        IsWay := False;
        if Assigned(TmpWay) then
        begin
          FManager.MapData.WayList.Add(TmpWay);
          TmpWay := nil;
        end;

      end;

      if IsArea then
      begin
        IsArea := False;
        if Assigned(TmpArea) then
        begin
          if Length(TmpArea.Rings[0].Nodes) > 2 then
            FManager.MapData.AreaList.Add(TmpArea)
          else
            TmpArea.Free();
          TmpArea := nil;
        end;
      end;
    end;
  end
  else
  if (s = ';') or (s = '') then
  begin
    // comment, ignore
  end
  else
  begin
    // key=value
    n := Pos('=', AText);
    sKey := Copy(AText, 1, n-1);
    sValue := Copy(AText, n+1, MaxInt);

    ParseKeyValue(sKey, sValue);
  end;
end;

procedure TMpFileImporter.ParseKeyValue(const AKey, AValue: string);
var
  TypeId: Integer;
begin
  if AKey = 'Type' then
  begin
    TypeId := StrToInt(AValue);
    if IsArea then
    begin
      ParseAreaType(TypeId);
    end;

    if IsWay then
    begin
      ParseWayType(TypeId);
    end;
  end
  else
  if AKey = 'Data0' then
  begin
    ParseData0(AValue);
  end
  else
  if AKey = 'Label' then
  begin
    ParseLabel(AValue);
  end
  else
  if AKey = 'HouseNumber' then
  begin
    if IsArea and Assigned(TmpArea) then
    begin
      // номер дома
      TmpArea.Rings[0].FeatureValueBuffer.SetFeatureValue(ftName, AValue);
    end
  end
  else
  if AKey = 'StreetDesc' then
  begin
    if IsArea and Assigned(TmpArea) then
    begin
      // улица
      TmpArea.Rings[0].FeatureValueBuffer.SetFeatureValue(ftAddress, AValue);
    end
  end;
end;

procedure TMpFileImporter.ParseAreaType(const TypeId: Integer);
begin
  case TypeId of
    $01, // place = city
    $02: // place = town
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('city'));
    end;
    $03, // landuse = residential
    $04: // landuse = military
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('landuse_residential'));
    end;
    $05: // landuse = garages
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building_industrial'));
    end;
    $06: // building = garages
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building_industrial'));
    end;
    $07, // building = yes, aeroway = terminal
    $08, // landuse = retail
    $09: // building = yes, leisure = marina
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building_industrial'));
    end;
    $0A,  // amenity = school
    $0B, // amenity = hospital
    $0C, // industrial
    $6F: // building = industrial
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building_industrial'));
    end;
    $13: // building
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building'));
    end;
    $17: // leisure = park
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('forest'));
    end;
    $19, // leisure = pitch, sports
    $4D: // sport = ice_hockey
    begin
      //TmpArea := TMapArea.Create();
      //TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('sports'));
    end;
    $40, // natural = water
    $41, // natural = water
    $81, // natural = wetland
    $47: // waterway = riverbank
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('water'));
    end;
    $4E, // landuse = orchard
    $4F, // natural = scrub
    $50, // landuse = forest
    $51: // natural = wood
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('forest'));
    end;
    $6C, // building = apartments
    $6D, // building = administrative
    $6E: // building = hospital
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('building_apartments'));
    end;
    $88, // landuse = farmland
    $89: //natural = beach
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('farmland'));
    end;
    $95, // natural = grassland
    $98: // landuse = grass
    begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('grass'));
    end;
    $1A, // barrier = wall, landuse = cemetery
    $6A, // area = yes, highway = footway, highway = pedestrian
    $8A: // natural = scree
    begin
      // area = yes, highway = footway, highway = pedestrian
    end
  else
    {begin
      TmpArea := TMapArea.Create();
      TmpArea.Rings[0].SetType(FManager.MapTypeConfig.GetTypeInfo('grass'));
    end;}
  end;
end;

procedure TMpFileImporter.ParseWayType(const TypeId: Integer);
begin
  case TypeId of
    $00: // highway = construction
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_construction'));
    end;
    $01: // highway = trunk (магистраль)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_trunk'));
    end;
    $04: // highway = primary  (главная дорога)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_primary'));
    end;
    $02, // highway = secondary  (второстепенная дорога)
    $03, // highway = tertiary
    $05: // highway = secondary, tertiary
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_secondary'));
    end;
    $06: // highway = residential  (проезд к домам)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_residential'));
    end;
    $07: // highway = service  (служебный проезд)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_service'));
    end;
    $08:  // highway = tertiary_link  (выезд на второстепенную)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_secondary'));
    end;
    $09: // highway = trunk_link  (выезд на магистраль)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_trunk'));
    end;
    $0A: // highway = track (грунтовка)
    begin
      //TmpWay := TMapWay.Create();
      //TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_primary'));
    end;
    $0C: // junction = roundabout, highway = primary, secondary  (круговое движение)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_primary'));
    end;
    $14: // railway = rail  (железная дорога)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('railway_rail'));
    end;
    $18, // waterway = stream
    $1F: // waterway = river
    begin
      //TmpWay := TMapWay.Create();
      //TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('water_way'));
    end;
    $3F: // railway = tram  (трамвайные рельсы)
    begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('railway_tram'));
    end;
    $16, // highway = path  (тропинка)
    $1C, // boundary = administrative
    $22, // area = yes, natural = cliff
    $26, // waterway = ditch
    $27, // aeroway = runway
    $28, // man_made = pipeline
    $29, // power = line
    $42, // highway = track, tracktype = grade1  (метро?)
    $43, // highway = residential, tunnel = yes
    $46, // barrier = fence
    $48: // man_made = cutline
    begin
      //TmpWay := TMapWay.Create();
      //TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_path'));
    end;
  else
    {begin
      TmpWay := TMapWay.Create();
      TmpWay.SetType(FManager.MapTypeConfig.GetTypeInfo('highway_road'));
    end;}
  end;
end;

procedure TMpFileImporter.ParseLabel(const AValue: string);
begin
  if IsArea and Assigned(TmpArea) then
  begin
    // номер дома, название участка
    //TmpArea.Rings[0].FeatureValueBuffer.SetFeatureValue(ftName, UTF8ToSys(AValue));
    TmpArea.Rings[0].FeatureValueBuffer.SetFeatureValue(ftName, AValue);
  end
  else
  if IsWay and Assigned(TmpWay) then
  begin
    // название дороги / реки
    //TmpArea.Rings[0].FeatureValueBuffer.SetFeatureValue(ftName, UTF8ToSys(AValue));
    TmpWay.FeatureValueBuffer.SetFeatureValue(ftName, AValue);
  end;
end;

procedure TMpFileImporter.ParseData0(AValue: string);
  function _ExtractCoord(var AOffs: Integer; out ACoord: TGeoPoint): Boolean;
  var
    iBeg, iEnd: Integer;
  begin
    iBeg := PosEx('(', AValue, AOffs);
    if iBeg > 0 then
    begin
      iEnd := PosEx(')', AValue, AOffs);
      AOffs := iEnd + 1;
      Result := ACoord.Parse(Copy(AValue, iBeg+1, iEnd-iBeg-1));
    end
    else
      Result := False;
  end;

var
  Offs: Integer;
  //Coord: TGeoPoint;
  GeoPoint: TGeoPoint;
  n: Integer;
begin
  if Assigned(TmpWay) then
  begin
    Offs := 1;
    n := 0;
    while _ExtractCoord(Offs, GeoPoint) do
    begin
      //GeoPoint.Serial := Byte(n);
      AppendGeoPointToArray(TmpWay.Nodes, GeoPoint);
      Inc(n);
    end;
  end;

  if Assigned(TmpArea) then
  begin
    if AreaCircleNo > 0 then
    begin
      SetLength(TmpArea.Rings, Length(TmpArea.Rings)+1);
      TmpArea.Rings[AreaCircleNo].Ring := 2;
      TmpArea.Rings[AreaCircleNo].SetType(TmpArea.Rings[AreaCircleNo-1].GetType());
    end;

    Offs := 1;
    n := 0;
    while _ExtractCoord(Offs, GeoPoint) do
    begin
      //GeoPoint.Serial := Byte(n);
      AppendGeoPointToArray(TmpArea.Rings[AreaCircleNo].Nodes, GeoPoint);
      Inc(n);
    end;
    Inc(AreaCircleNo);
  end;

end;

function TMpFileImporter.Import(AManager: TMapManager; AFileName: string): Boolean;
var
  //txt: TextFile;
  ss: string;
  ResStream: TStream;
  sr: TStreamReader;
begin
  Result := False;
  Assert(Assigned(AManager));
  if Pos('_', AFileName) = 1 then
  begin
    // from resource
    ResStream := TResourceStream.Create(HInstance, AFileName, RT_RCDATA);
  end
  else
  begin
    //ResStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    ResStream := TFileStream.Create(AFileName, fmOpenRead);
  end;
  sr := TStreamReader.Create(ResStream);

  Init();
  FManager := AManager;
  LineNo := 0;
  {$ifdef FPC}
  while not sr.Eof do
  {$else}
  while not sr.EndOfStream do
  {$endif}
  begin
    Inc(LineNo);
    ss := sr.ReadLine;
    ParseLine(ss);
  end;
  Result := True;
  sr.Free();
  ResStream.Free();

  {end
  else
  begin
    if not FileExists(AFileName) then
      Exit;

    Init();
    FManager := AManager;
    LineNo := 0;
    AssignFile(txt, AFileName);
    try
      Reset(txt);
      while not EOF(txt) do
      begin
        Inc(LineNo);
        ReadLn(txt, ss);
        ParseLine(ss);
      end;

      Result := True;
    finally
      CloseFile(txt);
    end;
  end;  }
end;

end.

