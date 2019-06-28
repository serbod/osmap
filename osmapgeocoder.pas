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
(*
Geocoder - find coordinates by postal address
*)

unit OsMapGeocoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsMapGeometry, OsMapTypes, OsMapObjTypes, OsMapObjects;


type

  { TMapGeocoder }

  TMapGeocoder = class
  private
    procedure FindSIDArr(const AName: string; var AIdArr: TBoundArray; AMaxCount: Integer = 10);
    function FindSID(const AName: string): Integer;
    function FindWay(const AName: string): TMapWay;
  public
    MapData: TMapData;
    ValStorage: TFeatureValueStorage;

    function GetStreetStart(const AName: string; out APoint: TGeoPoint): Boolean;
    function GetStreetEnd(const AName: string; out APoint: TGeoPoint): Boolean;
    //function GetStreetCenter(const AName: string; out APoint: TGeoPoint): Boolean;

    function GetAreaCenter(const AName: string; out APoint: TGeoPoint): Boolean;

    //function GetCityCenter(const AName: string; out APoint: TGeoPoint): Boolean;
  end;

implementation

uses LazUTF8;

{ TMapGeocoder }

procedure TMapGeocoder.FindSIDArr(const AName: string; var AIdArr: TBoundArray; AMaxCount: Integer);
var
  s, sName: string;
  i, IdCount: Integer;
begin
  if not Assigned(ValStorage) then Exit;
  if not Assigned(MapData) then Exit;

  sName := Trim(UTF8LowerCase(AName));
  IdCount := 0;
  SetLength(AIdArr, AMaxCount);
  for i := 0 to ValStorage.StrList.Count-1 do
  begin
    s := Trim(UTF8LowerCase(ValStorage.StrList[i]));

    //if Pos(sName, s) > 0 then
    if UTF8Pos(sName, s) > 0 then
    begin
      AIdArr[IdCount] := i;
      Inc(IdCount);
      if IdCount >= AMaxCount then
        Break;
    end;
  end;
  SetLength(AIdArr, IdCount);
end;

function TMapGeocoder.FindSID(const AName: string): Integer;
var
  s, sName: string;
  i: Integer;
begin
  Result := -1;
  if not Assigned(ValStorage) then Exit;
  if not Assigned(MapData) then Exit;

  sName := Trim(UTF8LowerCase(AName));
  for i := 0 to ValStorage.StrList.Count-1 do
  begin
    s := Trim(UTF8LowerCase(ValStorage.StrList[i]));

    //if Pos(sName, s) > 0 then
    if UTF8Pos(sName, s) > 0 then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TMapGeocoder.FindWay(const AName: string): TMapWay;
var
  i, TmpId, SID: Integer;
begin
  Result := nil;
  SID := FindSID(AName);
  if SID = -1 then Exit;

  for i := 0 to MapData.WayList.Count-1 do
  begin
    TmpId := MapData.WayList.Items[i].FeatureValueBuffer.GetFeatureValueId(ftName);
    if SID = TmpId then
    begin
      Result := MapData.WayList.Items[i];
      Break;
    end;
  end;
end;

function TMapGeocoder.GetStreetStart(const AName: string; out
  APoint: TGeoPoint): Boolean;
var
  TmpWay: TMapWay;
begin
  Result := False;
  TmpWay := FindWay(AName);

  if Assigned(TmpWay) then
  begin
    APoint := TmpWay.GetCoord(0);
    Result := True;
  end;
end;

function TMapGeocoder.GetStreetEnd(const AName: string; out
  APoint: TGeoPoint): Boolean;
var
  TmpWay: TMapWay;
begin
  Result := False;
  TmpWay := FindWay(AName);

  if Assigned(TmpWay) then
  begin
    APoint := TmpWay.Nodes[Length(TmpWay.Nodes)-1];
    Result := True;
  end;
end;

{function TMapGeocoder.GetStreetCenter(const AName: string; out
  APoint: TGeoPoint): Boolean;
begin

end;  }

function TMapGeocoder.GetAreaCenter(const AName: string; out
  APoint: TGeoPoint): Boolean;
var
  i, SID: Integer;
  TmpArea: TMapArea;
begin
  Result := False;
  if not Assigned(ValStorage) then Exit;
  if not Assigned(MapData) then Exit;

  SID := FindSID(AName);
  if SID = -1 then Exit;

  for i := 0 to MapData.AreaList.Count-1 do
  begin
    TmpArea := MapData.AreaList.Items[i];
    if (SID = TmpArea.Rings[0].FeatureValueBuffer.GetFeatureValueId(ftName))
    or (SID = TmpArea.Rings[0].FeatureValueBuffer.GetFeatureValueId(ftAddress)) then
    begin
      APoint.Init(0, 0);
      Result := TmpArea.Rings[0].GetCenter(APoint);
      Break;
    end;
  end;
end;

{function TMapGeocoder.GetCityCenter(const AName: string; out
  APoint: TGeoPoint): Boolean;
begin

end; }

end.

