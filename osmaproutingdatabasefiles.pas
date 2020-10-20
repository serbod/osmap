(*
  OsMap components for offline rendering and routing functionalities
  based on OpenStreetMap data

  Copyright (C) 2019  Sergey Bodrov

  This source is ported from libosmscout library
  Copyright (C) 2009  Tim Teulings

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

ObjectVariantDataFile

routing/RouteNodeDataFile
  RouteNodeDataFile  ...

routing/RoutingDB
  RoutingDatabase ...
*)

unit OsMapRoutingDatabaseFiles;

//{$mode objfpc}{$H+}
{$mode Delphi}

interface

uses
  Classes, SysUtils,
  OsMapTypes, OsMapGeometry, OsMapRouting, OsMapFiles, OsMapDataFiles,
  OsMapObjTypes, OsMapUtils, OsMapDataTile;

type

  { DataFile class for loading the object variant data, which is part of the
    routing graph. The object variat data contains a lookup table for path
    attributes. Since the number of attribute value combinations is much
    smaller then the actual number of elements in the route graph it makes
    sense to store all possible combinations in a lookup table and just
    index them from the routing graph paths. }

  { TObjectVariantDataFile }
  TObjectVariantDataFile = object
    Data: TObjectVariantDataArray;
    IsLoaded: Boolean;
    Filename: string;

    { todo }
    procedure Init();
    function Load(ATypeConfig: TTypeConfig; AFilename: string): Boolean;
  end;

  { TRouteNodeDataFile }

  TRouteNodeDataFile = class
  private
    type
      TIndexEntry = record
        FileOffset: TFileOffset;
        Count: Integer;
      end;

      TIndexPage = record
        FileOffset: TFileOffset;  // TIndexEntry.FileOffset
        Remaining: Integer;       // TIndexEntry.Count
        //NodeMap: TRouteNodeMapById;

        RouteNodeList: TRouteNodeArray;
        RouteNodeHash: TSimpleStringHash;

        //procedure Init();
        //function Find(AScanner: TFileScanner; AId: TId): TRouteNode;
      end;

      //TIndexEntryMapByPixel = specialize TFPGMap<TPixel, TIndexEntry>;

  private
    FDataFile: string;     // Basename part of the data file name
    FDataFileName: string; // complete filename for data file

    FTypeConfig: TTypeConfig;

    //FIndex: TIndexEntryMapByPixel;  // map<Tile, IndexEntry>;
    FIndexEntryArray: array of TIndexEntry;
    FIndexEntryHash: TSimpleStringHash;

    // type TValueCache = TCache<TileId, IndexPage>
    //FCache: TValueCache;  // Cache of loaded route node pages
    FIndexPageArray: array of TIndexPage;
    FIndexPageCache: TSimpleCache;

    FScanner: TFileScanner; // File stream to the data file
    //FAccessMutex: TMutex; // Mutex to secure multi-thread access
    FMagnification: TMagnification; // Magnification of tiled index


    procedure IndexPageInit(out AValue: TIndexPage;
      AFileOffset: TFileOffset = 0; ARemaining: Integer = 0);
    function IndexPageFind(var AIndexPage: TIndexPage; AScanner: TFileScanner;
      AId: TId): PRouteNode;

    { emulate hashmap TId : TRouteNode for TIndexPage }
    {procedure SetIndexPageRouteNodeItem(var AIndexPage: TIndexPage;
        const AItemKey: TId; const AItemValue: TRouteNode);
    function GetIndexPageRouteNodeItem(const AIndexPage: TIndexPage;
        const AItemKey: TId; var AItemValue: TRouteNode): Boolean; }

    { emulate hashmap IndexTile : TIndexEntry }
    procedure SetIndexEntry(const AItemKey: TIndexTile; const AItemValue: TIndexEntry);
    function GetIndexEntry(const AItemKey: TIndexTile; var AItemValue: TIndexEntry): Boolean;

    { emulate cache IndexTile.Id : TIndexPage }
    function GetCacheEntry(const AKey: TId; var AValue: TIndexPage): Boolean;
    procedure SetCacheEntry(const AKey: TId; const AValue: TIndexPage);


    function LoadIndexPage(const ATile: TIndexTile; var AValue: TIndexPage): Boolean;
    function GetIndexPage(const ATile: TIndexTile; var AValue: TIndexPage): Boolean;

  public
    constructor Create(const ADataFile: string; ACacheSize: Integer);

    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean;

    { Return true, if index is currently opened. }
    function IsOpen(): Boolean; virtual;
    { Close the index. }
    function Close(): Boolean; virtual;

    function GetTile(const ACoord: TGeoPoint): TIndexTile;
    function IsCovered(const ATile: TIndexTile): Boolean; overload;
    function IsCovered(const ACoord: TGeoPoint): Boolean; overload;

    function Get(AId: TId; out ANode: TRouteNode): Boolean; overload;
    { AIds - TMapPoint.ID }
    function Get(const AIds: TIdArray; AData: TRouteNodeList): Boolean; overload;
    function Get(const AIds: TIdArray; AData: TRouteNodeMapById): Boolean; overload;
    function Get(const ACoords: TGeoPointArray; AData: TRouteNodeList): Boolean; overload;
    function Get(const ACoords: TGeoPointArray; AData: TRouteNodeMapById): Boolean; overload;
  end;


  TIntersectionDataFile = class(TIndexedDataFile)
    { todo }

  end;

  { Encapsulation of the routing relevant data files, similar to Database. }
  TRoutingDatabase = class(TObject)
  private
    FTypeConfig: TTypeConfig;
    FPath: string;
    FRouteNodeDataFile: TRouteNodeDataFile;
    FJunctionDataFile: TIntersectionDataFile; // Cached access to the 'junctions.dat' file
    FObjectVariantDataFile: TObjectVariantDataFile;
  public
    { todo }
    constructor Create();

    function Open(ADatabase: TMapDatabaseId): Boolean;
    procedure Close();

    function GetRouteNode(const AId: TId; ANode: TRouteNode): Boolean;

    function GetRouteNodes(ABegin, AEnd: Integer; ASize: Integer;
      ARouteNodeMap: TRouteNodeMapById): Boolean; overload;

    function GetRouteNodes(ABegin, AEnd: Integer; ASize: Integer;
      ARouteNodes: TRouteNodeList): Boolean; overload;

    { Junction = Intersection }
    function GetJunctions(const AIds: array of TId;
      var AJunctions: TIntersectionArray): Boolean;

    function GetObjectVariantData(): TObjectVariantDataArray;

    function ContainsNode(const AId: TId): Boolean;
  end;


implementation

{ TObjectVariantDataFile }

procedure TObjectVariantDataFile.Init();
begin
  SetLength(Data, 0);
  IsLoaded := False;
  Filename := '';
end;

function TObjectVariantDataFile.Load(ATypeConfig: TTypeConfig;
  AFilename: string): Boolean;
var
  Scanner: TFileScanner;
  i, iDataCount: Integer;
begin
  Init();
  Filename := AFilename;

  Scanner := TFileScanner.Create();
  try
    Scanner.Open(Filename, fsmSequential, True);

    Scanner.Read(iDataCount);
    SetLength(Data, iDataCount);
    for i := 0 to iDataCount-1 do
    begin
      Data[i].Read(ATypeConfig, Scanner);
    end;
    Scanner.Close();
    IsLoaded := True;
  finally
    Scanner.Free();
  end;
  Result := IsLoaded;
end;

{ TRouteNodeDataFile }

procedure TRouteNodeDataFile.IndexPageInit(out AValue: TIndexPage;
  AFileOffset: TFileOffset; ARemaining: Integer);
begin
  AValue.FileOffset := AFileOffset;
  AValue.Remaining := ARemaining;
  AValue.RouteNodeHash.Init();
  SetLength(AValue.RouteNodeList, 0);
end;

function TRouteNodeDataFile.IndexPageFind(var AIndexPage: TIndexPage;
  AScanner: TFileScanner; AId: TId): PRouteNode;
var
  n: Integer;
begin
  n := Length(AIndexPage.RouteNodeList);
  if (n > 0) then
  begin
    if AIndexPage.RouteNodeHash.FindValue(IntToStr(AId), n) then
    begin
      Result := AIndexPage.RouteNodeList[n];
      Exit;
    end;
  end;

  if (AIndexPage.Remaining > 0) then
  begin
    FScanner.Position := fileOffset;

    while (AIndexPage.Remaining > 0) do
    begin
      Dec(AIndexPage.Remaining);

      SetLength(AIndexPage.RouteNodeList, n+1);
      AIndexPage.RouteNodeList[n].Init();
      Result := AIndexPage.RouteNodeList[n];

      Result.Read(FTypeConfig, FScanner);

      AIndexPage.RouteNodeHash.Add(IntToStr(Result.GetId()), n);

      fileOffset := FScanner.Position;

      if (Result.GetId() = AId) then
        Exit;
    end;
  end;

  Result := nil;
end;

procedure TRouteNodeDataFile.SetIndexEntry(const AItemKey: TIndexTile;
  const AItemValue: TIndexEntry);
var
  n: Integer;
begin
  n := Length(FIndexEntryArray);
  SetLength(FIndexEntryArray, n+1);
  FIndexEntryArray[n] := AItemValue;
  FIndexEntryHash.Add(IntToStr(AItemKey.GetId()), n);
end;

function TRouteNodeDataFile.GetIndexEntry(const AItemKey: TIndexTile;
  var AItemValue: TIndexEntry): Boolean;
var
  n: Integer;
begin
  Result := False;
  if (Length(FIndexEntryArray) > 0) then
  begin
    if FIndexEntryHash.FindValue(IntToStr(AItemKey.GetId()), n) then
    begin
      AItemValue := FIndexEntryArray[n];
      Result := True;
    end;
  end;
end;

{procedure TRouteNodeDataFile.SetIndexPageRouteNodeItem(var AIndexPage: TIndexPage;
  const AItemKey: TId; const AItemValue: TRouteNode);
begin

end;

function TRouteNodeDataFile.GetIndexPageRouteNodeItem(const AIndexPage: TIndexPage;
  const AItemKey: TId; var AItemValue: TRouteNode): Boolean;
var
  n: Integer;
begin
  n := Length(AIndexPage.RouteNodeList);
  Result := False;
  if (n > 0) then
  begin
    if AIndexPage.RouteNodeHash.FindValue(IntToStr(AItemKey), n) then
    begin
      AItemValue := AIndexPage.RouteNodeList[n];
      Result := True;
    end;
  end;
end;  }

function TRouteNodeDataFile.GetCacheEntry(const AKey: TId;
  var AValue: TIndexPage): Boolean;
var
  n: Integer;
begin
  Result := FIndexPageCache.GetEntry(IntToStr(AKey), n);
  if Result then
    AValue := FIndexPageArray[n];
end;

procedure TRouteNodeDataFile.SetCacheEntry(const AKey: TId;
  const AValue: TIndexPage);
var
  n: Integer;
begin
  FIndexPageCache.SetEntry(IntToStr(AKey), n);
  FIndexPageArray[n] := AValue;
end;

function TRouteNodeDataFile.LoadIndexPage(const ATile: TIndexTile;
  var AValue: TIndexPage): Boolean;
var
  entry: TIndexEntry;
  cacheEntry: TIndexPage;
begin
  Assert(IsOpen());

  Result := GetIndexEntry(ATile, entry);

  if Result then
  begin
    IndexPageInit(cacheEntry, entry.FileOffset, entry.Count);
    SetCacheEntry(ATile.GetId(), cacheEntry);
  end;
end;

function TRouteNodeDataFile.GetIndexPage(const ATile: TIndexTile;
  var AValue: TIndexPage): Boolean;
var
  cacheEntry: TIndexPage;
begin
  if (not GetCacheEntry(ATile.GetId(), cacheEntry)) then
  begin
    //std::cout << "RouteNodeDF::GetIndexPage() Not fond in cache, loading...!" << std::endl;
    Result := LoadIndexPage(ATile, cacheEntry));
  end
  else
    Result := True;
end;

constructor TRouteNodeDataFile.Create(const ADataFile: string;
  ACacheSize: Integer);
begin
  FDataFile := ADataFile;
  FScanner := TFileScanner.Create();

  SetLength(FIndexEntryArray, 0);
  FIndexEntryHash.Init();

  SetLength(FIndexPageArray, 0);
  FIndexPageCache.Init(ACacheSize);
end;

function TRouteNodeDataFile.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMappedData: Boolean): Boolean;
var
  indexFileOffset: TFileOffset;
  dataCount, indexEntryCount, tileMag: UInt32;
  i: Integer;
  cell: TIndexTile;
  entry: TIndexEntry;
begin
  FTypeConfig := ATypeConfig;

  FDataFileName := AppendFileToDir(APath, FDataFile);

  try
    FScanner.Open(FDataFileName, fsmLowMemRandom, AMemoryMappedData);

    FScanner.Read(indexFileOffset);
    FScanner.Read(dataCount);
    FScanner.Read(tileMag);

    FMagnification.SetLevel(tileMag);

    FScanner.Position := indexFileOffset;
    FScanner.Read(indexEntryCount);

    for i := 1 to indexEntryCount do
    begin
      FScanner.Read(cell.X);
      FScanner.Read(cell.Y);
      FScanner.ReadFileOffset(entry.FileOffset);
      FScanner.Read(entry.Count);

      //index[cell]=entry;
      SetIndexEntry(cell, entry);
    end;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'Error: ', E.ToString());
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TRouteNodeDataFile.IsOpen(): Boolean;
begin
  Result := FScanner.IsOpen;
end;

function TRouteNodeDataFile.Close(): Boolean;
begin
  FTypeConfig := nil;

  try
    if FScanner.IsOpen() then
      FScanner.Close();

    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'Error: ', E.ToString());
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TRouteNodeDataFile.GetTile(const ACoord: TGeoPoint): TIndexTile;
begin
  Result := GetIndexTile(FMagnification, ACoord);
end;

function TRouteNodeDataFile.IsCovered(const ATile: TIndexTile): Boolean;
var
  IndexEntry: TIndexEntry;
begin
  Result := GetIndexEntry(ATile, IndexEntry);
end;

function TRouteNodeDataFile.IsCovered(const ACoord: TGeoPoint): Boolean;
begin
  Result := IsCovered(GetTile(ACoord));
end;

function TRouteNodeDataFile.Get(AId: TId; out ANode: TRouteNode): Boolean;
var
  coord: TGeoPoint;
  tile: TIndexTile;
  IndexPage: TIndexPage;
begin
  //std::cout << "Loading RouteNode " << id << "..." << std::endl;

  coord := GetCoordFromId(AId);
  tile := GetIndexTile(FMagnification, coord);

  //std::cout << "Tile " << tile.GetDisplayText() << " " << tile.GetId() << "..." << std::endl;

  if GetIndexPage(tile, IndexPage) then
  begin
    ANode := IndexPageFind(IndexPage, FScanner, AId);
    Result := (ANode <> nil);
  end
  else
  begin
    ANode := nil;
    Result := False;
  end;
end;

function TRouteNodeDataFile.Get(const AIds: TIdArray;
  AData: TRouteNodeList): Boolean;
var
  i: Integer;
  node: TRouteNode;
begin
  AData.Capacity := Length(AIds);

  for i := Low(AIds) to High(AIds) do
  begin
    if Get(AIds[i], node) then
      AData.Add(node)
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

function TRouteNodeDataFile.Get(const AIds: TIdArray;
  AData: TRouteNodeMapById): Boolean;
var
  id: TId;
  node: TRouteNode;
begin
  AData.Capacity := Length(AIds);

  for (id in AIds) do
  begin
    if Get(id, node) then
      AData.AddOrSetData(id, node)
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

function TRouteNodeDataFile.Get(const ACoords: TGeoPointArray;
  AData: TRouteNodeList): Boolean;
var
  i: Integer;
  node: TRouteNode;
begin
  AData.Capacity := Length(AIds);

  for i := Low(ACoords) to High(ACoords) do
  begin
    if Get(ACoords[i].GetId(), node) then
      AData.Add(node)
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

function TRouteNodeDataFile.Get(const ACoords: TGeoPointArray;
  AData: TRouteNodeMapById): Boolean;
var
  i: Integer;
  node: TRouteNode;
begin
  AData.Capacity := Length(AIds);

  for i := Low(ACoords) to High(ACoords) do
  begin
    if Get(ACoords[i].GetId(), node) then
      AData.AddOrSetData(ACoords[i].GetId(), node)
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;


end.

