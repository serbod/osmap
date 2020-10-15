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
Classes and functions for storing and indexing of OSM objects into
database-like on-disk data structures.

NumericIndex:
  NumericIndex -> TNumericIndexFile

DataFile:
  DataBlockSpan
  DataFile
  IndexedDataFile

BoundingBoxDataFile:
  BoundingBoxDataFile

AreaNodeIndex:
  AreaNodeIndex

AreaWayIndex:
  AreaWayIndex

AreaAreaIndex:
  AreaAreaIndex
*)

unit OsMapDataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  OsMapTypes, OsMapFiles, OsMapObjTypes, OsMapObjects, OsMapGeometry,
  OsMapDataTile, OsMapUtils;

type

  { TNumericIndexFile }

  TNumericIndexFile = class
  private
    type
      TEntry = record
        StartId: TId;
        FileOffset: TFileOffset;
      end;

      TPage = record
        Entries: array of TEntry;
        //function IndexIsValid(AIndex: Integer): Boolean;
      end;
      PPage = ^TPage;

  private
    FFileName: string;             // Name of the index file
    FFilePath: string;             // Complete file name including directory

    FScanner: TFileScanner;        // FileScanner instance for file access

    FCacheSize: Integer;           // Maximum umber of index pages cached
    FPageSize: Integer;            // Size of one page as stated by the actual index file
    FLevels: Integer;              // Number of index levels as stated by the actual index file
    FPageCounts: array of Integer; // Number of pages per level as stated by the actual index file
    FBuffer: array of Byte;        // Temporary buffer for reading page data

    FRoot: TPage;                  // Reference to the root page
    FSimpleCacheMaxLevel: Integer; // Maximum level for simple caching
    //FSimplePageCache: array of TPageSimpleCache;     // Simple map to cache all entries
    //FPageCaches: array of TPageCache;          // Complex cache with LRU characteristics

    //FAccessMutex: TMutex;         // Mutex to secure multi-thread access

    function GetCacheValueSize(const AValue: TPage): Integer;
    function PageIndexIsValid(const APage: TPage; AIndex: Integer): Boolean;
    { Binary search for index page for given id }
    function GetPageIndex(const APage: TPage; AId: TId): Integer;
    procedure ReadPage(AOffset: TFileOffset; var APage: TPage);
    procedure InitializeCache();
  public
    constructor Create(const AFileName: string; ACacheSize: Integer);
    destructor Destroy(); override;

    { Open the index file. }
    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMapped: Boolean): Boolean;

    { Return true, if index is currently opened. }
    function IsOpen(): Boolean; virtual;
    { Close the index. }
    function Close(): Boolean; virtual;

    { Return the file offset in the data file for the given object id. }
    function GetOffset(const AId: TId; var AOffset: TFileOffset): Boolean;

    { Return the file offsets in the data file for the given object ids. }
    function GetOffsets(const AIds: TIdArray;
      var AOffsets: TFileOffsetArray): Boolean;

    procedure DumpStatistics();
  end;

  TDataBlockSpan = record
    { Offset for the first data entry referenced in the file. Data will be read starting from this position }
    StartOffset: TFileOffset;
    { Number of entries to read. }
    Count: Integer;
  end;

  TDataBlockSpanArray = array of TDataBlockSpan;

  { Access to standard format data files.
    Allows to load data objects by offset using various standard library data structures. }

  { TDataFile }

  TDataFile = class(TObject)
  protected
    FFileName: string;
    FFilePath: string; // complete filename for data file
    FItemClass: TMapObjectClass;
    FTypeConfig: TTypeConfig;

    //FCache: TValueCache;
    FScanner: TFileScanner; // File stream to the data file
    //FAccessMutex: TMutex; // Mutex to secure multi-thread access

    { Read one data value from the current position of the stream
      Method is NOT thread-safe. }
    function ReadData(ATypeConfig: TTypeConfig;
      AScanner: TFileScanner;
      AData: TMapObject): Boolean; overload;

    { Read one data value from the given file offset.
      Method is NOT thread-safe. }
    function ReadData(ATypeConfig: TTypeConfig;
      AScanner: TFileScanner;
      AOffset: TFileOffset;
      AData: TMapObject): Boolean; overload;
  public

    constructor Create(const AFileName: string; ACacheSize: Integer);
    destructor Destroy(); override;

    { Open the index file. }
    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean;

    { Return true, if index is currently opened. }
    function IsOpen(): Boolean; virtual;
    { Close the index. }
    function Close(): Boolean; virtual;

    // ValueType = T

    function GetByOffset(AOffset: TFileOffset; var AEntry: TMapObject): Boolean; overload;

    { Reads data for the given file offsets. File offsets are passed by iterator over
      some container. the size parameter hints as the number of entries returned by the iterators
      and is used to preallocate enough room in the result vector.
    AOffsets - file offsets array
    AData - vector containing data. Data is appended.
    Result - false if there was an error, else true
    (Method is thread-safe) }
    function GetByOffset(const AOffsets: TFileOffsetArray;
      AData: TMapObjectList): Boolean; overload;

    { Read data values from the given file offsets. }
    function GetByOffset(const AOffsets: TFileOffsetArray;
      const ABoundingBox: TGeoBox;
      AData: TMapObjectList): Boolean; overload;

    function GetByBlockSpan(const ASpan: TDataBlockSpan;
      AData: TMapObjectList): Boolean;

    {function GetByOffset(const AOffsets: TFileOffsetArray;
      var AData: TMap<TFileOffset, TMapObject>): Boolean; overload;  }

    function GetByBlockSpans(const AOffsets: TFileOffsetArray;
      AData: TMapObjectList): Boolean;

    property FilePath: string read FFilePath;
    // Basename part of the data file name
    property FileName: string read FFileName;
  end;

  { Extension of DataFile to allow loading data not only by offset but
    by Id using an additional index file, mapping objects Id to object
    file offset. }

  { TIndexedDataFile }

  TIndexedDataFile = class(TDataFile)
  private
    // TValueType = TMapObject
    // TDataIndex = TNumericIndex<TIdType>

    FIndex: TNumericIndexFile;
    //FTypeConfig: TTypeConfig;

  public
    constructor Create(const ADataFile: string;
      const AIndexFile: string;
      AIndexCacheSize: Integer;
      ADataCacheSize: Integer);
    destructor Destroy(); override;

    function Open(const ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMapedIndex: Boolean;
      AMemoryMapedData: Boolean): Boolean;

    function Close(): Boolean; override;
    function IsOpen(): Boolean; override;

    function GetOffset(AId: TId; var AOffset: TFileOffset): Boolean;

    function GetOffsets(const AIds: TIdArray;
      var AOffsets: TFileOffsetArray): Boolean; overload;

    function Get(AId: TId; var AEntry: TMapObject): Boolean; overload;

    function Get(const AIds: TIdArray;
      AData: TMapObjectList): Boolean; overload;

    {function Get(const AIds: TIdArray;
      var AData: map of <TId, TMapObject>): Boolean; overload;}
  end;

  { TAreaDataFile }

  TAreaDataFile = class(TDataFile)
  public
    procedure AfterConstruction; override;
  end;

  { TNodeDataFile }

  TNodeDataFile = class(TDataFile)
  public
    procedure AfterConstruction; override;
  end;

  { TWayDataFile }

  TWayDataFile = class(TDataFile)
  public
    procedure AfterConstruction; override;
  end;

  { TBoundingBoxDataFile }

  TBoundingBoxDataFile = object
  private
    FIsLoaded: Boolean;
    FFileName: string;
    FBoundingBox: TGeoBox;
  public
    procedure Init();
    { Load the bounding box data and return, if this operation was successful.
      APath - Directory, wehre the data file has been placed
      Result - True on success, else false }
    function Load(const APath: string): Boolean;
    property IsLoaded: Boolean read FIsLoaded;
    property FileName: string read FFileName;
    property BoundingBox: TGeoBox read FBoundingBox;
  end;

  { Base class for index files, specialized to getting items for specified area }

  { TAreaIndexFile }

  TAreaIndexFile = class
  private
    FDataFileName: string;
  protected
    FFileName: string; // name of the data file
    FFilePath: string; // Full path and name of the data file
    FScanner: TFileScanner; // Scanner instance for reading this file
    //FLookupMutex: TMutex;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean; virtual;

    function IsOpen(): Boolean; virtual;
    function Close(): Boolean; virtual;

    function GetOffsets(const ABoundingBox: TGeoBox;
      const ARequestedTypes: TTypeInfoSet;
      var AOffsets: TFileOffsetArray;
      var ALoadedTypes: TTypeInfoSet): Boolean; virtual; abstract;

    procedure DumpStatistics(); virtual;

    property FileName: string read FDataFileName;
  end;

  { AreaWayIndex allows you to find ways and way relations in
    a given area.

    Ways can be limited by type and result count. }

  { TAreaNodeIndex }

  TAreaNodeIndex = class(TAreaIndexFile)
  private
    type
      TListTile = record
        FileOffset: TFileOffset;
        EntryCount: Word;
        StoreGeoCoord: Boolean;
      end;

      TBitmapTile = record
        FileOffset: TFileOffset;
        DataOffsetBytes: Byte;
        Magnification: TMagnificationLevel;
      end;

      { todo: initialize TTypeData }
      TTypeData = record
        IsComplex: Boolean;
        BoundingBox: TGeoBox;
        IndexOffset: TFileOffset;
        EntryCount: Word;

        ListTiles: array of TListTile;
        BitmapTiles: array of TBitmapTile;

        ListTilesHash: TSimpleStringHash;
        BitmapTilesHash: TSimpleStringHash;
      end;

  private
    FGridMag: TMagnificationLevel;
    FNodeTypeData: array of TTypeData;

    { emulate hashmap TTileId : TListTile for TTypeData }
    procedure SetTypeDataListTilesItem(var ATypeData: TTypeData;
        const AItemKey: TTileId; const AItemValue: TListTile);
    function GetTypeDataListTilesItem(const ATypeData: TTypeData;
        const AItemKey: TTileId; var AItemValue: TListTile): Boolean;

    { emulate hashmap TTileId : TBitmapTile for TTypeData }
    procedure SetTypeDataBitmapTilesItem(var ATypeData: TTypeData;
        const AItemKey: TTileId; const AItemValue: TBitmapTile);
    function GetTypeDataBitmapTilesItem(const ATypeData: TTypeData;
        const AItemKey: TTileId; var AItemValue: TBitmapTile): Boolean;

    function GetOffsetsList(const ATypeData: TTypeData;
      const ABoundingBox: TGeoBox;
      var AOffsets: TFileOffsetArray): Boolean;

    function GetOffsetsTileList(const ATypeData: TTypeData;
      const ABoundingBox: TGeoBox;
      var AOffsets: TFileOffsetArray): Boolean;

    function GetOffsetsBitmap(const ATypeData: TTypeData;
      const ABoundingBox: TGeoBox;
      var AOffsets: TFileOffsetArray): Boolean;

  public
    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean; override;

    function GetOffsets(const ABoundingBox: TGeoBox;
      const ARequestedTypes: TTypeInfoSet;
      var AOffsets: TFileOffsetArray;
      var ALoadedTypes: TTypeInfoSet): Boolean; override;
  end;

  TAreaWayIndex = class(TAreaIndexFile)
  private
    type
      TTypeData = packed record
        TypeInfo: TTypeInfo;
        IndexLevel: LongWord;

        DataOffsetBytes: Byte;
        BitmapOffset: TFileOffset;
        CellXStart: LongWord;
        CellXEnd: LongWord;
        CellYStart: LongWord;
        CellYEnd: LongWord;
        CellXCount: LongWord;
        CellYCount: LongWord;
        CellWidth: LongWord;
        CellHeight: LongWord;

        MinLon: TLongitude;
        MaxLon: TLongitude;
        MinLat: TLatitude;
        MaxLat: TLatitude;
      end;
  private
    FNodeTypeData: array of TTypeData;
  end;

  { Data structure for every index cell of our index. }
  TAreaIndexCell = record
    // File index of each of the four children, or 0 if there is no child
    children: array[0..3] of TFileOffset;
    // The file index at which the data payload starts
    data: TFileOffset;
  end;

  { TAreaIndexCache }

  TAreaIndexCache = object
  private
    Items: array of TAreaIndexCell;
    Cache: TSimpleCache;

  public
    procedure Init(AMaxSize: Integer);
    procedure Clear();

    function GetEntry(const AKey: TFileOffset; var AValue: TAreaIndexCell): Boolean;
    procedure SetEntry(const AKey: TFileOffset; const AValue: TAreaIndexCell);

    procedure DumpStatistics(ACacheName: string);
  end;

  TAreaCell = record
    Offset: TFileOffset;
    X: Integer;
    Y: Integer;
  end;
  TAreaCellArray = array of TAreaCell;

  { AreaAreaIndex allows you to find areas in
    a given region.

    For areas result can be limited by the maximum level (which in turn
    defines the minimum size of the resulting areas since an area in a given
    level must fit into the cell size (but can cross cell borders)) and the
    maximum number of areas found.

    Internally the index is implemented as quadtree. As a result each index entry
    has 4 children (besides entries in the lowest level). }

  { TAreaAreaIndex }

  TAreaAreaIndex = class(TAreaIndexFile)
  private
    FMaxLevel: Cardinal;       // Maximum level in index
    FTopLevelOffset: TFileOffset; // File offset of the top level index entry

    FIndexCache: TAreaIndexCache;     // Cached map of all index entries by file offset
    //function GetIndexCacheValueSize(): Integer; // SizeOf(TIndexCell)
    function AreaCell(AOffset: TFileOffset; AX, AY: Integer): TAreaCell;

    function GetAreaIndexCell(ALevel: Cardinal;
      AOffset: TFileOffset;
      out AIndexCell: TAreaIndexCell;
      out ADataOffset: TFileOffset): Boolean;

    function ReadCellData(ATypeConfig: TTypeConfig;
      const ATypes: TTypeInfoSet;
      ADataOffset: TFileOffset;
      var ASpans: TDataBlockSpanArray): Boolean;

    procedure PushCellsForNextLevel(
      AMinLon, AMinLat, AMaxLon, AMaxLat: TReal;
      const ACellIndexData: TAreaIndexCell;
      const ACellDimension: TCellDimension;
      Acx, Acy: Integer;
      var ANextCellRefs: TAreaCellArray);

  public
    constructor Create(ACacheSize: Integer);

    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean; override;

    { Returns references in form of DataBlockSpans to all areas within the
      given area,
      ATypeConfig - Type configuration
      AMaxLevel - The maximum index level to load areas from
      ATypes - Set of types to load data for
      ASpans - List of DataBlockSpans referencing the the found areas }
    function GetAreasInArea(ATypeConfig: TTypeConfig;
      const ABoundingBox: TGeoBox;
      AMaxLevel: Integer;
      const ATypes: TTypeInfoSet;
      var ASpans: TDataBlockSpanArray;
      var ALoadedTypes: TTypeInfoSet): Boolean;

    procedure DumpStatistics(); override;
  end;

const
  AREA_NODE_IDX = 'areanode.idx';
  AREA_AREA_IDX = 'areaarea.idx';

implementation

{ TDataFile }

function TDataFile.ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner;
  AData: TMapObject): Boolean;
begin
  try
    AData.Read(ATypeConfig, AScanner);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TDataFile.ReadData(): ', E.Message);
      Result := False;
    end;
  end;
end;

function TDataFile.ReadData(ATypeConfig: TTypeConfig; AScanner: TFileScanner;
  AOffset: TFileOffset; AData: TMapObject): Boolean;
begin
  try
    AScanner.Position := AOffset;

    AData.Read(ATypeConfig, AScanner);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TDataFile.ReadData(): ', E.Message);
      Result := False;
    end;
  end;
end;

constructor TDataFile.Create(const AFileName: string; ACacheSize: Integer);
begin
  inherited Create;
  FFileName := AFileName;
  //FCache.Init(ACacheSize);
  FScanner := TFileScanner.Create();
end;

destructor TDataFile.Destroy();
begin
  if IsOpen() then
    Close();
  FreeAndNil(FScanner);
  inherited Destroy();
end;

function TDataFile.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMappedData: Boolean): Boolean;
begin
  FTypeConfig := ATypeConfig;

  FFilePath := IncludeTrailingPathDelimiter(APath) + FFileName;

  try
    FScanner.Open(FFilePath, fsmLowMemRandom, AMemoryMappedData);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TDataFile.Open(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TDataFile.IsOpen(): Boolean;
begin
  Result := Assigned(FScanner) and FScanner.IsOpen;
end;

function TDataFile.Close(): Boolean;
begin
  FTypeConfig := nil;

  try
    if FScanner.IsOpen then
      FScanner.Close();
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TDataFile.Close(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;

end;

function TDataFile.GetByOffset(AOffset: TFileOffset;
  var AEntry: TMapObject): Boolean;
{$ifdef USE_CACHE}
var
  entryRef: TValueCacheRef;
{$endif}
begin
  //std::lock_guard<std::mutex> lock(accessMutex);
  {$ifdef USE_CACHE}
  if FCache.GetEntry(iOffset, entryRef) then
    AEntry := entryRef.Value
  else
  {$endif}
  begin
    AEntry := FItemClass.Create();

    if ReadData(FTypeConfig, FScanner, AOffset, AEntry) then
    begin
      {$ifdef USE_CACHE}
      FCache.SetEntry(ValueCacheEntry(AOffset, AEntry));
      {$endif}
    end
    else
    begin
      FreeAndNil(AEntry);
      WriteLn(LogFile, 'Error while reading data from offset ', AOffset,
        ' of file ', FFilePath, '!');
      Result := False;
    end;
  end;
end;

function TDataFile.GetByOffset(const AOffsets: TFileOffsetArray;
  AData: TMapObjectList): Boolean;
var
  iSize: Integer;
  iOffset: TFileOffset;
  //entryRef: TValueCacheRef;
  Item: TMapObject;
begin
  Result := True;
  iSize := Length(AOffsets);
  if (iSize = 0) then
    Exit;

  AData.Capacity := AData.Count + iSize;
  //std::lock_guard<std::mutex> lock(accessMutex);

  {$ifdef USE_CACHE}
  if (FCache.GetMaxSize() > 0) and (iSize > FCache.GetMaxSize()) then
  begin
    WriteLn(LogFile, 'Warn: Cache size (', FCache.GetMaxSize(),
      ') for file ', FDataFile,
      ' is smaller than current request (', iSize, ')');
  end;
  {$endif}

  for iOffset in AOffsets do
  begin
    {$ifdef USE_CACHE}
    if FCache.GetEntry(iOffset, entryRef) then
      AData.Add(entryRef.Value)
    else
    {$endif}
    begin
      Item := FItemClass.Create();

      if ReadData(FTypeConfig, FScanner, iOffset, Item) then
      begin
        {$ifdef USE_CACHE}
        FCache.SetEntry(ValueCacheEntry(iOffset, value));
        {$endif}
        AData.Add(Item);
      end
      else
      begin
        Item.Free();
        WriteLn(LogFile, 'Error while reading data from offset ', iOffset,
          ' of file ', FFilePath, '!');
        Result := False;
      end;
    end;
  end;
end;

function TDataFile.GetByOffset(const AOffsets: TFileOffsetArray;
  const ABoundingBox: TGeoBox; AData: TMapObjectList): Boolean;
var
  iOffset: TFileOffset;
  //entryRef: TValueCacheRef;
  Item: TMapObject;
  iSize, inBoxCount, hitRate: Integer;
begin
  Result := True;
  iSize := Length(AOffsets);
  if (iSize = 0) then
    Exit;

  AData.Capacity := AData.Count + iSize;
  //std::lock_guard<std::mutex> lock(accessMutex);

  {$ifdef USE_CACHE}
  if (FCache.GetMaxSize() > 0) and (iSize > FCache.GetMaxSize()) then
  begin
    WriteLn(LogFile, 'Warn: Cache size (', FCache.GetMaxSize(),
      ') for file ', FDataFile,
      ' is smaller than current request (', iSize, ')');
  end;
  {$endif}

  inBoxCount := 0;
  for iOffset in AOffsets do
  begin
    Item := nil;
    {$ifdef USE_CACHE}
    if FCache.GetEntry(iOffset, entryRef) then
      Item := entryRef.Value)
    else
    {$endif}
    begin
      Item := FItemClass.Create();

      if ReadData(FTypeConfig, FScanner, iOffset, Item) then
      begin
        {$ifdef USE_CACHE}
        FCache.SetEntry(ValueCacheEntry(iOffset, value));
        {$endif}
      end
      else
      begin
        FreeAndNil(Item);
        WriteLn(LogFile, 'Error while reading data from offset ', iOffset,
          ' of file ', FFilePath, '!');
        Result := False;
        Exit;
      end;

      if Assigned(Item) and Item.Intersects(ABoundingBox) then
      begin
        Inc(inBoxCount);
        AData.Add(Item);
      end
      else
      begin
        Continue;
      end;
    end;
  end;


  hitRate := (inBoxCount * 100) div iSize;
  if (iSize > 100) and (hitRate < 50) then
  begin
    WriteLn(LogFile, 'Warn: Bounding box hit rate for file ', FileName,
      ' is only ', hitRate, '% (', inBoxCount, '/', iSize, ')');
  end;

end;

function TDataFile.GetByBlockSpan(const ASpan: TDataBlockSpan;
  AData: TMapObjectList): Boolean;
begin
  Assert(False, 'Not implemented!');
end;

function TDataFile.GetByBlockSpans(const AOffsets: TFileOffsetArray;
  AData: TMapObjectList): Boolean;
begin
  Assert(False, 'Not implemented!');
end;

{ TAreaDataFile }

procedure TAreaDataFile.AfterConstruction;
begin
  inherited AfterConstruction;
  FItemClass := TMapArea;
  FFileName := 'areas.dat';
end;

{ TNodeDataFile }

procedure TNodeDataFile.AfterConstruction;
begin
  inherited AfterConstruction;
  FItemClass := TMapNode;
  FFileName := 'nodes.dat';
end;

{ TWayDataFile }

procedure TWayDataFile.AfterConstruction;
begin
  inherited AfterConstruction;
  FItemClass := TMapWay;
  FFileName := 'ways.dat';
end;

{ TIndexedDataFile }

constructor TIndexedDataFile.Create(const ADataFile: string;
  const AIndexFile: string; AIndexCacheSize: Integer; ADataCacheSize: Integer);
begin
  inherited Create(ADataFile, ADataCacheSize);
  FIndex := TNumericIndexFile.Create(AIndexFile, AIndexCacheSize);
end;

destructor TIndexedDataFile.Destroy();
begin
  Close();
  FreeAndNil(FIndex);
  inherited Destroy();
end;

function TIndexedDataFile.Open(const ATypeConfig: TTypeConfig;
  const APath: string; AMemoryMapedIndex: Boolean;
  AMemoryMapedData: Boolean): Boolean;
begin
  Result := inherited Open(ATypeConfig, APath, AMemoryMapedData);
  if Result then
    Result := FIndex.Open(ATypeConfig, APath, AMemoryMapedIndex);
end;

function TIndexedDataFile.Close(): Boolean;
begin
  Result := inherited Close();
  Result := Result and FIndex.Close();
end;

function TIndexedDataFile.IsOpen(): Boolean;
begin
  Result := inherited IsOpen() and FIndex.IsOpen();
end;

function TIndexedDataFile.GetOffset(AId: TId;
  var AOffset: TFileOffset): Boolean;
begin
  Result := FIndex.GetOffset(AId, AOffset);
end;

function TIndexedDataFile.Get(AId: TId; var AEntry: TMapObject): Boolean;
var
  offset: TFileOffset;
begin
  Result := FIndex.GetOffset(AId, offset);
  if Result then
    Result := GetByOffset(offset, AEntry);
end;

function TIndexedDataFile.GetOffsets(const AIds: TIdArray;
  var AOffsets: TFileOffsetArray): Boolean;
begin
  Result := FIndex.GetOffsets(AIds, AOffsets);
end;

function TIndexedDataFile.Get(const AIds: TIdArray;
  AData: TMapObjectList): Boolean;
var
  offsets: TFileOffsetArray;
begin
  Result := FIndex.GetOffsets(AIds, offsets);
  if Result then
    Result := GetByOffset(offsets, AData);
end;

{ TNumericIndexFile }

constructor TNumericIndexFile.Create(const AFileName: string;
  ACacheSize: Integer);
begin
  inherited Create;
  FFileName := AFileName;
  FCacheSize := ACacheSize;
  FPageSize := 0;
  FLevels := 0;
  SetLength(FBuffer, 0);
  FScanner := TFileScanner.Create();
end;

destructor TNumericIndexFile.Destroy();
begin
  Close();
  SetLength(FBuffer, 0);
  FreeAndNil(FScanner);
  inherited Destroy();
end;

function TNumericIndexFile.GetCacheValueSize(const AValue: TPage): Integer;
begin
  Result := SizeOf(AValue) + (SizeOf(TEntry) * Length(AValue.Entries));
end;

function TNumericIndexFile.PageIndexIsValid(const APage: TPage;
  AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= Length(APage.Entries));
end;

function TNumericIndexFile.GetPageIndex(const APage: TPage; AId: TId): Integer;
var
  iLeft, iRight, iMid: Integer;
begin
  Result := Length(APage.Entries);

  //std::cout << "Lookup in page: " << page.entries[0].startId << " " << id << " " << page.entries[page.entries.size()-1].startId << std::endl;

  if (Result > 0) then
  begin
    iLeft := 0;
    iRight := Result - 1;

    while (iLeft <= iRight) do
    begin
      iMid := (iLeft + iRight) div 2;
      if (APage.Entries[iMid].StartId <= AId)
      and ((iMid+1 >= Result) or (APage.Entries[iMid+1].StartId > AId))
      then
      begin
        Result := iMid;
        Break;
      end
      else if (APage.Entries[iMid].StartId < AId) then
        iLeft := iMid + 1
      else
      begin
        if (iMid = 0) then
          Break;

        iRight := iMid - 1;
      end;
    end;
  end;
end;

procedure TNumericIndexFile.ReadPage(AOffset: TFileOffset; var APage: TPage);
var
  iCount: Integer;
  currentPos, idBytes, fileOffsetBytes: Integer;
  prevId, curId: TId;
  prefFileOffset, curFileOffset: TFileOffset;
  entry: TEntry;
begin
  iCount := 0;
  SetLength(APage.Entries, FPageSize div 4);

  FScanner.Position := AOffset;
  FScanner.Read(FBuffer[0], LongWord(FPageSize));

  currentPos := 0;
  prevId := 0;
  prefFileOffset := 0;

  //std::cout << "Page: " << offset << std::endl;

  while (currentPos < FPageSize) and (FBuffer[currentPos] <> 0) do
  begin
    idBytes := DecodeNumber(FBuffer[currentPos], curId);
    currentPos := currentPos + idBytes;

    fileOffsetBytes := DecodeNumber(FBuffer[currentPos], curFileOffset);
    currentPos := currentPos + fileOffsetBytes;

    entry.StartId := prevId + curId;
    entry.FileOffset := prefFileOffset + curFileOffset;

    //std::cout << "- " << entry.startId << " " << idBytes << " " << entry.fileOffset << " " << fileOffsetBytes << std::endl;

    prevId := entry.StartId;
    prefFileOffset := entry.FileOffset;

    if Length(APage.Entries) <= iCount then
      SetLength(APage.Entries, iCount + 4);
    APage.Entries[iCount] := entry;
    Inc(iCount);
  end;

  SetLength(APage.Entries, iCount);
end;

procedure TNumericIndexFile.InitializeCache();
begin
  // !!!
end;

function TNumericIndexFile.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMapped: Boolean): Boolean;
var
  iEntries: LongWord;
  iLevel: Integer;
  lastLevelPageStart, indexPageCountsOffset: TFileOffset;
begin
  FFilePath := IncludeTrailingPathDelimiter(APath) + FFileName;

  try
    FScanner.Open(FFilePath, fsmFastRandom, AMemoryMapped);

    FScanner.ReadNumber(LongWord(FPageSize));       // Size of one index page
    FScanner.ReadNumber(iEntries);                  // Number of entries in data file

    FScanner.Read(LongWord(FLevels));               // Number of levels
    FScanner.ReadFileOffsetValue(lastLevelPageStart);    // Start of top level index page
    FScanner.ReadFileOffsetValue(indexPageCountsOffset); // Start of list of sizes of index levels

    Result := not FScanner.HasError;
    if (not Result) then
    begin
      WriteLn(LogFile, 'Error while loading header data of index file: ', FFileName);
      Exit;
    end;

    SetLength(FPageCounts, FLevels);

    FScanner.Position := indexPageCountsOffset;
    for iLevel := 0 to FLevels-1 do
      FScanner.ReadNumber(FPageCounts[iLevel]);


    SetLength(FBuffer, FPageSize);

    //std::cout << "Index " << filename << ": " << entries << " entries to index, " << levels << " levels, pageSize " << pageSize << ", cache size " << cacheSize << std::endl;

    ReadPage(lastLevelPageStart, FRoot);

    InitializeCache();
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TNumericIndexFile.Open(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TNumericIndexFile.IsOpen(): Boolean;
begin
  Result := FScanner.IsOpen;
end;

function TNumericIndexFile.Close(): Boolean;
begin
  try
    if FScanner.IsOpen then
      FScanner.Close();
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TNumericIndexFile.Close(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TNumericIndexFile.GetOffset(const AId: TId;
  var AOffset: TFileOffset): Boolean;
var
  r, iLevel, i: Integer;
  page: TPage;
  rootEntry, entry: TEntry;
  startId: Tid;
begin
  Result := False;
  try
    //std::lock_guard<std::mutex> lock(accessMutex);

    //std::cout << "Looking up " << id << " in index...." << std::endl;

    r := GetPageIndex(FRoot, AId);

    if (not PageIndexIsValid(FRoot, r)) then
    begin
      //std::cerr << "Id " << id << " not found in root index, " << root->entries.front().startId << "-" << root->entries.back().startId << std::endl;
      Exit;
    end;

    rootEntry := FRoot.Entries[r];

    //std::cout << "Root entry index: " << r << " " << rootEntry.startId << " " << rootEntry.fileOffset << std::endl;

    AOffset := rootEntry.FileOffset;

    startId := rootEntry.StartId;
    for iLevel := 0 to FLevels-2 do
    begin
      //std::cout << "Level " << iLevel << "/" << FLevels << std::endl;
      if (iLevel <= FSimpleCacheMaxLevel) then
      begin
        {$ifdef USE_CACHE}
        if not simplePageCache[level].find(startId, page) then
        {$endif}
        begin
          //pageRef := NULL; // Make sure, that we allocate a new page and not reuse an old one

          ReadPage(AOffset, page);

          {$ifdef USE_CACHE}
          simplePageCache[level].insert(startId, page);
          {$endif}
        end;
      end
      else
      begin
        {$ifdef USE_CACHE}
        if not pageCaches[level].find(startId, page) then
        {$endif}
        begin
          ReadPage(AOffset, page);

          {$ifdef USE_CACHE}
          pageCaches[level].insert(startId, page);
          {$endif}
        end;
      end;

      i := GetPageIndex(page, AId);

      if (not PageIndexIsValid(page, i)) then
      begin
        //std::cerr << "Id " << id << " not found in index level " << level+2 << "!" << std::endl;
        Exit;
      end;

      entry := page.Entries[i];

      //std::cout << "Sub entry index: " << i << " " << entry.startId << " " << entry.fileOffset << std::endl;

      startId := entry.StartId;
      AOffset := entry.FileOffset;
    end;

    if (startId <> AId) then
    begin
      //std::cerr << "Id " << id << " not found in leaf index level (" << levels << " levels)" << std::endl;
    end;

    Result := (startId = AId);
  except
    on E: Exception do
      WriteLn(LogFile, 'TNumericIndexFile.GetOffset(): ', E.Message);
  end;
end;

function TNumericIndexFile.GetOffsets(const AIds: TIdArray;
  var AOffsets: TFileOffsetArray): Boolean;
var
  TmpId: TId;
  n: Integer;
  offset: TFileOffset;
begin
  SetLength(AOffsets, Length(AIds));

  n := 0;
  for TmpId in AIds do
  begin
    if GetOffset(TmpId, offset) then
      AOffsets[n] := offset;
    Inc(n);
  end;

  SetLength(AOffsets, n);
  Result := True;
end;

procedure TNumericIndexFile.DumpStatistics();
var
  i, memory, pages: Integer;
begin
  memory := 0;
  pages := 0;

  Inc(pages);
  memory := memory + Length(FRoot.entries) * SizeOf(TEntry);

  {$ifdef USE_CACHE}
  for i := 0 to Length(pageCaches) do
  begin
    pages+=pageCaches[i].GetSize();
    memory+=sizeof(pageCaches[i])+pageCaches[i].GetMemory(NumericIndexCacheValueSizer());
  end;
  {$endif}

  WriteLn(LogFile, Format('Index %s: %d pages, memory %d', [FFileName, pages, memory]));
end;

{ TBoundingBoxDataFile }

procedure TBoundingBoxDataFile.Init();
begin
  FFileName := 'bounding.dat';
  FIsLoaded := False;
end;

function TBoundingBoxDataFile.Load(const APath: string): Boolean;
var
  scanner: TFileScanner;
  FilePath: string;
begin
  FIsLoaded := false;
  FilePath := IncludeTrailingPathDelimiter(APath) + FFileName;

  scanner := TFileScanner.Create();
  try
    scanner.Open(FilePath, fsmSequential, True);

    FBoundingBox.ReadFromStream(scanner.Stream);

    WriteLn(LogFile, 'BoundingBox: ', BoundingBox.GetDisplayText());

    scanner.Close();

    FIsLoaded := True;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TBoundingBoxDataFile.Load(): ', E.Message);
      scanner.CloseFailsafe();
      Result := False;
    end;
  end;
  scanner.Free();
end;

{ TAreaIndexFile }

constructor TAreaIndexFile.Create;
begin
  inherited Create;
  FScanner := TFileScanner.Create();
end;

destructor TAreaIndexFile.Destroy;
begin
  FreeAndNil(FScanner);
  inherited Destroy;
end;

function TAreaIndexFile.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMappedData: Boolean): Boolean;
begin
  Assert(FFileName <> '');
  FFilePath := IncludeTrailingPathDelimiter(APath) + FFileName;

  try
    FScanner.Open(FFilePath, fsmFastRandom, AMemoryMappedData);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TAreaIndexFile.Open(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

function TAreaIndexFile.IsOpen(): Boolean;
begin
  Result := FScanner.IsOpen;
end;

function TAreaIndexFile.Close(): Boolean;
begin
  try
    if FScanner.IsOpen then
      FScanner.Close();
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'TAreaIndexFile.Close(): ', E.Message);
      FScanner.CloseFailsafe();
      Result := False;
    end;
  end;
end;

procedure TAreaIndexFile.DumpStatistics();
begin
  // todo..
end;

{ TAreaNodeIndex }

function TAreaNodeIndex.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMappedData: Boolean): Boolean;
var
  gridMag, tileEntryCount, bitmapEntryCount: UInt32;
  indexEntryCount: UInt16;
  i: Integer;
  typeId: TTypeId;
  minCoord, maxCoord: TGeoPoint;
  x, y: UInt32;
  TileEntry: TListTile;
  BitmapEntry: TBitmapTile;
  btMagnification: Byte;
begin
  FFileName := AREA_NODE_IDX;
  Result := inherited Open(ATypeConfig, APath, AMemoryMappedData);

  if not Result then Exit;

  try
    FScanner.Read(gridMag);
    Self.FGridMag := gridMag;

    FScanner.Read(indexEntryCount);
    for i := 1 to indexEntryCount do
    begin
      FScanner.ReadNumber(typeId);
      if (typeId >= Length(FNodeTypeData)) then
        SetLength(FNodeTypeData, typeId+1);

      FScanner.Read(FNodeTypeData[typeId].IsComplex);
      minCoord.ReadFromStream(FScanner.Stream);
      maxCoord.ReadFromStream(FScanner.Stream);

      FNodeTypeData[typeId].BoundingBox.SetValue(minCoord, maxCoord);

      if (not FNodeTypeData[typeId].IsComplex) then
      begin
        FScanner.ReadFileOffsetValue(FNodeTypeData[typeId].IndexOffset);
        FScanner.Read(FNodeTypeData[typeId].EntryCount);
      end
      else
      begin
        FNodeTypeData[typeId].IndexOffset := 0;
        FNodeTypeData[typeId].EntryCount := 0;
      end;
    end;

    { read tiles entry }
    FScanner.Read(tileEntryCount);
    for i := 1 to tileEntryCount do
    begin
      FScanner.ReadNumber(typeId);
      if (typeId >= Length(FNodeTypeData)) then
        SetLength(FNodeTypeData, typeId+1);

      FScanner.Read(x);
      FScanner.Read(y);

      //TileEntry := FNodeTypeData[typeId].ListTiles[GetTileId(x, y)];

      FScanner.ReadFileOffsetValue(TileEntry.FileOffset);
      FScanner.Read(TileEntry.EntryCount);
      FScanner.Read(TileEntry.StoreGeoCoord);

      //FNodeTypeData[typeId].ListTiles := Concat(FNodeTypeData[typeId].ListTiles, [TileEntry]);
      SetTypeDataListTilesItem(FNodeTypeData[typeId], GetTileId(x, y), TileEntry);
    end;

    FScanner.Read(bitmapEntryCount);

    for i := 1 to bitmapEntryCount do
    begin
      FScanner.ReadNumber(typeId);
      if (typeId >= Length(FNodeTypeData)) then
        SetLength(FNodeTypeData, typeId+1);

      FScanner.Read(x);
      FScanner.Read(y);

      //BitmapTile& entry=nodeTypeData[typeId].bitmapTiles[TileId(x,y)];

      FScanner.ReadFileOffsetValue(BitmapEntry.FileOffset);
      FScanner.Read(BitmapEntry.DataOffsetBytes);
      FScanner.Read(btMagnification);
      BitmapEntry.Magnification := btMagnification;

      FNodeTypeData[typeId].BitmapTiles := Concat(FNodeTypeData[typeId].BitmapTiles, [BitmapEntry]);
    end;

    Result := (not FScanner.HasError);
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'Error: ', E.ToString());
      Result := False;
    end;
  end;
end;

procedure TAreaNodeIndex.SetTypeDataListTilesItem(var ATypeData: TTypeData;
  const AItemKey: TTileId; const AItemValue: TListTile);
var
  s: string;
  n: Integer;
begin
  s := AItemKey.GetDisplayText();
  if not ATypeData.ListTilesHash.FindValue(s, n) then
  begin
    n := Length(ATypeData.ListTiles);
    SetLength(ATypeData.ListTiles, n+1);
    ATypeData.ListTilesHash.Add(s, n);
  end;
  ATypeData.ListTiles[n] := AItemValue;
end;

function TAreaNodeIndex.GetTypeDataListTilesItem(const ATypeData: TTypeData;
  const AItemKey: TTileId; var AItemValue: TListTile): Boolean;
var
  s: string;
  n: Integer;
begin
  s := AItemKey.GetDisplayText();
  Result := ATypeData.ListTilesHash.FindValue(s, n);
  if Result then
    AItemValue := ATypeData.ListTiles[n];
end;

procedure TAreaNodeIndex.SetTypeDataBitmapTilesItem(var ATypeData: TTypeData;
  const AItemKey: TTileId; const AItemValue: TBitmapTile);
var
  s: string;
  n: Integer;
begin
  s := AItemKey.GetDisplayText();
  if not ATypeData.BitmapTilesHash.FindValue(s, n) then
  begin
    n := Length(ATypeData.BitmapTiles);
    SetLength(ATypeData.BitmapTiles, n+1);
    ATypeData.BitmapTilesHash.Add(s, n);
  end;
  ATypeData.BitmapTiles[n] := AItemValue;
end;

function TAreaNodeIndex.GetTypeDataBitmapTilesItem(const ATypeData: TTypeData;
  const AItemKey: TTileId; var AItemValue: TBitmapTile): Boolean;
var
  s: string;
  n: Integer;
begin
  s := AItemKey.GetDisplayText();
  Result := ATypeData.BitmapTilesHash.FindValue(s, n);
  if Result then
    AItemValue := ATypeData.BitmapTiles[n];
end;

function TAreaNodeIndex.GetOffsetsList(const ATypeData: TTypeData;
  const ABoundingBox: TGeoBox; var AOffsets: TFileOffsetArray): Boolean;
var
  previousOffset, fileOffset: TFileOffset;
  i: Integer;
  coord: TGeoPoint;
begin
  FScanner.Position := ATypeData.IndexOffset;

  previousOffset := 0;

  for i := 1 to ATypeData.EntryCount do
  begin
    coord.ReadFromStream(FScanner.Stream);
    FScanner.ReadNumber(fileOffset);

    fileOffset := fileOffset + previousOffset;

    previousOffset := fileOffset;

    if ABoundingBox.IsIncludes(coord) then
      AOffsets := Concat(AOffsets, [fileOffset]);
  end;

  Result := True;
end;

function TAreaNodeIndex.GetOffsetsTileList(const ATypeData: TTypeData;
  const ABoundingBox: TGeoBox; var AOffsets: TFileOffsetArray): Boolean;
var
  tileBox: TTileIdBox;
  tileIdIterator: TTileIdBoxConstIterator;
  tileId: TTileId;
  tile: TListTile;
  previousOffset, fileOffset: TFileOffset;
  i: Integer;
  coord: TGeoPoint;
begin
  tileBox := GetTileIdBox(GetTileId(FGridMag, ABoundingBox.MinCoord),
                          GetTileId(FGridMag, ABoundingBox.MaxCoord));


  tileIdIterator.Init(tileBox.MinTile, tileBox.MinTile, tileBox.MaxTile);
  while tileIdIterator.GetNext(tileId) do
  begin
    if GetTypeDataListTilesItem(ATypeData, tileId, tile) then
    begin
      FScanner.Position := (tile.fileOffset);

      previousOffset := 0;

      if tile.StoreGeoCoord then
      begin
        for i := 1 to tile.EntryCount do
        begin
          coord.ReadFromStream(FScanner.Stream);
          FScanner.ReadNumber(fileOffset);
          fileOffset := fileOffset + previousOffset;
          previousOffset := fileOffset;

          if ABoundingBox.IsIncludes(coord) then
            AOffsets := Concat(AOffsets, [fileOffset]);
        end;
      end
      else
      begin
        for i := 1 to tile.EntryCount do
        begin
          FScanner.ReadNumber(fileOffset);
          fileOffset := fileOffset + previousOffset;
          previousOffset := fileOffset;

          AOffsets := Concat(AOffsets, [fileOffset]);
        end;
      end;
    end;
  end;

  Result := True;

end;

function TAreaNodeIndex.GetOffsetsBitmap(const ATypeData: TTypeData;
  const ABoundingBox: TGeoBox; var AOffsets: TFileOffsetArray): Boolean;
var
  searchTileBox: TTileIdBox;
  tileIdIterator: TTileIdBoxConstIterator;
  tileId: TTileId;
  tileBitmap: TBitmapTile;
  box: TGeoBox;
  MagLev: TMagnificationLevel; // magnification
  bitmapTileBox, boundingBoxTileBox: TTileIdBox;
  dataOffset, initialCellDataOffset, cellIndexOffset: TFileOffset;
  cellDataOffset, previousOffset, fileOffset: TFileOffset;
  minxc, maxxc, minyc, maxyc, y, x, dataCount: UInt32;
  i, d, cellDataOffsetCount: Integer;
begin
  searchTileBox := GetTileIdBox(GetTileId(FGridMag, ABoundingBox.MinCoord),
                                GetTileId(FGridMag, ABoundingBox.MaxCoord));

  tileIdIterator.Init(searchTileBox.MinTile, searchTileBox.MinTile, searchTileBox.MaxTile);
  while tileIdIterator.GetNext(tileId) do
  begin
    if GetTypeDataBitmapTilesItem(ATypeData, tileId, tileBitmap) then
    begin
      MagLev := tileBitmap.Magnification;
      box := tileId.GetBoundingBox(FGridMag);
      bitmapTileBox := GetTileIdBox(GetTileId(MagLev, box.MinCoord),
                                    GetTileId(MagLev, box.MaxCoord));
      boundingBoxTileBox := GetTileIdBox(GetTileId(MagLev, ABoundingBox.MinCoord),
                                         GetTileId(MagLev, ABoundingBox.MaxCoord));

      // Offset of the data behind the bitmap
      dataOffset := tileBitmap.FileOffset + (bitmapTileBox.GetCount() * tileBitmap.DataOffsetBytes);

      minxc := Max(bitmapTileBox.MinX, boundingBoxTileBox.MinX);
      maxxc := Min(bitmapTileBox.MaxX, boundingBoxTileBox.MaxX);

      minyc := Max(bitmapTileBox.MinY, boundingBoxTileBox.MinY);
      maxyc := Min(bitmapTileBox.MaxY, boundingBoxTileBox.MaxY);

      // For each row
      for y := minyc to maxyc do
      begin
        //std::lock_guard<std::mutex> guard(lookupMutex);
        initialCellDataOffset := 0;
        cellDataOffsetCount := 0;
        cellIndexOffset := tileBitmap.FileOffset
          + ((y - bitmapTileBox.MinY) * bitmapTileBox.GetWidth() + minxc - bitmapTileBox.MinX)
          * tileBitmap.DataOffsetBytes;

        FScanner.Position := cellIndexOffset;

        // For each column in row
        for x := minxc  to maxxc do
        begin
          FScanner.ReadFileOffsetValue(cellDataOffset, tileBitmap.DataOffsetBytes);

          if (cellDataOffset = 0) then
            Continue;


          // We added +1 during import and now substract it again
          Dec(cellDataOffset);

          if (initialCellDataOffset = 0) then
            initialCellDataOffset := dataOffset + cellDataOffset;

          Inc(cellDataOffsetCount);
        end;

        if (cellDataOffsetCount = 0) then
          Continue;

        Assert(initialCellDataOffset >= cellIndexOffset);

        FScanner.Position := initialCellDataOffset;

        // For each data cell in row found
        for i := 0 to cellDataOffsetCount-1 do
        begin
          previousOffset := 0;

          FScanner.ReadNumber(dataCount);

          for d := 1 to dataCount do
          begin
            FScanner.ReadNumber(fileOffset);

            fileOffset := fileOffset + previousOffset;

            AOffsets := Concat(AOffsets, [fileOffset]);

            previousOffset := fileOffset;
          end;
        end;
      end;
    end;
  end;

  Result := True;

end;

function TAreaNodeIndex.GetOffsets(const ABoundingBox: TGeoBox;
  const ARequestedTypes: TTypeInfoSet; var AOffsets: TFileOffsetArray;
  var ALoadedTypes: TTypeInfoSet): Boolean;
var
  time: TStopClock;
  TypeInfo: TTypeInfo;
  i: Integer;
  index: TTypeId;
begin
  Result := False;
  ALoadedTypes.Clear();

  //offsets.reserve(std::min((size_t)10000,offsets.capacity()));

  try
    //for (TypeInfo in ARequestedTypes) do
    for i := Low(ARequestedTypes.Types) to High(ARequestedTypes.Types) do
    begin
      TypeInfo := ARequestedTypes.Types[i];
      if TypeInfo = nil then
        Continue;

      if TypeInfo.IsInternal then
        Continue;

      index := TypeInfo.NodeId;
      if (index < Length(FNodeTypeData)) then
      begin
        if (not FNodeTypeData[index].BoundingBox.IsIntersects(ABoundingBox)) then
          // No data available in given bounding box
          Continue;

        if (not FNodeTypeData[index].IsComplex)
        and (FNodeTypeData[index].IndexOffset <> 0)
        and (FNodeTypeData[index].EntryCount <> 0) then
        begin
          if (not GetOffsetsList(FNodeTypeData[index], ABoundingBox, AOffsets)) then
            Exit;
        end
        else if FNodeTypeData[index].IsComplex then
        begin
          if (not GetOffsetsTileList(FNodeTypeData[index], ABoundingBox, AOffsets)) then
            Exit;
          if (not GetOffsetsBitmap(FNodeTypeData[index], ABoundingBox, AOffsets)) then
            Exit;
        end
        else
          Continue;
      end;

      ALoadedTypes.Add(TypeInfo);
    end;
  except
    on E: Exception do
    begin
      WriteLn(LogFile, 'Error: ', E.ToString());

      Result := False;
    end;
  end;

  time.Stop();

  if (time.GetMilliseconds() > 100) then
  begin
    WriteLn(LogFile, 'Warn: Retrieving ', Length(AOffsets),
               ' node offsets from area index for ', ABoundingBox.GetDisplayText(),
               ' took ', time.ResultString());
  end;

  Result := True;
end;

{ TAreaAreaIndex }

function TAreaAreaIndex.AreaCell(AOffset: TFileOffset; AX,
  AY: Integer): TAreaCell;
begin
  Result.offset := AOffset;
  Result.x := AX;
  Result.y := AY;
end;

function TAreaAreaIndex.GetAreaIndexCell(ALevel: Cardinal;
  AOffset: TFileOffset; out AIndexCell: TAreaIndexCell;
  out ADataOffset: TFileOffset): Boolean;
var
  childOffset: TFileOffset;
  i: Integer;
begin
  if (ALevel < FMaxLevel) then
  begin
    //std::lock_guard<std::mutex> guard(lookupMutex);

{$ifdef ANALYZE_CACHE}
    if (indexCache.GetSize()==indexCache.GetMaxSize()) {
      log.Warn() << "areaarea.index cache of " << indexCache.GetSize() << "/" << indexCache.GetMaxSize()
                 << " is too small";
      indexCache.DumpStatistics("areaarea.idx",IndexCacheValueSizer());
    }
{$endif}

    if (not FIndexCache.GetEntry(AOffset, AIndexCell)) then
    begin
      FScanner.Position := AOffset;

      for i := Low(AIndexCell.children) to High(AIndexCell.children) do
      begin
        FScanner.ReadNumber(childOffset);

        if (childOffset = 0) then
          AIndexCell.children[i] := 0
        else
          AIndexCell.children[i] := AOffset - childOffset;
      end;

      AIndexCell.data := FScanner.Position;
      FIndexCache.SetEntry(AOffset, AIndexCell)
    end;
  end
  else
  begin
    AIndexCell.data := AOffset;
    for i := Low(AIndexCell.children) to High(AIndexCell.children) do
    begin
      AIndexCell.children[i] := 0;
    end;
  end;

  ADataOffset := AIndexCell.data;

  Result := True;
end;

function TAreaAreaIndex.ReadCellData(ATypeConfig: TTypeConfig;
  const ATypes: TTypeInfoSet; ADataOffset: TFileOffset;
  var ASpans: TDataBlockSpanArray): Boolean;
var
  typeCount, dataCount: UInt32;
  typeId: TTypeId;
  dataFileOffset, prevDataFileOffset: TFileOffset;
  t: Integer;
  TypeInfo: TTypeInfo;
  span: TDataBlockSpan;
begin
  //std::lock_guard<std::mutex> guard(lookupMutex);

  FScanner.Position := ADataOffset;
  FScanner.ReadNumber(typeCount);
  prevDataFileOffset := 0;

  for t := 0 to typeCount-1 do
  begin
    FScanner.ReadTypeId(typeId, ATypeConfig.AreaTypeIdBytes);
    FScanner.ReadNumber(dataCount);
    FScanner.ReadNumber(dataFileOffset);

    Inc(dataFileOffset, prevDataFileOffset);
    prevDataFileOffset := dataFileOffset;

    if (dataFileOffset = 0) then
      Continue;

    TypeInfo := ATypeConfig.GetAreaTypeInfo(typeId);

    if ATypes.IsSet(TypeInfo) then
    begin
      span.StartOffset := dataFileOffset;
      span.Count := dataCount;

      ASpans := Concat(ASpans, [span]);
    end;
  end;

  Result := True;
end;

procedure TAreaAreaIndex.PushCellsForNextLevel(AMinLon, AMinLat, AMaxLon,
  AMaxLat: TReal; const ACellIndexData: TAreaIndexCell;
  const ACellDimension: TCellDimension; Acx, Acy: Integer;
  var ANextCellRefs: TAreaCellArray);

  procedure _AddCell(AChildIndex: Integer; cx, cy: Integer);
  var
    x, y: TReal;
  begin
    if (ACellIndexData.children[AChildIndex] <> 0) then
    begin
      // top left
      x := cx * ACellDimension.Width;
      y := cy * ACellDimension.Height;

      if not ((x > AMaxLon + ACellDimension.Width/2)
           or (y > AMaxLat + ACellDimension.Height/2)
           or (x + ACellDimension.Width < AMinLon - ACellDimension.Width/2)
           or (y + ACellDimension.Height < AMinLat - ACellDimension.Height/2))
      then
        ANextCellRefs := Concat(ANextCellRefs, [AreaCell(ACellIndexData.children[AChildIndex], cx, cy)]);
    end;
  end;

begin
  _AddCell(0, Acx,   Acy+1);  // top left
  _AddCell(1, Acx+1, Acy+1);  // top right
  _AddCell(2, Acx,   Acy);    // bottom left
  _AddCell(3, Acx+1, Acy);    // bottom left
end;

constructor TAreaAreaIndex.Create(ACacheSize: Integer);
begin
  inherited Create;
  FIndexCache.Init(ACacheSize);

  FMaxLevel := 0;
  FTopLevelOffset := 0;
end;

function TAreaAreaIndex.Open(ATypeConfig: TTypeConfig; const APath: string;
  AMemoryMappedData: Boolean): Boolean;
begin
  FFileName := AREA_AREA_IDX;
  Result := inherited Open(ATypeConfig, APath, AMemoryMappedData);

  if Result then
  begin
    FScanner.ReadNumber(FMaxLevel);
    FScanner.ReadFileOffsetValue(FTopLevelOffset);
    Result := (not FScanner.HasError);
  end;
end;

function TAreaAreaIndex.GetAreasInArea(ATypeConfig: TTypeConfig;
  const ABoundingBox: TGeoBox; AMaxLevel: Integer; const ATypes: TTypeInfoSet;
  var ASpans: TDataBlockSpanArray; var ALoadedTypes: TTypeInfoSet): Boolean;
var
  time: TStopClock;
  cellRef: TAreaCell;
  cellRefs: TAreaCellArray;     // cells to scan in this level
  nextCellRefs: TAreaCellArray; // cells to scan for the next level
  MinLon, MaxLon, MinLat, MaxLat: TReal;
  i, level: Integer;
  cellIndexData: TAreaIndexCell;
  cellDataOffset: TFileOffset;
  cx, cy: Integer;
begin
  Result := False;

  MinLon := ABoundingBox.MinCoord.Lon + 180.0;
  MaxLon := ABoundingBox.MaxCoord.Lon + 180.0;
  MinLat := ABoundingBox.MinCoord.Lat + 90.0;
  MaxLat := ABoundingBox.MaxCoord.Lat + 90.0;

  // Clear result data structures
  //ASpansCount := 0;
  SetLength(ASpans, 0);
  SetLength(cellRefs, 0);
  ALoadedTypes.Init(ATypeConfig);

  // Make the vector preallocate memory for the expected data size
  // This should void reallocation
  //SetLength(ASpans, 1000);

  //cellRefs.reserve(2000);
  //nextCellRefs.reserve(2000);

  cellRefs := Concat(cellRefs, [AreaCell(FTopLevelOffset, 0, 0)]);

  try
    // For all levels:
    // * Take the tiles and offsets of the last level
    // * Calculate the new tiles and offsets that still interfere with given area
    // * Add the new offsets to the list of offsets and finish if we have
    //   reached maxLevel or maxAreaCount.
    // * copy no, ntx, nty to ctx, cty, co and go to next iteration
    level := 0;
    while (level <= FMaxLevel) and (level <= AMaxLevel) and (Length(cellRefs) > 0) do
    begin
      SetLength(nextCellRefs, 0);

      for i := Low(cellRefs) to High(cellRefs) do
      begin
        cellRef := cellRefs[i];
        if (not GetAreaIndexCell(level, cellRef.Offset, cellIndexData, cellDataOffset)) then
        begin
          WriteLn(LogFile, 'Error: Cannot find offset ', cellRef.Offset,
                        ' in level ', level,
                        ' in file "', FScanner.Filename, '"');
          Exit;
        end;

        // Now read the area offsets by type in this index entry

        if (not ReadCellData(ATypeConfig, ATypes, cellDataOffset, ASpans)) then
        begin
          WriteLn(LogFile, 'Error: Cannot read index data for level ', level,
                       ' at offset ', cellDataOffset,
                       ' in file "', FScanner.Filename, '"');

          Exit;
        end;

        if (level < FMaxLevel) then
        begin
          cx := cellRef.X * 2;
          cy := cellRef.Y * 2;

          PushCellsForNextLevel(MinLon, MinLat, MaxLon, MaxLat,
            cellIndexData, GetCellDimension(level+1), cx, cy, nextCellRefs);
        end;
      end;

      //std::swap(cellRefs, nextCellRefs);
      cellRefs := nextCellRefs;

      Inc(level);
    end;
  except
    on E: Exception do
      WriteLn(LogFile, 'Error: ', E.ToString());
  end;

  time.Stop();

  if (time.GetMilliseconds() > 100) then
  begin
    WriteLn(LogFile, 'Warn: Retrieving ', Length(ASpans), ' spans from area index for ',
      ABoundingBox.GetDisplayText(), ' took ', time.ResultString());
  end;

  ALoadedTypes := ATypes;

  Result := True;
end;

procedure TAreaAreaIndex.DumpStatistics();
begin
  inherited DumpStatistics();
  FIndexCache.DumpStatistics(AREA_AREA_IDX);
end;

{ TAreaIndexCache }

procedure TAreaIndexCache.Init(AMaxSize: Integer);
begin
  SetLength(Items, AMaxSize);
  Cache.Init(AMaxSize);
end;

procedure TAreaIndexCache.Clear();
begin
  Cache.Clear();
end;

function TAreaIndexCache.GetEntry(const AKey: TFileOffset;
  var AValue: TAreaIndexCell): Boolean;
var
  n: Integer;
begin
  Result := Cache.GetEntry(IntToStr(AKey), n);
  if Result then
    AValue := Items[n];
end;

procedure TAreaIndexCache.SetEntry(const AKey: TFileOffset;
  const AValue: TAreaIndexCell);
var
  n: Integer;
begin
  Cache.SetEntry(IntToStr(AKey), n);
  Items[n] := AValue;
end;

procedure TAreaIndexCache.DumpStatistics(ACacheName: string);
begin
  WriteLn(LogFile, 'Debug: ', ACacheName, ' entries=', Cache.GetCount(),
    ', memory=', (Cache.GetMaxSize() * SizeOf(TAreaIndexCell)) );
end;

end.

