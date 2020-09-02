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

*)

unit OsMapDataFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OsMapTypes, OsMapFiles, OsMapObjTypes, OsMapObjects, OsMapGeometry, OsMapUtils;

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

    property FileName: string read FDataFileName;
  end;

  { AreaWayIndex allows you to find ways and way relations in
    a given area.

    Ways can be limited by type and result count. }
  TAreaNodeIndex = class(TAreaIndexFile)
  private
    type
      TListTile = packed record
        FileOffset: TFileOffset;
        EntryCount: Word;
        StoreGeoCoord: Boolean;
      end;

      TBitmapTile = packed record
        FileOffset: TFileOffset;
        DataOffsetBytes: Byte;
        Magnification: TMagnificationLevel;
      end;

      TTypeData = packed record
        IsComplex: Boolean;
        BoundingBox: TGeoBox;
        IndexOffset: TFileOffset;
        EntryCount: Word;

        ListTiles: array of TListTile;
        BitmapTiles: array of TBitmapTile;
      end;

  private
    FGridMag: TMagnificationLevel;
    FNodeTypeData: array of TTypeData;
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

  TAreaAreaIndex = class(TAreaIndexFile)

  end;

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

end.
