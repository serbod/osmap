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
  RouteNodeDataFile

routing/RoutingDB
  RoutingDatabase
*)

unit OsMapRoutingDatabase;

//{$mode objfpc}{$H+}
{$mode Delphi}

interface

uses
  Classes, SysUtils,
  OsMapTypes, OsMapGeometry, OsMapRouting, OsMapFiles, OsMapDataFiles,
  OsMapObjTypes;

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

    procedure Init();
    function Load(ATypeConfig: TTypeConfig; AFilename: string): Boolean;
  end;

  TRouteNodeDataFile = class
  private
    // type TValueCache = TCache<Id, IndexPage>
    FDataFile: string;
    FDataFileName: string; // complete filename for data file

    FTypeConfig: TTypeConfig;

    //FIndex: TMap<Pixel, IndexEntry>;
    //FCache: TValueCache;
    FScanner: TFileScanner; // File stream to the data file
    //FAccessMutex: TMutex; // Mutex to secure multi-thread access
    FMagnification: TMagnification; // Magnification of tiled index

    {function LoadIndexPage(const ATile: TPixel;
      var ACache: TValueCache): Boolean;
    function GetIndexPage(const ATile: TPixel;
      var ACache: TValueCache): Boolean; }

  public
    constructor Create(const ADataFile: string; ACacheSize: Integer);

    function Open(ATypeConfig: TTypeConfig;
      const APath: string;
      AMemoryMappedData: Boolean): Boolean;

    function IsOpen(): Boolean; virtual;
    function Close(): Boolean; virtual;

    function GetTile(const ACoord: TGeoPoint): TPixel;
    function IsCovered(const ATile: TPixel): Boolean;

    function Get(AId: TId; ANode: TRouteNode): Boolean; overload;

    function Get(ABegin, AEnd, ASize: Integer;
      AData: TRouteNodeList): Boolean; overload;

    function Get(ABegin, AEnd, ASize: Integer;
      AData: TRouteNodeMapById): Boolean; overload;
  end;


  TIntersectionDataFile = class(TIndexedDataFile)

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

type
  TIndexEntry = record
    FileOffset: TFileOffset;
    Count: Integer;
  end;

  TIndexPage = record
    FileOffset: TFileOffset;
    Remaining: Integer;
    NodeMap: TRouteNodeMapById;

    function Find(AScanner: TFileScanner; AId: TId): TRouteNode;
  end;

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


end.

