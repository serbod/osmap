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

Database:
  DatabaseParameter
  NodeRegionSearchResultEntry
  NodeRegionSearchResult
  WayRegionSearchResultEntry
  WayRegionSearchResult
  AreaRegionSearchResultEntry
  AreaRegionSearchResult
  Database -> TMapDatabase ...
*)
unit OsMapDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OsMapTypes, OsMapFiles, OsMapObjTypes, OsMapObjects, OsMapGeometry,
  OsMapDataFiles;

type
  { Database instance initialization parameter to influence the behavior of the database
    instance.

    The following attributes are currently available:
    cache sizes. }
  TDatabaseParameter = record
    AreaAreaIndexCacheSize: Integer;
    NodeDataCacheSize: Integer;
    WayDataCacheSize: Integer;
    AreaDataCacheSize: Integer;

    RouterDataMMap: Boolean;
    NodesDataMMap: Boolean;
    AreasDataMMap: Boolean;
    WaysDataMMap: Boolean;
    OptimizeLowZoomMMap: Boolean;
    IndexMMap: Boolean;
  end;

  TNodeRegionSearchResultEntry = record
    Node: TMapNode;
    Distance: TDistance;
  end;

  TNodeRegionSearchResult = array of TNodeRegionSearchResultEntry;

  TWayRegionSearchResultEntry = record
    Way: TMapWay;
    Distance: TDistance;
    ClosestPoint: TGeoPoint;
  end;

  TWayRegionSearchResult = array of TWayRegionSearchResultEntry;

  TAreaRegionSearchResultEntry = record
    Area: TMapArea;
    Distance: TDistance;
    ClosestPoint: TGeoPoint;
    InArea: Boolean;
  end;

  TAreaRegionSearchResult = array of TAreaRegionSearchResultEntry;

  { Central access class to all the individual data files and indexes.

    A database is mainly initialized with a number of optional but performance
    relevant parameters.

    The Database is opened by passing the directory that contains
    all database files. }
  TMapDatabase = class
  private
    FParameter: TDatabaseParameter;          // Parameterization of this database object

    FPath: string;                     // Path to the directory containing all files
    FIsOpen: Boolean;                   // true, if opened

    FTypeConfig: TTypeConfig;               // Type config for the currently opened map

    FBoundingBoxDataFile: TBoundingBoxDataFile;      // Cached access to the bounding box data file
    //FBoundingBoxDataFileMutex: TMutex; // Mutex to make lazy initialisation of node DataFile thread-safe

    FNodeDataFile: TNodeDataFile;             // Cached access to the 'nodes.dat' file
    //FNodeDataFileMutex: TMutex;        // Mutex to make lazy initialisation of node DataFile thread-safe

    FAreaDataFile: TAreaDataFile;             //!< Cached access to the 'areas.dat' file
    //FAreaDataFileMutex: TMutex;        // Mutex to make lazy initialisation of area DataFile thread-safe

    FWayDataFile: TWayDataFile;              // Cached access to the 'ways.dat' file
    //FWayDataFileMutex: TMutex;         // Mutex to make lazy initialisation of way DataFile thread-safe

    FAreaNodeIndex: TAreaNodeIndex;            // Index of nodes by containing area
    //FAreaNodeIndexMutex: TMutex;       // Mutex to make lazy initialisation of area node index thread-safe

    FAreaWayIndex: TAreaWayIndex;             // Index of areas by containing area
    //FAreaWayIndexMutex: TMutex;        // Mutex to make lazy initialisation of area way index thread-safe

    FAreaAreaIndex: TAreaAreaIndex;            // Index of ways by containing area
    //FAreaAreaIndexMutex: TMutex;       // Mutex to make lazy initialisation of area area index thread-safe

    {mutable LocationIndexRef        locationIndex;            //!< Location-based index
    mutable std::mutex              locationIndexMutex;       //!< Mutex to make lazy initialisation of location index thread-safe

    mutable WaterIndexRef           waterIndex;               //!< Index of land/sea tiles
    mutable std::mutex              waterIndexMutex;          //!< Mutex to make lazy initialisation of water index thread-safe

    mutable OptimizeAreasLowZoomRef optimizeAreasLowZoom;     //!< Optimized data for low zoom situations
    mutable std::mutex              optimizeAreasMutex;       //!< Mutex to make lazy initialisation of optimized areas index thread-safe

    mutable OptimizeWaysLowZoomRef  optimizeWaysLowZoom;      //!< Optimized data for low zoom situations
    mutable std::mutex              optimizeWaysMutex;        //!< Mutex to make lazy initialisation of optimized ways index thread-safe
    }

  public
    constructor Create(const AParameter: TDatabaseParameter);
    destructor Destroy; override;

    function Open(const APath: string): Boolean;
    function IsOpen(): Boolean;
    procedure Close();

    { Load nodes of given types with maximum distance to the given coordinate.
      ALocation - Geo coordinate in the center of the given circle
      ATypes - Set of type to load conadidates for
      AMaxDistance - lookup distance in meters, Maximum radius from center to search for
      throws OSMScoutException in case of errors }
    function LoadNodesInRadius(const ALocation: TGeoPoint;
      const ATypes: TTypeInfoSet;
      AMaxDistance: TDistance = 100): TNodeRegionSearchResult;

    { Load ways of given types with maximum distance to the given coordinate.
      ALocation - Geo coordinate in the center of the given circle
      ATypes - Set of type to load conadidates for
      AMaxDistance - lookup distance in meters, Maximum radius from center to search for
      throws OSMScoutException in case of errors }
    function LoadWaysInRadius(const ALocation: TGeoPoint;
      const ATypes: TTypeInfoSet;
      AMaxDistance: TDistance = 100): TWayRegionSearchResult;

    { Load areas of given types with maximum distance to the given coordinate.
      ALocation - Geo coordinate in the center of the given circle
      ATypes - Set of type to load conadidates for
      AMaxDistance - lookup distance in meters, Maximum radius from center to search for
      throws OSMScoutException in case of errors }
    function LoadAreasInRadius(const ALocation: TGeoPoint;
      const ATypes: TTypeInfoSet;
      AMaxDistance: TDistance = 100): AreaRegionSearchResult;

    { Load nodes of given types in the given geo box
      Distance is measured in relation to the center of the bounding box
      ATypes - Set of type to load conadidates for
      ABoundingBox - Geographic area to search in
      throws OSMScoutException in case of errors }
    function LoadNodesInArea(const ATypes: TTypeInfoSet;
      const ABoundingBox: TGeoBox): TNodeRegionSearchResult;

    { Load ways of given types in the given geo box.
      Distance is measured in relation to the center of the bounding box
      ATypes - Set of type to load conadidates for
      ABoundingBox - Geographic area to search in
      throws OSMScoutException in case of errors }
    function LoadWaysInArea(const ATypes: TTypeInfoSet;
      const ABoundingBox: TGeoBox): TWayRegionSearchResult;

    { Load areas of given types in the given geo box.
      Distance is measured in relation to the center of the bounding box
      ATypes - Set of type to load conadidates for
      ABoundingBox - Geographic area to search in
      throws OSMScoutException in case of errors }
    function LoadAreasInArea(const ATypes: TTypeInfoSet;
      const ABoundingBox: TGeoBox): TAreaRegionSearchResult;

    procedure DumpStatistics();

    property Path: string read FPath;
    property TypeConfig: TTypeConfig read FTypeConfig;
    property Parameter: TDatabaseParameter read FParameter;

    property BoundingBoxDataFile: TBoundingBoxDataFile read FBoundingBoxDataFile;
    property NodeDataFile: TNodeDataFile read FNodeDataFile;
    property AreaDataFile: TAreaDataFile read FAreaDataFile;
    property WayDataFile: TWayDataFile read FWayDataFile;
  end;

implementation


end.

