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

routing/RoutingDB
  RoutingDatabase ...
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

  { Encapsulation of the routing relevant data files, similar to Database. }
  TRoutingDatabase = class(TObject)
  protected
    FTypeConfig: TTypeConfig;
    FPath: string;
  public
    constructor Create();

    function Open(ADatabase: TMapDatabaseId): Boolean; virtual; abstract;
    procedure Close();

    function GetRouteNode(const AId: TId; ANode: TRouteNode): Boolean; virtual; abstract;

    function GetRouteNodes(ABegin, AEnd: Integer; ASize: Integer;
      ARouteNodeMap: TRouteNodeMapById): Boolean; overload; virtual; abstract;

    function GetRouteNodes(ABegin, AEnd: Integer; ASize: Integer;
      ARouteNodes: TRouteNodeList): Boolean; overload; virtual; abstract;

    { Junction = Intersection }
    function GetJunctions(const AIds: array of TId;
      var AJunctions: TIntersectionArray): Boolean; virtual; abstract;

    function GetObjectVariantData(): TObjectVariantDataArray; virtual; abstract;

    function ContainsNode(const AId: TId): Boolean; virtual; abstract;
  end;


implementation


end.

