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
TypeFeatures:
  NameFeature
  NameAltFeature
  RefFeature
  LocationFeature
  AddressFeature
  AccessFeature  -> TAccessFeature
  AccessRestrictedFeature -> TAccessRestrictedFeature

FeatureReader:
  FeatureReader
  DynamicFeatureReader
  FeatureValueReader
  FeatureLabelReader
*)
unit OsMapObjFeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsMapTypes, OsMapObjTypes, OsMapFiles, OsMapTags;

const
  FeatureNames: array[TFeatureType] of string = (
    'None',
    'Name',
    'NameAlt',
    'Ref',
    'Location',
    'Address',
    'Access',
    'AccessRestricted',
    'Layer',
    'Width',
    'MaxSpeed',
    'Grade',
    'AdminLevel',
    'PostalCode',
    'Bridge',
    'Tunnel',
    'Embankment',
    'Roundabout',
    'Ele',
    'Destination',
    'Building',
    'Website',
    'Phone',
    'IsIn',
    'ConstructionYear',
    'Sideway',
    'Lanes'
  );


type

  { FFeatureParser }

  TFeatureParser = class
  private
    FTagRef: TTagId;

    FTagAddrHouseNr: TTagId;
    FTagAddrStreet: TTagId;
    FTagAddrPlace: TTagId;

    FTagAccess: TTagId;
    FTagAccessForward: TTagId;
    FTagAccessBackward: TTagId;

    FTagFoot: TTagId;
    FTagFootForward: TTagId;
    FTagFootBackward: TTagId;

    FTagBicycle: TTagId;
    FTagBicycleForward: TTagId;
    FTagBicycleBackward: TTagId;

    FTagMotorVehicle: TTagId;
    FTagMotorVehicleForward: TTagId;
    FTagMotorVehicleBackward: TTagId;

    FTagMotorcar: TTagId;
    FTagMotorcarForward: TTagId;
    FTagMotorcarBackward: TTagId;

    FTagLayer: TTagId;
    // ---
    FTagWidth: TTagId;

    FTagMaxSpeed: TTagId;
    // grade
    FTagSurface: TTagId;
    FTagTrackType: TTagId;
    // admin level
    FTagAdminLevel: TTagId;
    FTagIsIn: TTagId;
    // postal code
    FTagPostalCode: TTagId;
    FTagAddrPostCode: TTagId;

    FTagBridge: TTagId;
    FTagTunnel: TTagId;
    FTagEmbankment: TTagId;

    FTagEle: TTagId;
    // destination
    FTagDestination: TTagId;
    FTagDestinationRef: TTagId;
    FTagDestinationForward: TTagId;

    FTagBuilding: TTagId;

    FTagWebsite: TTagId;
    FTagPhone: TTagId;

    FTagConstructionYear: TTagId;
    FTagStartDate: TTagId;

    FTagSidewalk: TTagId;
    FTagCyclewayLeft: TTagId;
    FTagCyclewayLeftSegregated: TTagId;
    FTagCyclewayRight: TTagId;
    FTagCyclewayRightSegregated: TTagId;

    FTagJunction: TTagId;
    FTagOneway: TTagId;
    FTagLanes: TTagId;
    FTagLanesForward: TTagId;
    FTagLanesBackward: TTagId;
    FTagTurnLanes: TTagId;
    FTagTurnLanesForward: TTagId;
    FTagTurnLanesBackward: TTagId;
    FTagDestinationLanes: TTagId;
    FTagDestinationLanesForward: TTagId;
    FTagDestinationLanesBackward: TTagId;

  public
    { called from TFeature }
    procedure Initialize(AFeature: TFeature; ATagRegistry: TTagRegistry);

    { called from TFeature }
    procedure Parse(AFeature: TFeature;
      AReporter: TTagErrorReporter;
      ATagRegistry: TTagRegistry;
      const AFeatureInfo: TFeatureInfo;
      const AObjectOSMRef: TObjectOSMRef;
      const ATagMap: TTagMap;
      var ABuffer: TFeatureValueBuffer);
  end;


  { The location feature stores the location of an (normally) node or area. Even the data is not stored
    the location feature checks that a street or place and an house number is stored on the object.

    So in effect it stores the location part of objects that have an address. }


  { The address feature stores the house number of an (normally) node or area. Even the data is not stored
    the address feature checks that a street or place and an house number is stored on the object.

    So in effect it stores the house number part of objects that have an address. }



  { TAccessFeature }

  TAccessFeature = class(TFeature)
  public
    class function CanRoute(AValue: Byte): Boolean;
    class function CanRoute(AValue: Byte; AVehicle: TVehicleType): Boolean; overload;
    class function CanRoute(AValue: Byte; AVehicles: TVehicleTypes): Boolean; overload;

    class function CanRouteForward(AValue: Byte): Boolean;
    class function CanRouteForward(AValue: Byte; AVehicle: TVehicleType): Boolean; overload;

    class function CanRouteBackward(AValue: Byte): Boolean;
    class function CanRouteBackward(AValue: Byte; AVehicle: TVehicleType): Boolean; overload;

    class function IsOneway(AValue: Byte): Boolean;
    class function IsOnewayForward(AValue: Byte): Boolean;
    class function IsOnewayBackward(AValue: Byte): Boolean;
  end;

  { AccessRestriction signals, if there is some access restriction for a given way and a given vehicle.

    An access restriction means, that a way can be used for a vehicle, but access ist restricted.
    Restricted access means, that you can enter a restricted region, but cannot leave it again for a given
    route. You may only enter the restricted region if you have a certain intention.

    No access restriction, does not mean that a way can be used for a given vehicle. You must still evaluate if
    there is access at all for the vehicle. }

  { TAccessRestrictedFeature }

  TAccessRestrictedFeature = class(TFeature)
  public
    class function CanAccess(AValue: Byte): Boolean;
    class function CanAccess(AValue: Byte; AVehicle: TVehicleType): Boolean; overload;
    class function CanAccess(AValue: Byte; AVehicles: TVehicleTypes): Boolean; overload;
  end;

  { TAdminLevelFeature }

  TAdminLevelFeature = class(TFeature)
  public
    class procedure SetAdminLevel(AAdminLevel: Byte; var AValue: string);
    class procedure SetIsIn(AIsIn: string; var AValue: string);

    class function GetAdminLevel(const AValue: string): Byte;
    class function GetIsIn(const AValue: string): string;
  end;



  { TEleFeature }

  TEleFeature = class(TFeature)
  public
    class procedure SetEle(AEle: Integer; var AValue: string);
    class function GetEle(var AValue: string): Integer;
  end;


  { Helper template class for easy access to flag-like Features.

    Each type may have stored the feature in request at a different index. The FeatureReader
    caches the index for each type once in the constructor and later on allows access to the feature
    in O(1) - without iterating of all feature(values) of an object. }

  { TFeatureReader }

  TFeatureReader = object
  private
    FFeatureName: string;
    { Index - TTypeInfo.Index; Value - feature index for type }
    FLookupTable: array of Integer;
  public
    procedure Init(ATypeConfig: TTypeConfig; AFeatureType: TFeatureType);

    { Returns the index of the Feature/FeatureValue within the given FeatureValueBuffer. }
    function GetIndex(const ABuffer: TFeatureValueBuffer; out AIndex: Integer): Boolean;
    { Returns true, if the feature is set for the given FeatureValueBuffer }
    function IsSet(const ABuffer: TFeatureValueBuffer): Boolean;
    { Returns the FeatureValue for the given FeatureValueBuffer }
    function GetValue(const ABuffer: TFeatureValueBuffer): string;
    function GetValueByte(const ABuffer: TFeatureValueBuffer): Byte;
    function ReadValueByte(const ABuffer: TFeatureValueBuffer; out AValue: Byte): Boolean;
    { Returns the label of the given object }
    function GetLabel(const ABuffer: TFeatureValueBuffer): string;

    property FeatureName: string read FFeatureName;
  end;

  { Variant of FeatureReader that is not type set and thus can easier get used
    in cases where runtime dynamics are required and features are referenced
    by name and not by type. }
  TDynamicFeatureReader = TFeatureReader;

  { Helper template class for easy access to the value of a certain feature for objects of any type.

    Each type may have stored the feature in request at a different index. The FeatureValueReader
    caches the index for each type once in the constructor and later on allows access to the feature value
    in O(1) - without iterating of all feature(values) of an object. }
  TFeatureValueReader = TFeatureReader;

  TFeatureLabelReader = TFeatureReader;

function GetDefaultRouteAccess(AFoot, ABycicle, ACar: Boolean): Byte;


implementation

uses OsMapUtils;

const
  ROUTE_FOOT_FORWARD     = $01;
  ROUTE_FOOT_BACKWARD    = $02;
  ROUTE_BICYCLE_FORWARD  = $04;
  ROUTE_BICYCLE_BACKWARD = $08;
  ROUTE_CAR_FORWARD      = $10;
  ROUTE_CAR_BACKWARD     = $20;
  ROUTE_ONEWAY_FORWARD   = $40;
  ROUTE_ONEWAY_BACKWARD  = $80;

  ACCESS_FOOT     = $01;
  ACCESS_BICYCLE  = $02;
  ACCESS_CAR      = $04;


procedure ParseAccesFlag(const AValue: string; var AAccess: Byte; ABit: Byte);
begin
  AAccess := AAccess and (not ABit);
  if AValue <> 'no' then
    AAccess := AAccess or ABit;
end;

function GetDefaultRouteAccess(AFoot, ABycicle, ACar: Boolean): Byte;
begin
  Result := 0;
  if AFoot then
    Result := Result or ROUTE_FOOT_FORWARD or ROUTE_FOOT_BACKWARD;

  if ABycicle then
    Result := Result or ROUTE_BICYCLE_FORWARD or ROUTE_BICYCLE_BACKWARD;

  if ACar then
    Result := Result or ROUTE_CAR_FORWARD or ROUTE_CAR_BACKWARD;
end;

{ TFeatureReader }

procedure TFeatureReader.Init(ATypeConfig: TTypeConfig; AFeatureType: TFeatureType);
var
  i, n: Integer;
  TmpType: TTypeInfo;
begin
  FFeatureName := FeatureNames[AFeatureType];

  SetLength(FLookupTable, ATypeConfig.Types.Count);
  for i := 0 to ATypeConfig.Types.Count-1 do
  begin
    TmpType := ATypeConfig.Types[i];
    if TmpType.FindFeature(AFeatureType, n) then
      FLookupTable[i] := n
    else
      FLookupTable[i] := -1;
  end;
end;

function TFeatureReader.GetIndex(const ABuffer: TFeatureValueBuffer; out
  AIndex: Integer): Boolean;
begin
  AIndex := FLookupTable[ABuffer.TypeInfo.Index];
  Result := (AIndex <> -1);
end;

function TFeatureReader.IsSet(const ABuffer: TFeatureValueBuffer): Boolean;
var
  i: Integer;
begin
  i := FLookupTable[ABuffer.TypeInfo.Index];
  if i <> -1 then
    Result := ABuffer.HasFeatureValue(i)
  else
    Result := False;
end;

function TFeatureReader.GetValue(const ABuffer: TFeatureValueBuffer): string;
var
  i: Integer;
begin
  i := FLookupTable[ABuffer.TypeInfo.Index];
  if (i <> -1) and ABuffer.HasFeatureValue(i) then
    Result := ABuffer.GetValue(i)
  else
    Result := '';
end;

function TFeatureReader.GetValueByte(const ABuffer: TFeatureValueBuffer): Byte;
var
  i: Integer;
begin
  i := FLookupTable[ABuffer.TypeInfo.Index];
  if (i <> -1) and ABuffer.HasFeatureValue(i) then
    Result := StrTointDef(ABuffer.GetValue(i), 0)
  else
    Result := 0;
end;

function TFeatureReader.ReadValueByte(const ABuffer: TFeatureValueBuffer; out
  AValue: Byte): Boolean;
var
  i: Integer;
begin
  i := FLookupTable[ABuffer.TypeInfo.Index];
  if (i <> -1) and ABuffer.HasFeatureValue(i) then
  begin
    AValue := StrToIntDef(ABuffer.GetValue(i), 0);
    Result := True;
  end
  else
    Result := False;
end;

function TFeatureReader.GetLabel(const ABuffer: TFeatureValueBuffer): string;
var
  i: Integer;
begin
  i := FLookupTable[ABuffer.TypeInfo.Index];
  if (i <> -1) and ABuffer.HasFeatureValue(i) then
    Result := ABuffer.GetValue(i)
  else
    Result := '';
end;

{ TEleFeature }

class procedure TEleFeature.SetEle(AEle: Integer; var AValue: string);
begin
  AValue := IntToStr(AEle);
end;

class function TEleFeature.GetEle(var AValue: string): Integer;
begin
  Result := StrToIntDef(AValue, 0);
end;

{ TAdminLevelFeature }

class procedure TAdminLevelFeature.SetAdminLevel(AAdminLevel: Byte;
  var AValue: string);
var
  sIsIn: string;
begin
  sIsIn := GetIsIn(AValue);
  AValue := IntToStr(AAdminLevel) + ' ' + sIsIn;
end;

class procedure TAdminLevelFeature.SetIsIn(AIsIn: string; var AValue: string);
var
  n: Integer;
begin
  n := Pos(' ', AValue);
  if n <> 0 then
    AValue := Copy(AValue, 1, n) + AIsIn // including found space
  else
    AValue := '0 ' + AIsIn;
end;

class function TAdminLevelFeature.GetAdminLevel(const AValue: string): Byte;
var
  n: Integer;
begin
  n := Pos(' ', AValue);
  if n <> 0 then
    Result := StrToInt(Copy(AValue, 1, n-1))
  else
    Result := 0;
end;

class function TAdminLevelFeature.GetIsIn(const AValue: string): string;
var
  n: Integer;
begin
  n := Pos(' ', AValue);
  if n <> 0 then
    Result := Copy(AValue, n+1, MaxInt)
  else
    Result := '';
end;

{ TFeatureParser }

procedure TFeatureParser.Initialize(AFeature: TFeature;
  ATagRegistry: TTagRegistry);
begin
  case AFeature.FeatureType of
    ftRef:
    begin
      FTagRef := ATagRegistry.RegisterTag('ref');
    end;

    ftLocation:
    begin
      FTagAddrHouseNr := ATagRegistry.RegisterTag('addr:housenumber');
      FTagAddrStreet := ATagRegistry.RegisterTag('addr:street');
      FTagAddrPlace := ATagRegistry.RegisterTag('addr:place');
    end;

    ftAddress:
    begin
      FTagAddrHouseNr := ATagRegistry.RegisterTag('addr:housenumber');
      FTagAddrStreet := ATagRegistry.RegisterTag('addr:street');
      FTagAddrPlace := ATagRegistry.RegisterTag('addr:place');
    end;

    ftAccess:
    begin
      FTagOneway := ATagRegistry.RegisterTag('oneway');
      FTagJunction := ATagRegistry.RegisterTag('junction');

      FTagAccess := ATagRegistry.RegisterTag('access');
      FTagAccessForward := ATagRegistry.RegisterTag('access:forward');
      FTagAccessBackward := ATagRegistry.RegisterTag('access:backward');

      FTagFoot := ATagRegistry.RegisterTag('foot');
      FTagFootForward := ATagRegistry.RegisterTag('foot:forward');
      FTagFootBackward := ATagRegistry.RegisterTag('foot:backward');

      FTagBicycle := ATagRegistry.RegisterTag('bicycle');
      FTagBicycleForward := ATagRegistry.RegisterTag('bicycle:forward');
      FTagBicycleBackward := ATagRegistry.RegisterTag('bicycle:backward');

      FTagMotorVehicle := ATagRegistry.RegisterTag('motor_vehicle');
      FTagMotorVehicleForward := ATagRegistry.RegisterTag('motor_vehicle:forward');
      FTagMotorVehicleBackward := ATagRegistry.RegisterTag('motor_vehicle:backward');

      FTagMotorcar := ATagRegistry.RegisterTag('motorcar');
      FTagMotorcarForward := ATagRegistry.RegisterTag('motorcar:forward');
      FTagMotorcarBackward := ATagRegistry.RegisterTag('motorcar:backward');
    end;

    ftAccessRestricted:
    begin
      FTagAccess := ATagRegistry.RegisterTag('access');
      FTagFoot := ATagRegistry.RegisterTag('foot');
      FTagBicycle := ATagRegistry.RegisterTag('bicycle');
      FTagMotorVehicle := ATagRegistry.RegisterTag('motor_vehicle');
    end;

    ftLayer:
    begin
      FTagLayer := ATagRegistry.RegisterTag('layer');
    end;

    ftWidth:
    begin
      FTagWidth := ATagRegistry.RegisterTag('width');
    end;

    ftMaxSpeed:
    begin
      FTagMaxSpeed := ATagRegistry.RegisterTag('maxspeed');
    end;

    ftGrade:
    begin
      FTagSurface := ATagRegistry.RegisterTag('surface');
      FTagTrackType := ATagRegistry.RegisterTag('tracktype');
    end;

    ftAdminLevel:
    begin
      FTagAdminLevel := ATagRegistry.RegisterTag('admin_level');
      FTagIsIn := ATagRegistry.RegisterTag('is_in');
    end;

    ftPostalCode:
    begin
      FTagPostalCode := ATagRegistry.RegisterTag('postal_code');
      FTagAddrPostCode := ATagRegistry.RegisterTag('addr:postcode');
    end;

    ftWebsite:
    begin
      FTagWebsite := ATagRegistry.RegisterTag('website');
    end;

    ftPhone:
    begin
      FTagPhone := ATagRegistry.RegisterTag('phone');
    end;

    ftBridge:
    begin
      FTagBridge := ATagRegistry.RegisterTag('bridge');
    end;

    ftTunnel:
    begin
      FTagTunnel := ATagRegistry.RegisterTag('tunnel');
    end;

    ftEmbankment:
    begin
      FTagEmbankment := ATagRegistry.RegisterTag('embankment');
    end;

    ftRoundabout:
    begin
      FTagJunction := ATagRegistry.RegisterTag('junction');
    end;

    ftEle:
    begin
      FTagEle := ATagRegistry.RegisterTag('ele');
    end;

    ftDestination:
    begin
      FTagDestination := ATagRegistry.RegisterTag('destination');
      FTagDestinationRef := ATagRegistry.RegisterTag('destination:ref');
      FTagDestinationForward := ATagRegistry.RegisterTag('destination:forward');
    end;

    ftBuilding:
    begin
      FTagBuilding := ATagRegistry.RegisterTag('building');
    end;

    ftIsIn:
    begin
      FTagIsIn := ATagRegistry.RegisterTag('is_in');
    end;

    ftConstructionYear:
    begin
      FTagConstructionYear := ATagRegistry.RegisterTag('year_of_construction');
      FTagStartDate := ATagRegistry.RegisterTag('start_date');
    end;

    ftSideway:
    begin
      FTagSidewalk := ATagRegistry.RegisterTag('sidewalk');
      FTagCyclewayLeft := ATagRegistry.RegisterTag('cycleway:left');
      FTagCyclewayLeftSegregated := ATagRegistry.RegisterTag('cycleway:left:segregated');
      FTagCyclewayRight := ATagRegistry.RegisterTag('cycleway:right');
      FTagCyclewayRightSegregated := ATagRegistry.RegisterTag('cycleway:right:segregated');
    end;

    ftLanes:
    begin
      FTagOneway := ATagRegistry.RegisterTag('oneway');
      FTagLanes := ATagRegistry.RegisterTag('lanes');
      FTagLanesForward := ATagRegistry.RegisterTag('lanes:forward');
      FTagLanesBackward := ATagRegistry.RegisterTag('lanes:backward');
      FTagTurnLanes := ATagRegistry.RegisterTag('turn:lanes');
      FTagTurnLanesForward := ATagRegistry.RegisterTag('turn:lanes:forward');
      FTagTurnLanesBackward := ATagRegistry.RegisterTag('turn:lanes:backward');
      FTagDestinationLanes := ATagRegistry.RegisterTag('destination:lanes');
      FTagDestinationLanesForward := ATagRegistry.RegisterTag('destination:lanes:forward');
      FTagDestinationLanesBackward := ATagRegistry.RegisterTag('destination:lanes:backward');
    end;
  end;
end;

procedure TFeatureParser.Parse(AFeature: TFeature;
  AReporter: TTagErrorReporter; ATagRegistry: TTagRegistry;
  const AFeatureInfo: TFeatureInfo; const AObjectOSMRef: TObjectOSMRef;
  const ATagMap: TTagMap; var ABuffer: TFeatureValueBuffer);
var
  s: string;
  i: Integer;
  MaxPrio, TagPrio: LongWord;
  IsTag: Boolean;
  Value: TFeatureValue;
begin
  MaxPrio := 0;
  s := '';

  case AFeature.FeatureType of
    ftName,
    ftNameAlt:
    begin
      for i := 0 to ATagMap.Count-1 do
      begin
        case AFeature.FeatureType of
          ftName:    IsTag := ATagRegistry.IsNameTag(ATagMap.Keys[i], TagPrio);
          ftNameAlt: IsTag := ATagRegistry.IsNameAltTag(ATagMap.Keys[i], TagPrio);
        end;
        if IsTag and ((s = '') or (TagPrio > MaxPrio)) then
        begin
          s := ATagMap.Data[i];
          MaxPrio := TagPrio;
        end;
      end;

      if (s <> '') then
      begin
        ABuffer.SetValue(AFeatureInfo.Index, s);
      end;
    end;
  end;
end;

{ TAccessRestrictedFeature }

class function TAccessRestrictedFeature.CanAccess(AValue: Byte): Boolean;
begin
  Result := (AValue and (ACCESS_FOOT or ACCESS_BICYCLE or ACCESS_CAR)) <> 0;
end;

class function TAccessRestrictedFeature.CanAccess(AValue: Byte;
  AVehicle: TVehicleType): Boolean;
begin
  case AVehicle of
    vehicleFoot:    Result := (AValue and ACCESS_FOOT) <> 0;
    vehicleBicycle: Result := (AValue and ACCESS_BICYCLE) <> 0;
    vehicleCar:     Result := (AValue and ACCESS_CAR) <> 0;
  else
    Result := False;
  end;
end;

class function TAccessRestrictedFeature.CanAccess(AValue: Byte;
  AVehicles: TVehicleTypes): Boolean;
begin
  Result := True;
  if (vehicleFoot in AVehicles) then
    if (AValue and ACCESS_FOOT) <> 0 then
      Exit;

  if (vehicleBicycle in AVehicles) then
    if (AValue and ACCESS_BICYCLE) <> 0 then
      Exit;

  if (vehicleCar in AVehicles) then
    if (AValue and ACCESS_CAR) <> 0 then
      Exit;

  Result := False;
end;

{ TAccessFeature }

class function TAccessFeature.CanRoute(AValue: Byte): Boolean;
begin
  Result := (AValue and (ROUTE_FOOT_FORWARD or ROUTE_FOOT_BACKWARD
                      or ROUTE_BICYCLE_FORWARD or ROUTE_BICYCLE_BACKWARD
                      or ROUTE_CAR_FORWARD or ROUTE_CAR_BACKWARD)) <> 0;
end;

class function TAccessFeature.CanRoute(AValue: Byte; AVehicle: TVehicleType
  ): Boolean;
begin
  case AVehicle of
    vehicleFoot:    Result := (AValue and (ROUTE_FOOT_FORWARD or ROUTE_FOOT_BACKWARD)) <> 0;
    vehicleBicycle: Result := (AValue and (ROUTE_BICYCLE_FORWARD or ROUTE_BICYCLE_BACKWARD)) <> 0;
    vehicleCar:     Result := (AValue and (ROUTE_CAR_FORWARD or ROUTE_CAR_BACKWARD)) <> 0;
  else
    Result := False;
  end;
end;

class function TAccessFeature.CanRoute(AValue: Byte; AVehicles: TVehicleTypes
  ): Boolean;
begin
  Result := True;
  if (vehicleFoot in AVehicles) then
    if (AValue and (ROUTE_FOOT_FORWARD or ROUTE_FOOT_BACKWARD)) <> 0 then
      Exit;

  if (vehicleBicycle in AVehicles) then
    if (AValue and (ROUTE_BICYCLE_FORWARD or ROUTE_BICYCLE_BACKWARD)) <> 0 then
      Exit;

  if (vehicleCar in AVehicles) then
    if (AValue and (ROUTE_CAR_FORWARD or ROUTE_CAR_BACKWARD)) <> 0 then
      Exit;

  Result := False;
end;

class function TAccessFeature.CanRouteForward(AValue: Byte): Boolean;
begin
  Result := (AValue and (ROUTE_FOOT_FORWARD or ROUTE_BICYCLE_FORWARD or ROUTE_CAR_FORWARD)) <> 0;
end;

class function TAccessFeature.CanRouteForward(AValue: Byte;
  AVehicle: TVehicleType): Boolean;
begin
  case AVehicle of
    vehicleFoot:    Result := (AValue and ROUTE_FOOT_FORWARD) <> 0;
    vehicleBicycle: Result := (AValue and ROUTE_BICYCLE_FORWARD) <> 0;
    vehicleCar:     Result := (AValue and ROUTE_CAR_FORWARD) <> 0;
  else
    Result := False;
  end;
end;

class function TAccessFeature.CanRouteBackward(AValue: Byte): Boolean;
begin
  Result := (AValue and (ROUTE_FOOT_BACKWARD or ROUTE_BICYCLE_BACKWARD or ROUTE_CAR_BACKWARD)) <> 0;
end;

class function TAccessFeature.CanRouteBackward(AValue: Byte;
  AVehicle: TVehicleType): Boolean;
begin
  case AVehicle of
    vehicleFoot:    Result := (AValue and ROUTE_FOOT_BACKWARD) <> 0;
    vehicleBicycle: Result := (AValue and ROUTE_BICYCLE_BACKWARD) <> 0;
    vehicleCar:     Result := (AValue and ROUTE_CAR_BACKWARD) <> 0;
  else
    Result := False;
  end;
end;

class function TAccessFeature.IsOneway(AValue: Byte): Boolean;
begin
  Result := (AValue and (ROUTE_ONEWAY_FORWARD or ROUTE_ONEWAY_BACKWARD)) <> 0;
end;

class function TAccessFeature.IsOnewayForward(AValue: Byte): Boolean;
begin
  Result := (AValue and ROUTE_ONEWAY_FORWARD) <> 0;
end;

class function TAccessFeature.IsOnewayBackward(AValue: Byte): Boolean;
begin
  Result := (AValue and ROUTE_ONEWAY_BACKWARD) <> 0;
end;


end.

