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
  OsMap Tags
Tag
util\TagErrorReporter
*)
unit OsMapTags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, OsMapTypes;

const
  { Magic constant for an unresolved and to be ignored tag }
  TAG_IGNORE = 0;

  TAG_AREA = 'area';
  TAG_NATURAL = 'natural';

type
  { Tag types}

  TTagId = Word;

  TTagMap = specialize TFPGMap<TTagId, string>;

  { Enumeration of possible binary operators as used by the various parsers }
  TBinaryOperator = (
    operatorLess,
    operatorLessEqual,
    operatorEqual,
    operatorGreaterEqual,
    operatorGreater,
    operatorNotEqual
  );

  { Abstract base class for all tag based conditions }
  TTagCondition = class
  public
    function Evaluate(ATagMap: TTagMap): Boolean; virtual; abstract;
  end;

  { Reference counted reference to a tag condition }
  TTagConditionRef = TTagCondition;
  TTagConditionList = specialize TFPGList<TTagConditionRef>;

  { Negates the result of the given child condition }
  TTagNotCondition = class(TTagCondition)
  private
    FCondition: TTagConditionRef;
  public
    constructor Create(ACondition: TTagConditionRef);
    function Evaluate(ATagMap: TTagMap): Boolean; override;
  end;

  TTagBoolType = (boolAnd, boolOr);

  { Allows a boolean and/or condition between a number of
    child conditions. }
  TTagBoolCondition = class(TTagCondition)
  private
    FConditions: TTagConditionList;
    FTagBoolType: TTagBoolType;
  public
    constructor Create(ATagBoolType: TTagBoolType);
    destructor Destroy(); override;
    procedure AddCondition(ACondition: TTagConditionRef);
    function Evaluate(ATagMap: TTagMap): Boolean; override;
  end;

  { Reference counted reference to a tag condition }
  TTagBoolConditionRef = TTagBoolCondition;

  { Returns true, if the given tag exists for an object }
  TTagExistsCondition = class(TTagCondition)
  private
    FTagId: TTagId;
  public
    constructor Create(ATagId: TTagId);
    function Evaluate(ATagMap: TTagMap): Boolean; override;
  end;

  TTagValueType = (vtString, vtInteger);

  { Returns true, if the value of the given tag fulfills the given
    boolean condition in regard to the comparison value. }
  TTagBinaryCondition = class(TTagCondition)
  private
    FTagId: TTagId;
    FBinaryOperator: TBinaryOperator;
    FValueType: TTagValueType;
    FTagStringValue: string;
    FTagSizeValue: Integer;
  public
    constructor Create(ATagId: TTagId;
                       ABinaryOperator: TBinaryOperator;
                       ATagValue: string);

    constructor Create(ATagId: TTagId;
                       ABinaryOperator: TBinaryOperator;
                       ATagValue: Integer);

    function Evaluate(ATagMap: TTagMap): Boolean; override;
  end;

  { Returns true, if the tag value of the given is one of the
    given values. }
  TTagIsInCondition = class(TTagCondition)
  private
    FTagId: TTagId;
    FTagValues: TStringList;  // list of tag names
  public
    constructor Create(ATagId: TTagId);
    destructor Destroy; override;
    procedure AddTagValue(AValue: string);
    function Evaluate(ATagMap: TTagMap): Boolean; override;
  end;

  TTagIsInConditionRef = TTagIsInCondition;

  { Information about a tag definition }
  TTagInfo = object
  public
    TagId: TTagId;  // unique id of this tag
    Name: string;
  end;

  TStringToTagMap = specialize TFPGMap<string, TTagId>;
  TNameTagIdToPrioMap = specialize TFPGMap<TTagId, LongWord>;
  TNameToMaxSpeedMap = specialize TFPGMap<string, Byte>;
  TSurfaceToGradeMap = specialize TFPGMap<string, Integer>;
  //TTagInfoList = specialize TFPGList<TTagInfo>;

  { TTagRegistry }

  TTagRegistry = class
  private
    FTags: array of TTagInfo;
    FNextTagId: TTagId;

    FStringToTagMap: TStringToTagMap;            //
    FNameTagIdToPrioMap: TNameTagIdToPrioMap;    //
    FNameAltTagIdToPrioMap: TNameTagIdToPrioMap; //
    FNameToMaxSpeedMap: TNameToMaxSpeedMap;      // mappings for max speed alias and value
    FSurfaceToGradeMap: TSurfaceToGradeMap;      // mappings for surfaces and surface grades
  public
    constructor Create();
    destructor Destroy(); override;

    function RegisterTag(ATagName: string): TTagId;
    function RegisterNameTag(ATagName: string; APriority: LongWord): TTagId;
    function RegisterNameAltTag(ATagName: string; APriority: LongWord): TTagId;

    function GetTagId(AName: string): TTagId;

    function IsNameTag(ATagId: TTagId; out APriority: LongWord): Boolean;
    function IsNameAltTag(ATagId: TTagId; out APriority: LongWord): Boolean;

    { Methods for dealing with mappings for surfaces and surface grades. }
    procedure RegisterSurfaceToGradeMapping(ASurface: string; AGrade: Integer);
    function ReadGradeForSurface(const ASurface: string; out AGrade: Integer): Boolean;
    { Methods for dealing with mappings for surfaces and surface grades. }
    procedure RegisterMaxSpeedAlias(AAlias: string; AMaxSpeed: Byte);
    function ReadMaxSpeedFromAlias(const AAlias: string; out AMaxSpeed: Byte): Boolean;
  end;

  TTagErrorReporter = class
  public
    procedure ReportTag(const AObject: TObjectOSMRef;
                        const ATags: TTagMap;
                        const AError: string); virtual; abstract;
  end;

  { TSilentTagErrorReporter }

  TSilentTagErrorReporter = class(TTagErrorReporter)
  public
    procedure ReportTag(const AObject: TObjectOSMRef;
                        const ATags: TTagMap;
                        const AError: string); override;
  end;

  function TagInfo(ATagId: TTagId; AName: string): TTagInfo;

implementation

uses LazDbgLog; // eliminate "end of source not found"

function TagInfo(ATagId: TTagId; AName: string): TTagInfo;
begin
  Result.TagId := ATagId;
  Result.Name := AName;
end;

{ TSilentTagErrorReporter }

procedure TSilentTagErrorReporter.ReportTag(const AObject: TObjectOSMRef;
  const ATags: TTagMap; const AError: string);
begin
  // silence
end;

{ TTagRegistry }

constructor TTagRegistry.Create();
begin
  //LogDebug('TTagRegistry.Create()');

  //FTags := TTagInfoList.Create();
  FStringToTagMap := TStringToTagMap.Create();
  FNameTagIdToPrioMap := TNameTagIdToPrioMap.Create();
  FNameAltTagIdToPrioMap := TNameTagIdToPrioMap.Create();
  FNameToMaxSpeedMap := TNameToMaxSpeedMap.Create();
  FSurfaceToGradeMap := TSurfaceToGradeMap.Create();

  FNextTagId := 0;

  // Make sure, that this is always registered first.
  // It assures that id 0 is always reserved for tagIgnore
  RegisterTag('');

  RegisterTag('area');
  RegisterTag('natural');
  RegisterTag('datapolygon');
  RegisterTag('type');
  RegisterTag('restriction');
  RegisterTag('junction');
end;

destructor TTagRegistry.Destroy();
begin
  //LogDebug('TTagRegistry.Destroy()');
  FreeAndNil(FSurfaceToGradeMap);
  FreeAndNil(FNameToMaxSpeedMap);
  FreeAndNil(FNameAltTagIdToPrioMap);
  FreeAndNil(FNameTagIdToPrioMap);
  FreeAndNil(FStringToTagMap);
  //FreeAndNil(FTags);
  inherited Destroy;
end;

function TTagRegistry.RegisterTag(ATagName: string): TTagId;
var
  n: Integer;
  TmpTagInfo: TTagInfo;
begin
  FStringToTagMap.Sorted := True;
  if FStringToTagMap.Find(ATagName, n) then
    Result := FStringToTagMap.Data[n]
  else
  begin
    TmpTagInfo.TagId := FNextTagId;
    TmpTagInfo.Name := ATagName;
    Inc(FNextTagId);
    //FTags.Add(TmpTagInfo);
    n := Length(FTags);
    SetLength(FTags, n + 1);
    FTags[n] := TmpTagInfo;

    FStringToTagMap.AddOrSetData(TmpTagInfo.Name, TmpTagInfo.TagId);
    Result := TmpTagInfo.TagId;
  end;
end;

function TTagRegistry.RegisterNameTag(ATagName: string; APriority: LongWord): TTagId;
begin
  Result := RegisterTag(ATagName);
  FNameTagIdToPrioMap.AddOrSetData(Result, APriority);
end;

function TTagRegistry.RegisterNameAltTag(ATagName: string; APriority: LongWord): TTagId;
begin
  Result := RegisterTag(ATagName);
  FNameAltTagIdToPrioMap.AddOrSetData(Result, APriority);
end;

function TTagRegistry.GetTagId(AName: string): TTagId;
var
  n: Integer;
begin
  FStringToTagMap.Sorted := True;
  if FStringToTagMap.Find(AName, n) then
    Result := FStringToTagMap.Data[n]
  else
    Result := TAG_IGNORE;
end;

function TTagRegistry.IsNameTag(ATagId: TTagId; out APriority: LongWord): Boolean;
var
  n: Integer;
begin
  Result := False;
  if FNameTagIdToPrioMap.Find(ATagId, n) then
  begin
    APriority := FNameTagIdToPrioMap.Data[n];
    Result := True;
  end;
end;

function TTagRegistry.IsNameAltTag(ATagId: TTagId; out APriority: LongWord): Boolean;
var
  n: Integer;
begin
  Result := False;
  if FNameAltTagIdToPrioMap.Find(ATagId, n) then
  begin
    APriority := FNameAltTagIdToPrioMap.Data[n];
    Result := True;
  end;
end;

procedure TTagRegistry.RegisterSurfaceToGradeMapping(ASurface: string;
  AGrade: Integer);
begin
  FSurfaceToGradeMap.AddOrSetData(ASurface, AGrade);
end;

function TTagRegistry.ReadGradeForSurface(const ASurface: string;
  out AGrade: Integer): Boolean;
var
  n: Integer;
begin
  if FSurfaceToGradeMap.Find(ASurface, n) then
  begin
    AGrade := FSurfaceToGradeMap.Data[n];
    Result := True;
  end
  else
    Result := False;
end;

procedure TTagRegistry.RegisterMaxSpeedAlias(AAlias: string; AMaxSpeed: Byte);
begin
  FNameToMaxSpeedMap.AddOrSetData(AAlias, AMaxSpeed);
end;

function TTagRegistry.ReadMaxSpeedFromAlias(const AAlias: string;
  out AMaxSpeed: Byte): Boolean;
var
  n: Integer;
begin
  if FNameToMaxSpeedMap.Find(AAlias, n) then
  begin
    AMaxSpeed := FNameToMaxSpeedMap.Data[n];
    Result := True;
  end
  else
    Result := False;
end;

{ TTagIsInCondition }

constructor TTagIsInCondition.Create(ATagId: TTagId);
begin
  FTagValues := TStringList.Create();
  FTagId := ATagId;
end;

destructor TTagIsInCondition.Destroy;
begin
  FreeAndNil(FTagValues);
  inherited Destroy;
end;

procedure TTagIsInCondition.AddTagValue(AValue: string);
begin
  FTagValues.Add(AValue);
end;

function TTagIsInCondition.Evaluate(ATagMap: TTagMap): Boolean;
var
  n: Integer;
  s: string;
begin
  Result := False;
  if ATagMap.Find(FTagId, n) then
  begin
    s := ATagMap.Data[n];
    Result := (FTagValues.IndexOf(s) >= 0);
  end;
end;

{ TTagBinaryCondition }

constructor TTagBinaryCondition.Create(ATagId: TTagId;
  ABinaryOperator: TBinaryOperator; ATagValue: string);
begin
  inherited Create();
  FTagId := ATagId;
  FBinaryOperator := ABinaryOperator;
  FTagStringValue := ATagValue;
  FValueType := vtString;
end;

constructor TTagBinaryCondition.Create(ATagId: TTagId;
  ABinaryOperator: TBinaryOperator; ATagValue: Integer);
begin
  FTagId := ATagId;
  FBinaryOperator := ABinaryOperator;
  FTagSizeValue := ATagValue;
  FValueType := vtInteger;
end;

function TTagBinaryCondition.Evaluate(ATagMap: TTagMap): Boolean;
var
  n: Integer;
  s: string;
begin
  Result := False;

  if not ATagMap.Find(FTagId, n) then
    Exit;

  s := ATagMap.Data[n];

  if (FValueType = vtString) then
  begin
    case FBinaryOperator of
      operatorLess:         Result := (s <  FTagStringValue);
      operatorLessEqual:    Result := (s <= FTagStringValue);
      operatorEqual:        Result := (s =  FTagStringValue);
      operatorNotEqual:     Result := (s <> FTagStringValue);
      operatorGreaterEqual: Result := (s >= FTagStringValue);
      operatorGreater:      Result := (s >  FTagStringValue);
    else
      Assert(false);
    end;
  end
  else if (FValueType = vtInteger) then
  begin
    if (not TryStrToInt(s, n)) then
      Exit;

    case FBinaryOperator of
      operatorLess:         Result := (n <  FTagSizeValue);
      operatorLessEqual:    Result := (n <= FTagSizeValue);
      operatorEqual:        Result := (n =  FTagSizeValue);
      operatorNotEqual:     Result := (n <> FTagSizeValue);
      operatorGreaterEqual: Result := (n >= FTagSizeValue);
      operatorGreater:      Result := (n >  FTagSizeValue);
    else
      Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TTagExistsCondition }

constructor TTagExistsCondition.Create(ATagId: TTagId);
begin
  inherited Create();
  FTagId := ATagId;
end;

function TTagExistsCondition.Evaluate(ATagMap: TTagMap): Boolean;
var
  n: Integer;
begin
  Result := ATagMap.Find(FTagId, n);
end;

{ TTagBoolCondition }

constructor TTagBoolCondition.Create(ATagBoolType: TTagBoolType);
begin
  inherited Create();
  FTagBoolType := ATagBoolType;
  FConditions := TTagConditionList.Create();
end;

destructor TTagBoolCondition.Destroy();
begin
  FreeAndNil(FConditions);
  inherited Destroy();
end;

procedure TTagBoolCondition.AddCondition(ACondition: TTagConditionRef);
begin
  FConditions.Add(ACondition);
end;

function TTagBoolCondition.Evaluate(ATagMap: TTagMap): Boolean;
var
  i: Integer;
  Item: TTagConditionRef;
begin
  Result := False;
  case FTagBoolType of
    boolAnd:
    begin
      // for (Item in FConditions) do
      for i := 0 to FConditions.Count-1 do
      begin
        Item := FConditions.Items[i];
        if not Item.Evaluate(ATagMap) then
          Exit(False);
      end;
      Result := True;
    end;

    boolOr:
    begin
      //for (Item in FConditions) do
      for i := 0 to FConditions.Count-1 do
      begin
        Item := FConditions.Items[i];
        if Item.Evaluate(ATagMap) then
          Exit(True);
      end;
    end;
  else
    Assert(False);
  end;
end;

{ TTagNotCondition }

constructor TTagNotCondition.Create(ACondition: TTagConditionRef);
begin
  inherited Create();
  FCondition := ACondition;
end;

function TTagNotCondition.Evaluate(ATagMap: TTagMap): Boolean;
begin
  Result := not FCondition.Evaluate(ATagMap);
end;

end.

