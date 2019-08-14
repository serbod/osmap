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

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fgl,
  {$else}
  System.Generics.Collections,
  {$endif}
  OsMapTypes, OsMapUtils;

const
  { Magic constant for an unresolved and to be ignored tag }
  TAG_IGNORE = 0;

  TAG_AREA = 'area';
  TAG_NATURAL = 'natural';

type
  { Tag types}

  TTagId = Word;

  //TTagMap = specialize TFPGMap<TTagId, string>;
  TTagMap = class(TStringList)
  public
    function GetKeyByIndex(AIndex: Integer): TTagId;
    procedure SetValue(ATagId: TTagId; AValue: string);
    function GetValue(ATagId: TTagId): string;
    function FindValue(ATagId: TTagId; out AIndex: Integer): Boolean;
    property Values[ATagId: TTagId]: string read GetValue write SetValue; default;
  end;

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
  TTagCondition = class(TObject)
  public
    function Evaluate(ATagMap: TTagMap): Boolean; virtual; abstract;
  end;

  {$ifdef FPC}
  TTagConditionList = specialize TFPGObjectList<TTagCondition>;
  {$else}
  TTagConditionList = TObjectList<TTagCondition>;
  {$endif}

  { Negates the result of the given child condition }
  TTagNotCondition = class(TTagCondition)
  private
    FCondition: TTagCondition;
  public
    constructor Create(ACondition: TTagCondition);
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
    procedure AddCondition(ACondition: TTagCondition);
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
                       ATagValue: string); overload;

    constructor Create(ATagId: TTagId;
                       ABinaryOperator: TBinaryOperator;
                       ATagValue: Integer); overload;

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

  //TStringToTagMap = specialize TFPGMap<string, TTagId>;
  //TNameTagIdToPrioMap = specialize TFPGMap<TTagId, LongWord>;
  //TNameToMaxSpeedMap = specialize TFPGMap<string, Byte>;
  //TSurfaceToGradeMap = specialize TFPGMap<string, Integer>;
  //TTagInfoList = specialize TFPGList<TTagInfo>;

  TSurfaceToGradeMap = class(TStringList)
  public
    procedure SetValue(const AName: string; AValue: Integer);
    function GetValue(const AName: string): Integer;
  end;

  TNameTagIdToPrioMap = class(TStringList)
  public
    procedure SetValue(ATagId: TTagId; AValue: LongWord);
    function GetValue(ATagId: TTagId): LongWord;
  end;

  { TTagRegistry }

  TTagRegistry = class
  private
    FTags: array of TTagInfo;
    FNextTagId: TTagId;

    FStringToTagMap: TSimpleStringHash;          //
    FNameTagIdToPrioMap: TNameTagIdToPrioMap;    //
    FNameAltTagIdToPrioMap: TNameTagIdToPrioMap; //
    FNameToMaxSpeedMap: TSimpleStringHash;       // mappings for max speed alias and value
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

uses Math; // eliminate "end of source not found"

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
  FStringToTagMap.Init();
  FNameTagIdToPrioMap := TNameTagIdToPrioMap.Create();
  FNameAltTagIdToPrioMap := TNameTagIdToPrioMap.Create();
  FNameToMaxSpeedMap.Init();
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
  FNameToMaxSpeedMap.Clear();
  FreeAndNil(FNameAltTagIdToPrioMap);
  FreeAndNil(FNameTagIdToPrioMap);
  FStringToTagMap.Clear();
  //FreeAndNil(FTags);
  inherited Destroy;
end;

function TTagRegistry.RegisterTag(ATagName: string): TTagId;
var
  n: Integer;
  TmpTagInfo: TTagInfo;
begin
  //FStringToTagMap.Sorted := True;
  if FStringToTagMap.FindValue(ATagName, n) then
    Result := FTags[n].TagId
  else
  begin
    TmpTagInfo.TagId := FNextTagId;
    TmpTagInfo.Name := ATagName;
    Inc(FNextTagId);
    //FTags.Add(TmpTagInfo);
    n := Length(FTags);
    SetLength(FTags, n + 1);
    FTags[n] := TmpTagInfo;

    FStringToTagMap.Add(TmpTagInfo.Name, TmpTagInfo.TagId);
    Result := TmpTagInfo.TagId;
  end;
end;

function TTagRegistry.RegisterNameTag(ATagName: string; APriority: LongWord): TTagId;
begin
  Result := RegisterTag(ATagName);
  FNameTagIdToPrioMap.SetValue(Result, APriority);
end;

function TTagRegistry.RegisterNameAltTag(ATagName: string; APriority: LongWord): TTagId;
begin
  Result := RegisterTag(ATagName);
  FNameAltTagIdToPrioMap.SetValue(Result, APriority);
end;

function TTagRegistry.GetTagId(AName: string): TTagId;
var
  n: Integer;
begin
  if FStringToTagMap.FindValue(AName, n) then
    Result := FTags[n].TagId
  else
    Result := TAG_IGNORE;
end;

function TTagRegistry.IsNameTag(ATagId: TTagId; out APriority: LongWord): Boolean;
var
  n: Integer;
begin
  Result := False;
  n := FNameTagIdToPrioMap.IndexOfName(IntToStr(ATagId));
  if n <> -1 then
  begin
    APriority := StrToIntDef(FNameTagIdToPrioMap.ValueFromIndex[n], 0);
    Result := True;
  end;
end;

function TTagRegistry.IsNameAltTag(ATagId: TTagId; out APriority: LongWord): Boolean;
var
  n: Integer;
begin
  Result := False;
  n := FNameAltTagIdToPrioMap.IndexOfName(IntToStr(ATagId));
  if n <> -1 then
  begin
    APriority := StrToIntDef(FNameAltTagIdToPrioMap.ValueFromIndex[n], 0);
    Result := True;
  end;
end;

procedure TTagRegistry.RegisterSurfaceToGradeMapping(ASurface: string;
  AGrade: Integer);
begin
  FSurfaceToGradeMap.SetValue(ASurface, AGrade);
end;

function TTagRegistry.ReadGradeForSurface(const ASurface: string;
  out AGrade: Integer): Boolean;
var
  n: Integer;
begin
  n := FSurfaceToGradeMap.IndexOfName(ASurface);
  if n <> -1 then
  begin
    AGrade := StrToIntDef(FSurfaceToGradeMap.ValueFromIndex[n], 0);
    Result := True;
  end
  else
    Result := False;
end;

procedure TTagRegistry.RegisterMaxSpeedAlias(AAlias: string; AMaxSpeed: Byte);
begin
  if FNameToMaxSpeedMap.ValueOf(AAlias) <> -1 then
    FNameToMaxSpeedMap.Add(AAlias, AMaxSpeed);
end;

function TTagRegistry.ReadMaxSpeedFromAlias(const AAlias: string;
  out AMaxSpeed: Byte): Boolean;
var
  n: Integer;
begin
  if FNameToMaxSpeedMap.FindValue(AAlias, n) then
  begin
    AMaxSpeed := Byte(n);
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
  if ATagMap.FindValue(FTagId, n) then
  begin
    s := ATagMap.ValueFromIndex[n];
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

  if not ATagMap.FindValue(FTagId, n) then
    Exit;

  s := ATagMap.ValueFromIndex[n];

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
  Result := ATagMap.FindValue(FTagId, n);
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

procedure TTagBoolCondition.AddCondition(ACondition: TTagCondition);
begin
  FConditions.Add(ACondition);
end;

function TTagBoolCondition.Evaluate(ATagMap: TTagMap): Boolean;
var
  i: Integer;
  Item: TTagCondition;
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

constructor TTagNotCondition.Create(ACondition: TTagCondition);
begin
  inherited Create();
  FCondition := ACondition;
end;

function TTagNotCondition.Evaluate(ATagMap: TTagMap): Boolean;
begin
  Result := not FCondition.Evaluate(ATagMap);
end;

{ TTagMap }

function TTagMap.FindValue(ATagId: TTagId; out AIndex: Integer): Boolean;
begin
  AIndex := IndexOfName(IntToStr(ATagId));
  Result := (AIndex <> -1);
end;

function TTagMap.GetKeyByIndex(AIndex: Integer): TTagId;
begin
  Result := StrToIntDef(Names[AIndex], 0);
end;

function TTagMap.GetValue(ATagId: TTagId): string;
var
  n: Integer;
begin
  n := IndexOfName(IntToStr(ATagId));
  if n <> -1 then
    Result := ValueFromIndex[n]
  else
    Result := '';
end;

procedure TTagMap.SetValue(ATagId: TTagId; AValue: string);
var
  n: Integer;
begin
  n := IndexOfName(IntToStr(ATagId));
  if n <> -1 then
    ValueFromIndex[n] := AValue
  else
    Append(IntToStr(ATagId) + NameValueSeparator + AValue);
end;

{ TNameTagIdToPrioMap }

function TNameTagIdToPrioMap.GetValue(ATagId: TTagId): LongWord;
var
  n: Integer;
begin
  n := IndexOfName(IntToStr(ATagId));
  if n <> -1 then
    Result := StrToIntDef(ValueFromIndex[n], 0)
  else
    Result := 0;
end;

procedure TNameTagIdToPrioMap.SetValue(ATagId: TTagId; AValue: LongWord);
var
  n: Integer;
begin
  n := IndexOfName(IntToStr(ATagId));
  if n <> -1 then
    ValueFromIndex[n] := IntToStr(AValue)
  else
    Append(IntToStr(ATagId) + NameValueSeparator + IntToStr(AValue));
end;

{ TSurfaceToGradeMap }

function TSurfaceToGradeMap.GetValue(const AName: string): Integer;
var
  n: Integer;
begin
  n := IndexOfName(AName);
  if n <> -1 then
    Result := StrToIntDef(ValueFromIndex[n], 0)
  else
    Result := 0;
end;

procedure TSurfaceToGradeMap.SetValue(const AName: string; AValue: Integer);
begin
  Values[AName] := IntToStr(AValue);
end;

end.

