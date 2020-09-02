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
util\Number (...)
Breaker:
  Breaker -> TBreakerObject

util/Cache:
  Cache
*)
unit OsMapUtils;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$ZEROBASEDSTRINGS OFF}
{$endif}

interface


uses
  Classes, SysUtils;

type

  { TBreakerObject }

  TBreakerObject = object
    IsAborted: Boolean;
    procedure Init();
    procedure Break();
  end;

  { TStopClock }

  TStopClock = object
    StartTime: TDateTime;
    StopTime: TDateTime;
    procedure Init();
    procedure Stop();
    function IsSignificant(): Boolean;
    function ResultString(): string;
  end;

  //PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: Integer;
  end;

  { TSimpleStringHash }
  { Simple string-to-integer hashtable }
  TSimpleStringHash = object
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: string): PHashItem;
    function HashOf(const Key: string): Cardinal;
  public
    procedure Init(Size: Cardinal = 256);
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    function Modify(const Key: string; Value: Integer): Boolean;
    { return -1 if no value for key, suitable for list index }
    function ValueOf(const Key: string): Integer;
    function FindValue(const Key: string; out Value: Integer): Boolean;
  end;

  TCompareFunc = function (const elem1, elem2): Integer;

  {TCache = object

  end; }

{ Sort any array }
procedure AnySort(const Arr; Count: Integer; ItemSize: Integer; CompareFunc: TCompareFunc);

{ Encode a signed number into the given buffer using some variable length encoding.
  The methods returns the number of bytes written.

  The first bit (if set) signals a negative numer. The highest bit in a byte
  is set, if there is an additional byte following. So we use one bit
  for signaling signess and use 7 of 8 bytes per byte for data.

  The current implementation thus requires the buffer to have at least space
  for sizeof(N)*8/7 + 1/8 bytes:
  This are 5 bytes for a 32bit value and 10 bytes for a 64bit value. }
function EncodeNumberSigned(ANum: Int64; var ABuffer): Integer;

{ Encode an unsigned number into the given buffer using some variable length encoding.
  The highest bit in a byte is set, if there is an additional byte following.
  So we use 7 of 8 bytes per byte for data.
  The methods returns the number of bytes written.

  The current implementation requires the buffer to have at least space
  for sizeof(N)*8/7 bytes:
  This are 5 bytes for a 32bit value and 10 bytes for a 64bit value. }
function EncodeNumberUnsigned(ANum: UInt64; var ABuffer): Integer;

{ Encode a number into the given buffer using some variable length encoding.
  The methods returns the number of bytes written.

  The current implementation requires the buffer to have at least space
  for sizeof(N)*8/7 bytes for an unsigned number
  and sizeof(N)*8/7 + 1/8 bytes for a signed number

  This are 5 bytes for a 32bit value and 10 bytes for a64bit value. }
function EncodeNumber(ANum: Int64; var ABuffer): Integer; overload;
function EncodeNumber(ANum: UInt64; var ABuffer): Integer; overload;

{ Decode a signed variable length encoded number from the buffer back to
  the variable.
  The methods returns the number of bytes read. }
function DecodeNumberSigned(const ABuffer; out ANum: Int64): Integer;

{ Decode a unsigned variable length encoded number from the buffer back to
  the variable.
  The methods returns the number of bytes read. }
function DecodeNumberUnsigned(const ABuffer; out ANum: UInt64): Integer;

{ Decode a variable length encoded number from the buffer back to
  the variable.
  The methods returns the number of bytes read. }
function DecodeNumber(const ABuffer; out ANum: Int64): Integer; overload;
function DecodeNumber(const ABuffer; out ANum: UInt64): Integer; overload;

{ Returns the number of bytes needed to encode the number. The function calculates
  the number of bytes that contain information, dropping leading bytes that only
  contain zero. }
function BytesNeededToEncodeNumber(ANum: Int64): Byte;

{ Returns the number of bytes needed to encode the number. The function calculates
  the number of bytes that contain information, dropping leading bytes that only
  contain zero. }
function BitsNeededToEncodeNumber(ANum: Int64): Byte;

{ Encodes the given numbers into a new number of twice the size that has
  the bits of each nunber alternating interleaved.

  This can be used to convert two dimensional coordinates into
  one number, where coordinates close in 2D are close in the
  one dimensional projection, too. }
function InterleaveNumbers(A, B: LongWord): UInt64;

{ Returns the number of bytes needed to encode the given number of bits. }
function BitsToBytes(ABits: Integer): Integer;

{:Fetch string from left of Value string.}
function Fetch(var Value: string; const Delimiter: string): string;

{ Return True if bit with BitIndex is set in Value }
function GetBit(const Value; BitIndex: Integer): Boolean;
{ Set bit with BitIndex (0..BitCount) in Value }
procedure SetBit(var Value; BitIndex: Integer; BitValue: Boolean);

procedure AppendToArray(var AArray; const AValue);

var
  LogFile: TextFile;

implementation

uses Math; // eliminate "end of source not found"

function EncodeNumberSigned(ANum: Int64; var ABuffer): Integer;
var
  pBuf: PByte;
begin
  pBuf := @ABuffer;
  if ANum < 0 then
    pBuf^ := 1
  else
    pBuf^ := 0;

  pBuf^ := pBuf^ or Byte((ANum and $3F) shl 1);
  ANum := ANum shr 6;
  Result := 1;

  while ANum > 0 do
  begin
    pBuf^ := pBuf^ or $80;
    Inc(pBuf);

    pBuf^ := Byte(ANum and $7F);
    ANum := ANum shr 7;
    Inc(Result);
  end;
end;

function EncodeNumberUnsigned(ANum: UInt64; var ABuffer): Integer;
var
  pBuf: PByte;
begin
  pBuf := @ABuffer;
  Result := 0;

  while ANum > $7F do
  begin
    pBuf^ := Byte(ANum and $7F) or $80;
    Inc(pBuf);
    ANum := ANum shr 7;
    Inc(Result);
  end;

  pBuf^ := Byte(ANum and $7F);
  Inc(Result);
end;

function EncodeNumber(ANum: Int64; var ABuffer): Integer;
begin
  Result := EncodeNumberSigned(ANum, ABuffer);
end;

function EncodeNumber(ANum: UInt64; var ABuffer): Integer;
begin
  Result := EncodeNumberUnsigned(ANum, ABuffer);
end;

function DecodeNumberSigned(const ABuffer; out ANum: Int64): Integer;
var
  pBuf: PByte;
  Val: Int64;
  Shift, NextShift: Integer;
begin
  pBuf := @ABuffer;
  Shift := 0;
  Result := 1;

  if (pBuf^ and $01) <> 0 then
    ANum := -1
  else
    ANum := 0;

  Val := (pBuf^ and $7E) shr 1;
  NextShift := 6;

  while (pBuf^ and $80) <> 0 do
  begin
    ANum := ANum or (Val shl Shift);
    Inc(pBuf);
    Val := (pBuf^ and $7F);
    Shift := NextShift;
    Inc(NextShift, 7);
    Inc(Result);
  end;
  ANum := ANum or (Val shl Shift);
end;

function DecodeNumberUnsigned(const ABuffer; out ANum: UInt64): Integer;
var
  pBuf: PByte;
  Shift: Integer;
begin
  pBuf := @ABuffer;
  Shift := 0;
  Result := 1;
  ANum := 0;

  while True and (Shift < 48) do
  begin
    ANum := ANum or (UInt64(pBuf^ and $7F) shl Shift);
    if (pBuf^ and $80) = 0 then
      Break;
    Inc(pBuf);
    Inc(Shift, 7);
    Inc(Result);
  end;
end;

function DecodeNumber(const ABuffer; out ANum: Int64): Integer;
begin
  Result := DecodeNumberSigned(ABuffer, ANum);
end;

function DecodeNumber(const ABuffer; out ANum: UInt64): Integer;
begin
  Result := DecodeNumberUnsigned(ABuffer, ANum);
end;

function BytesNeededToEncodeNumber(ANum: Int64): Byte;
begin
  Result := 0;
  while (ANum <> 0) do
  begin
    ANum := ANum div 256;
    Inc(Result);
  end;

  if Result = 0 then
    Result := 1;
end;

function BitsNeededToEncodeNumber(ANum: Int64): Byte;
begin
  Result := 0;
  while (ANum <> 0) do
  begin
    ANum := ANum div 2;
    Inc(Result);
  end;

  if Result = 0 then
    Result := 1;
end;

function InterleaveNumbers(A, B: LongWord): UInt64;
var
  i, nBit: Integer;
begin
  Result := 0;
  for i := 0 to 31 do
  begin
    nBit := 31-i;
    Result := Result shl 1;
    Result := Result + ((A shr nBit) and $01);

    Result := Result shl 1;
    Result := Result + ((B shr nBit) and $01);
  end;
end;

function BitsToBytes(ABits: Integer): Integer;
begin
  if (ABits mod 8) = 0 then
    Result := (ABits div 8)
  else
    Result := (ABits div 8) + 1;
end;

function Fetch(var Value: string; const Delimiter: string): string;
var
  n: Integer;
begin
  n := Pos(Delimiter, Value);
  if n = 0 then n := MaxInt-1;
  Result := Copy(Value, 1, n-1);
  Value := Copy(Value, n+1, MaxInt);
end;

function GetBit(const Value; BitIndex: Integer): Boolean;
var
  pb: PByte;
begin
  pb := @Value;
  Inc(pb, BitIndex div 8);  // point to n-th byte of Value
  Result := ((pb^ shr (BitIndex mod 8)) and 1) = 1;
end;

procedure SetBit(var Value; BitIndex: Integer; BitValue: Boolean);
var
  pb: PByte;
  BitOffset: Integer;
begin
  pb := @Value;
  BitOffset := BitIndex mod 8;
  Inc(pb, BitIndex div 8);  // point to n-th byte of Value
  if BitValue then
    pb^ := pb^ or (1 shl BitOffset)
  else
    pb^ := pb^ and (not (1 shl BitOffset));
end;

procedure AppendToArray(var AArray; const AValue);
begin

end;

{ TBreakerObject }

procedure TBreakerObject.Init();
begin
  IsAborted := False;
end;

procedure TBreakerObject.Break();
begin
  IsAborted := True;
end;

{ TStopClock }

procedure TStopClock.Init();
begin
  StartTime := Now();
  StopTime := 0;
end;

procedure TStopClock.Stop();
begin
  StopTime := Now();
end;

function TStopClock.IsSignificant(): Boolean;
begin
  Result := (StopTime > StartTime);
end;

function TStopClock.ResultString(): string;
begin
  Result := FormatDateTime('S.Z', (StopTime - StartTime));
end;

{ TSimpleStringHash }

function TSimpleStringHash.Find(const Key: string): PHashItem;
var
  Hash: Cardinal;
begin
  Assert(Length(Buckets) > 0);
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := Buckets[Hash];
  while Result <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
    begin
      Result := Result^.Next;
    end;
  end;
end;

function TSimpleStringHash.HashOf(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[I]);
end;

procedure TSimpleStringHash.Init(Size: Cardinal);
begin
  SetLength(Buckets, Size);
end;

procedure TSimpleStringHash.Add(const Key: string; Value: Integer);
var
  Hash: Cardinal;
  Bucket: PHashItem;
begin
  Assert(Length(Buckets) > 0);
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TSimpleStringHash.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

function TSimpleStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  P: PHashItem;
begin
  P := Find(Key);
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

function TSimpleStringHash.ValueOf(const Key: string): Integer;
var
  P: PHashItem;
begin
  P := Find(Key);
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

function TSimpleStringHash.FindValue(const Key: string; out Value: Integer): Boolean;
var
  P: PHashItem;
begin
  P := Find(Key);
  Result := (P <> nil);
  if Result then
    Value := P^.Value;
end;

procedure AnyQuickSort(const Arr; idxL, idxH: Integer;
  ItemSize: Integer; CompareFunc: TCompareFunc; var SwapBuf);
var
  ls, hs: Integer;
  li, hi: Integer;
  mi    : Integer;
  ms    : Integer;
  pb    : PByteArray;
begin
  pb := @Arr;
  li := idxL;
  hi := idxH;
  mi := (li+hi) div 2;
  ls := li * ItemSize;
  hs := hi * ItemSize;
  ms := mi * ItemSize;
  repeat
    while CompareFunc( pb[ls], pb[ms] ) < 0 do
    begin
      Inc(ls, ItemSize);
      Inc(li);
    end;
    while CompareFunc( pb[ms], pb[hs] ) < 0 do
    begin
      Dec(hs, ItemSize);
      Dec(hi);
    end;
    if ls <= hs then
    begin
      Move(pb[ls], SwapBuf, ItemSize);
      Move(pb[hs], pb[ls], ItemSize);
      Move(SwapBuf, pb[hs], ItemSize);
      Inc(ls, ItemSize); Inc(li);
      Dec(hs, ItemSize); Dec(hi);
    end;
  until ls > hs;
  if hi > idxL then AnyQuickSort(Arr, idxL, hi, ItemSize, CompareFunc, SwapBuf);
  if li < idxH then AnyQuickSort(Arr, li, idxH, ItemSize, CompareFunc, SwapBuf);
end;

procedure AnySort(const Arr; Count: Integer; ItemSize: Integer; CompareFunc: TCompareFunc);
var
  buf: array of byte;
begin
  SetLength(buf, ItemSize);
  AnyQuickSort(Arr, 0, Count-1, ItemSize, compareFunc, buf[0]);
end;

end.

