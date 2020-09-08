{-----------------------------------------------------------------------------
Всякие полезные функции

(C) Sergey Bodrov (serbod@gmail.com)
MIT license
-----------------------------------------------------------------------------}
unit RFUtils;                                     

{$ifdef FPC}
  {$mode DELPHI}
  {$define TRANSCODE_STRINGS}
{$else}
  {$ifndef ANDROID}
    {$define WIN_NATIVE}
  {$else}
    {$ZEROBASEDSTRINGS OFF}
  {$endif}
{$endif}

interface

uses
  {$ifdef WIN_NATIVE}Windows, {$endif}
  Types, Classes, SysUtils;

{$ifndef ANDROID}
// запись/чтение строки байтов в/из потока (TStream), с маркером длины
procedure WriteAnsiStringToStream(Stream: TStream; const Str: AnsiString);
function ReadAnsiStringFromStream(Stream: TStream; var Str: AnsiString): Boolean;
function GetAnsiStringFromStream(Stream: TStream): AnsiString;
// сохраняет содержимое потока в строку байтов с текущей позиции до конца
function StreamToAnsiString(AStream: TStream): AnsiString;
{$endif}
// запись/чтение строки текста в/из потока (TStream), с маркером длины
// кодировка в потоке UTF8, перекодировка в/из основной автоматически
procedure WriteStringToStream(AStream: TStream; const AStr: string);
function ReadStringFromStream(AStream: TStream; out AStr: string): Boolean;
// для случаев, когда нельзя передать строку как параметр
function GetStringFromStream(AStream: TStream): string;
// сохраняет содержимое потока в строку байтов с текущей позиции до конца
//function StreamToString(AStream: TStream): UTF8String;

// запись/чтение массива байтов в/из потока (TStream), с маркером длины
procedure WriteBytesToStream(Stream: TStream; const Data: TByteDynArray);
function ReadBytesFromStream(Stream: TStream; var Data: TByteDynArray): Boolean;

function StrToBytes(const AValue: string): TByteDynArray;
function BytesToStr(const AValue: TByteDynArray): string;

// простой и быстрый генератор псевдослучайных чисел
function XorShift32(var ASeed: LongWord): LongWord;
function XorShift64(var ASeed: Int64): Int64;
// закодировать буфер младшим байтом из Code
procedure Encode(var Data; DataLen, Code: Byte);
// раскодировать буфер младшим байтом из Code
procedure Decode(var Data; DataLen, Code: Byte);

// закодировать строку
function EncodeStr(const AStr: string; ACode: Integer): string; deprecated;
// раскодировать строку
function DecodeStr(const AStr: string; ACode: Integer): string; deprecated;
// XOR-кодирование массива байтов
procedure XorData(var AData: TByteDynArray; ACode: Integer);

function HexToByte(const HexValue: String): Byte;
function HexToInt(const Value: String): Int64;
function MACAddrToInt64(const AMacAddr: string): Int64;
// в строку "AA-BB-CC-DD-EE-FF"
function Int64ToMACAddr(AInt64: Int64): string;

{$IFNDEF FPC}
// Аналог стандартной функции из Windows
function GetTickCount(): Cardinal;
// Аналог стандартной функции из Windows
//function GetTickCount64(): Int64;
type TGetTickCount64 = function: Int64; stdcall;
{$EXTERNALSYM GetTickCount64}
var GetTickCount64: TGetTickCount64;
{$ENDIF}
// разница в тиках между текущим и предыдущим показанием, 0..MaxInt
function TickDiff(TickNow: Int64; TickPrev: Int64): Integer; overload;
function MinutesBetweenTicks(TickNow: Int64; TickPrev: Int64): Integer; overload;
function SecondsBetweenTicks(TickNow: Int64; TickPrev: Int64): Integer; overload;

// возвращает строку с расшифровкой количества секунд в часах-минутах-секундах
function SecondsToTimeStr(ASec: Cardinal): string;
// возвращает строку с расшифровкой количества секунд в минутах-секундах
function SecondsToTimeStrShort(ASec: Cardinal): string;
// Аналог стандартной функции из DateUtils
function SecondsBetween(const ANow, AThen: TDateTime): Int64;
// Аналог стандартной функции из DateUtils
function DateOf(const AValue: TDateTime): TDateTime;

// извлекает первое слово из строки, по умолчанию разделитель пробел
function ExtractFirstWord(var AStr: string; const WordSeparator: string = ' '): string;
// аналог стандартной функции из StrUtils
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
// добавляет строку AStr в конец строки ADest. Если AStr не пустая, до перед AStr
// добавляется разделитель ASeparator
procedure AppendStrEx(var ADest: string; const AStr: string; ASeparator: string = '');

// возвращает строку, содержащую только символы из строки-фильтра
function GetFilteredString(const AStr, AFilterStr: string): string;
// возвращает строку, содержащую только цифры (0..9) из исходной строки
function GetPureNumber(const ANumStr: String): String;
// возвращает строку, содержащую только Hex-символы (0..9;A..F) из исходной строки
function GetPureHex(const AStr: string): string;
// добавляет разделители тысяч
function FormatInt(ANum: Int64; ASep: string = ' '): string;

// Выдает HEX-строку содержимого буфера, байты разделены пробелами
// добавляет квадратные скобки в начале и в конце
function BufferToHex(const Buffer; BufferSize: Integer): string;
// Выдает HEX-строку содержимого буфера, заглавными буквами без пробелов
function BufToHex(const Buffer; BufferSize: Integer): string;

// к указанной строке добавляет информацию о текущем исключении
// можно использовать для обработчиков исключений
function GetExceptionMessage(): string;

{ Проверка наличия в значении Value бита BitIndex (0..BitCount) }
function CheckBit(BitIndex: Integer; Value: Byte): Boolean; overload;
function CheckBit(BitIndex: Integer; Value: Word): Boolean; overload;
function CheckBit(BitIndex: Integer; Value: Cardinal): Boolean; overload;
function GetBit(const Value; BitIndex: Integer): Boolean;
{ В значении Value назначение бита с индексом BitIndex (0..BitCount) }
procedure SetBit(var Value; BitIndex: Integer; BitValue: Boolean);
{ подсчет количества битов }
function BitCount(n: Word): Integer;

{$ifndef ANDROID}
// копирование файла
function CopyFile(const ASource, ADest: TFileName; AKeepExisting: Boolean): Boolean;
// Определение кол-ва файлов в определённой папке по маске
function GetFileCount(const Dir, Mask: string): Integer;
// Удалить первых N файлов в определённой папке по маске
procedure DeleteFileCount(const Dir, Mask: string; N: Integer);
{$endif}

{$ifdef WIN_NATIVE}
// версия файла
function GetFileVersion(const AFileName: string; var VersionMS, VersionLS: LongWord): LongWord;
// версия программы/DLL
function GetAppVersion(): LongWord;
{$endif}

var
  RFFormatSettings: TFormatSettings;

implementation

{$IFDEF TRANSCODE_STRINGS}
uses LConvEncoding, StrUtils;
{$ENDIF}

var
  PrevTickCount: Cardinal;
  TickCount64Base: Int64;
  {$ifdef ANDROID}
  CP1251Encoding: TEncoding;
  {$endif}

{$ifndef ANDROID}
// запись строки байтов в поток (TStream)
procedure WriteAnsiStringToStream(Stream: TStream; const Str: AnsiString);
var
  Len: Cardinal;
begin
  Assert(Assigned(Stream));
  Len := Length(Str);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    Stream.WriteBuffer(Str[1], Len);
end;

// чтение строки байтов из потока (TStream)
function ReadAnsiStringFromStream(Stream: TStream; var Str: AnsiString): Boolean;
var
  Len: LongWord;
begin
  if (Stream.Size - Stream.Position) >= SizeOf(Len) then
    Stream.ReadBuffer(Len, SizeOf(Len))
  else
    Len := 0;
  SetLength(Str, Len);
  Result := (Len > 0);
  if Result then
    Stream.ReadBuffer(Str[1], Len);
end;

function GetAnsiStringFromStream(Stream: TStream): AnsiString;
begin
  Result := '';
  ReadAnsiStringFromStream(Stream, Result);
end;

function StreamToAnsiString(AStream: TStream): AnsiString;
var
  n: Integer;
begin
  SetLength(Result, AStream.Size - AStream.Position);
  n := AStream.Read(PAnsiChar(Result)^, AStream.Size - AStream.Position);
  if n <> Length(Result) then SetLength(Result, n);
end;
{$endif}

// запись строки в поток (TStream)
procedure WriteStringToStream(AStream: TStream; const AStr: string);
var
  n: LongWord;
  s: UTF8String;
begin
  {$ifdef UNICODE}
  s := UTF8Encode(AStr);
  {$else}
  s := AnsiToUtf8(AStr);
  {$endif}

  n := Length(s);
  AStream.WriteBuffer(n, SizeOf(n));
  if n > 0 then
    AStream.WriteBuffer(s[1], n);
end;

// чтение строки из потока (TStream)
function ReadStringFromStream(AStream: TStream; out AStr: string): Boolean;
var
  n: LongWord;
  s: UTF8String;
begin
  Result := ((AStream.Size - AStream.Position) >= SizeOf(n));
  if not Result then
    Exit;

  AStream.ReadBuffer(n, SizeOf(n));

  Result := ((AStream.Size - AStream.Position) >= n);
  if (n = 0) or (not Result) then
    Exit;
  SetLength(s, n);
  AStream.ReadBuffer(s[1], n);
  {$ifdef UNICODE}
  AStr := UTF8ToString(s);
  {$else}
  AStr := UTF8ToAnsi(s);
  {$endif}
end;

function GetStringFromStream(AStream: TStream): string;
begin
  if not ReadStringFromStream(AStream, Result) then
    Result := '';
end;

(*
function StreamToString(AStream: TStream): UTF8String;
var
  n: Integer;
begin
  { TODO : char size }
  SetLength(Result, AStream.Size - AStream.Position);
  n := AStream.Read(Result[1], AStream.Size - AStream.Position);
  if n <> Length(Result) then SetLength(Result, n);
end;
*)
// запись/чтение массива байтов в/из потока (TStream), с маркером длины
procedure WriteBytesToStream(Stream: TStream; const Data: TByteDynArray);
var
  Len: LongWord;
begin
  Len := Length(Data);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    Stream.WriteBuffer(Data[0], Len);
end;

function ReadBytesFromStream(Stream: TStream; var Data: TByteDynArray): Boolean;
var
  n: LongWord;
begin
  Result := ((Stream.Size - Stream.Position) >= SizeOf(n));
  if Result then
  begin
    Stream.ReadBuffer(n, SizeOf(n));
    SetLength(Data, n);
    if (n > 0) then
      Stream.ReadBuffer(Data[0], n);
  end;
end;

function StrToBytes(const AValue: string): TByteDynArray;
var
  s: UTF8String;
  n: Integer;
begin
  {$ifdef UNICODE}
  s := UTF8Encode(AValue);
  {$else}
  s := AnsiToUtf8(AValue);
  {$endif}
  n := Length(s);
  SetLength(Result, n);
  if n > 0 then
    Move(s[1], Result[0], n);
end;

function BytesToStr(const AValue: TByteDynArray): string;
var
  s: UTF8String;
  n: Integer;
begin
  n := Length(AValue);
  SetLength(s, n);
  if n > 0 then
    Move(AValue[0], s[1], n);

  {$ifdef UNICODE}
  Result := UTF8ToString(s);
  {$else}
  Result := UTF8ToAnsi(s);
  {$endif}
end;


function XorShift32(var ASeed: LongWord): LongWord;
begin
  ASeed := ASeed xor (ASeed shl 13);
  ASeed := ASeed xor (ASeed shr 17);
  ASeed := ASeed xor (ASeed shl 5);
  Result := ASeed;
end;

function XorShift64(var ASeed: Int64): Int64;
begin
  ASeed := ASeed xor (ASeed shl 13);
  ASeed := ASeed xor (ASeed shr 7);
  ASeed := ASeed xor (ASeed shl 17);
  Result := ASeed;
end;

// закодировать
procedure Encode(var Data; DataLen, Code: Byte);
var
  pData: PByteArray;
  i: Integer;
begin
  pData := PByteArray(@Data);
  for i:=0 to DataLen-1 do
    pData^[i] := pData^[i] xor Code;
end;

// раскодировать
procedure Decode(var Data; DataLen, Code: Byte);
var
  pData: PByteArray;
  i: Integer;
begin
  pData := PByteArray(@Data);
  for i:=0 to DataLen-1 do
    pData^[i] := pData^[i] xor Code;
end;

function EncodeStr(const AStr: string; ACode: Integer): string;
begin
  // делаем полную копию строки, а не по ссылке, потому что Encode меняет
  // содержимое ссылки
  Result := Copy(AStr, 1, Length(AStr));
  if Length(Result) > 0 then
  begin
    Encode(Result[1], Length(Result), Byte(ACode));
  end;
end;

function DecodeStr(const AStr: string; ACode: Integer): string;
begin
  Result := Copy(AStr, 1, Length(AStr));
  if Length(Result) > 0 then
  begin
    Decode(Result[1], Length(Result), Byte(ACode));
  end;
end;

procedure XorData(var AData: TByteDynArray; ACode: Integer);
var
  n: Integer;
begin
  n := Length(AData);
  if n > 0 then
  begin
    Encode(AData[0], n, Byte(ACode));
  end;
end;

{$ifndef ANDROID}
function HexToByte(const HexValue: String): Byte;
var
  sHex: String;
  cDigit: Char;
begin
  Result := 0;
  sHex := UpperCase(HexValue);
  if Length(HexValue) > 0 then
  begin
    cDigit := sHex[1];
    if cDigit in ['0'..'9'] then
      Result := (Ord(cDigit) - 48) shl 4
    else
    if cDigit in ['A'..'F'] then
      Result := (Ord(cDigit) - 55) shl 4;
  end;

  if Length(HexValue) > 1 then
  begin
    cDigit := sHex[2];
    if cDigit in ['0'..'9'] then
      Result := Result + Ord(cDigit) - 48
    else
    if cDigit in ['A'..'F'] then
      Result := Result + Ord(cDigit) - 55;
  end;
end;
{$else}
function HexToByte(const HexValue: String): Byte;
var
  sHex: String;
  cDigit: Char;
begin
  Result := 0;
  sHex := UpperCase(HexValue);
  if Length(HexValue) > 0 then
  begin
    cDigit := sHex[1];
    if CharInSet(cDigit, ['0'..'9']) then
      Result := (Ord(cDigit) - 48) shl 4
    else
    if CharInSet(cDigit, ['A'..'F']) then
      Result := (Ord(cDigit) - 55) shl 4;
  end;

  if Length(HexValue) > 1 then
  begin
    cDigit := sHex[2];
    if CharInSet(cDigit, ['0'..'9']) then
      Result := Result + Ord(cDigit) - 48
    else
    if CharInSet(cDigit, ['A'..'F']) then
      Result := Result + Ord(cDigit) - 55;
  end;
end;
{$endif}

function HexToInt(const Value: String): Int64;
var
  i, m, c: Integer;
begin
  Result := 0;
  m := 0; // на сколько бит сдвигать
  i := Length(Value);
  while i > 0 do
  begin
    c := Ord(Value[i]);
    Dec(i);
    if (c >= $30) and (c <= $39) then
      Result := Result or (Int64(c - $30) shl m)
    else
    if (c >= $41) and (c <= $46) then
      Result := Result or (Int64((c - $41) + $0A) shl m)
    else
    if (c >= $61) and (c <= $66) then
      Result := Result or (Int64((c - $61) + $0A) shl m)
    else
      Continue;
    Inc(m, 4);
  end;
end;

function MACAddrToInt64(const AMacAddr: string): Int64;
var
  i, m, n, c: Integer;
begin
  //Result := HexToInt(GetPureHex(AMacAddr));
  Result := 0;
  m := 0; // на сколько бит сдвигать
  n := 0; // число полубайтов
  i := Length(AMacAddr);
  while (i > 0) and (n < 12) do
  begin
    c := Ord(AMacAddr[i]);
    Dec(i);
    if (c >= $30) and (c <= $39) then
      Result := Result or (Int64(c - $30) shl m)
    else
    if (c >= $41) and (c <= $46) then
      Result := Result or (Int64((c - $41) + $0A) shl m)
    else
    if (c >= $61) and (c <= $66) then
      Result := Result or (Int64((c - $61) + $0A) shl m)
    else
      Continue;

    Inc(m, 4);
    Inc(n);
  end;
end;

function Int64ToMACAddr(AInt64: Int64): string;
var
  i: Integer;
  bb: Byte;
begin
  Result := '';
  for i := 5 downto 0 do
  begin
    // shift desired byte to lower position and mask out others
    bb := (AInt64 shr (i*8)) and $FF;
    if Length(Result) > 0 then
      Result := Result + '-';
    Result := Result + IntToHex(bb, 2);
  end;
end;


{$IFNDEF FPC}
function GetTickCount(): Cardinal;
begin
  {$ifdef ANDROID}
  Result := TThread.GetTickCount();
  {$else}
  Result := Windows.GetTickCount();
  {$endif}
end;

// Аналог стандартной функции из Windows
function GetTickCount64Old(): Int64;
var
  tc: Cardinal;
begin
  {$ifdef ANDROID}
  tc := TThread.GetTickCount();
  {$else}
  tc := Windows.GetTickCount();
  {$endif}
  if tc < PrevTickCount then
  begin
    // проверка на ошбику таймера до 4 секунд
    if (PrevTickCount - tc) < $1000 then
      tc := PrevTickCount + 1
    else
      TickCount64Base := TickCount64Base + $0000000100000000;
  end;
  PrevTickCount := tc;
  Result := TickCount64Base or tc;
end;
{$ENDIF}

function TickDiff(TickNow: Int64; TickPrev: Int64): Integer;
var
  n: Int64;
begin
  n := (TickNow - TickPrev);
  if (n > High(Integer)) or (n < Low(Integer)) then
    Result := MaxInt
  else
    Result := Integer(n and $7FFFFFFF);
end;

function MinutesBetweenTicks(TickNow: Int64; TickPrev: Int64): Integer;
begin
  Result := TickDiff(TickNow, TickPrev) div 60000;
end;

function SecondsBetweenTicks(TickNow: Int64; TickPrev: Int64): Integer;
begin
  Result := TickDiff(TickNow, TickPrev) div 1000;
end;


function SecondsToTimeStr(ASec: Cardinal): string;
var
  dd, hh, nn, ss: Cardinal;
  s: string;
begin
  // дни
  dd := ASec div (60*60*24);
  ASec := ASec - Cardinal(dd * 60*60*24);
  // часы
  hh := ASec div (60*60);
  ASec := ASec - Cardinal(hh * 60*60);
  // минуты
  nn := ASec div 60;
  // секунды
  ss := ASec - Cardinal(nn * 60);
  if dd > 0 then
  begin
    if dd = 1 then
      Result := IntToStr(dd)+' сутки'
    else
      Result := IntToStr(dd)+' суток';
  end
  else
  begin
    Result := '';
    s := IntToStr(hh);
    if Length(s) = 1 then s := '0'+s;
    Result := Result + s;

    s := IntToStr(nn);
    if Length(s) = 1 then s := '0'+s;
    Result := Result + ':' + s;

    s := IntToStr(ss);
    if Length(s) = 1 then s := '0'+s;
    Result := Result + ':' + s;
  end;
end;

function SecondsToTimeStrShort(ASec: Cardinal): string;
var
  nn, ss: Cardinal;
  s: string;
begin
  // минуты
  nn := ASec div 60;
  // секунды
  ss := ASec - Cardinal(nn * 60);

  s := IntToStr(nn);
  if Length(s) = 1 then s := '0'+s;
  Result := s;

  s := IntToStr(ss);
  if Length(s) = 1 then s := '0'+s;
  Result := Result + ':' + s;
end;


function SecondsBetween(const ANow, AThen: TDateTime): Int64;
var
  Delta: Double;
begin
  if ANow < AThen then
    Delta := AThen - ANow
  else
    Delta := ANow - AThen;
  Result := Trunc(SecsPerDay * Delta);
end;

function DateOf(const AValue: TDateTime): TDateTime;
begin
  Result := Trunc(AValue);
end;

function ExtractFirstWord(var AStr: string; const WordSeparator: string = ' '): string;
var
  n: Integer;
begin
  n := Pos(WordSeparator, AStr);
  if n = 0 then n := MaxInt-1;
  Result := Copy(AStr, 1, n-1);
  AStr := Copy(AStr, n+1, MaxInt);
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

procedure AppendStrEx(var ADest: string; const AStr: string; ASeparator: string);
begin
  if ADest <> '' then
    ADest := ADest + ASeparator;
  ADest := ADest + AStr;
end;

function GetFilteredString(const AStr, AFilterStr: string): string;
var
  i, l, n: Integer;
begin
  l := Length(AStr);
  SetLength(Result, l);
  n := 0;
  for i := 1 to l do
  begin
    if Pos(AStr[i], AFilterStr) > 0 then
    begin
      Inc(n);
      Result[n] := AStr[i];
    end;
  end;
  SetLength(Result, n);
end;

function GetPureNumber(const ANumStr: String): String;
begin
  Result := GetFilteredString(ANumStr, '0123456789');
end;

function GetPureHex(const AStr: string): string;
var
  i, l, n: Integer;
begin
  l := Length(AStr);
  SetLength(Result, l);
  n := 0;
  for i := 1 to l do
  begin
    if Pos(AStr[i], '0123456789ABCDEFabcdef') > 0 then
    begin
      Inc(n);
      Result[n] := AStr[i];
    end;
  end;
  SetLength(Result, n);
  Result := UpperCase(Result);
end;

// Выдает HEX-строку содержимого буфера
function BufferToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '[';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
  Result := Result + ']';
end;

// Выдает HEX-строку содержимого буфера, заглавными буквами без пробелов
function BufToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
end;

// добавляет разделители тысяч
function FormatInt(ANum: Int64; ASep: string = ' '): string;
var
  s: string;
  i, n, ls, l2: Integer;
begin
  s := IntToStr(ANum);
  ls := Length(s);
  n := 1;
  if ls > 1 then
  begin
    if s[1] = '-' then
    begin
      n := 2;
      ls := ls - 1;
    end;
  end;

  if ls <= 3 then
  begin
    Result := s;
    Exit;
  end;

  l2 := (ls-1) div 3;
  Result := '';
  for i := 1 to l2 do
    Result := ASep + Copy(s, ls - (3 * i) + 1, 3) + Result;

  Result := Copy(s, n, (ls - 1) mod 3 + 1) + Result;
  if n > 1 then
    Result := '-' + Result;
end;


{$IFDEF FPC}
function GetExceptionMessage(): string;
var
  E: Exception;
  // Указатель на вершину списка исключений
  NextRaise: PExceptObject;
begin
  Result := '';
  NextRaise := RaiseList();
  while NextRaise <> nil do
  begin
    E := Exception(NextRaise^.FObject);
    if Result <> '' then
      Result := Result + '; ';
    if Assigned(E) then
      Result := Result + E.ClassName+'($' + IntToHex(PtrUInt(NextRaise^.Addr), 8) + '): ' + E.Message;
    NextRaise := NextRaise^.Next;
  end
end;
{$ELSE}
function GetExceptionMessage(): string;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = packed record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

var
  E: Exception;
  // Указатель на вершину списка исключений
  NextRaise: Pointer;

begin
  Result := '';
  {$WARNINGS OFF}
  {$ifdef ANDROID}
  E := Exception(AcquireExceptionObject());
  if Assigned(E) then
    Result := E.ClassName + '($' + IntToHex(Cardinal(ExceptAddr), 8) + '): ' + E.Message;
  {$else}
  NextRaise := RaiseList();
  while NextRaise <> nil do
  begin
    E := Exception(PRaiseFrame(NextRaise)^.ExceptObject);
    if Result <> '' then
      Result := Result + '; ' + sLineBreak;
    if Assigned(E) then
      Result := Result + E.ClassName+'($' + IntToHex(Cardinal(PRaiseFrame(NextRaise)^.ExceptAddr), 8) + '): ' + E.Message;
    NextRaise := PRaiseFrame(NextRaise)^.NextRaise;
  end
  {$endif}
  {$WARNINGS ON}

  {E := Exception(ExceptObject);
  if Assigned(E) then
  begin
    Result := E.ClassName+'($'+IntToHex(Cardinal(ExceptAddr), 8)+'): '+E.Message;
  end
  else
    Result := ''; }
end;
{$ENDIF}

{ Bit 0..15 }
function CheckBit(BitIndex: Integer; Value: Byte): Boolean;
begin
  Result := (Value and ($01 shl BitIndex)) > 0;
end;

function CheckBit(BitIndex: Integer; Value: Word): Boolean;
begin
  Result := (Value and ($01 shl BitIndex)) > 0;
end;

function CheckBit(BitIndex: Integer; Value: Cardinal): Boolean;
begin
  Result := (Value and ($01 shl BitIndex)) > 0;
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

function BitCount(n: Word): Integer;
begin
  Result := 0;
  while n <> 0 do
  begin
    Inc(Result);
    n := n and (n - 1);
  end;
end;

{$ifndef ANDROID}
// копирование файла
function CopyFile(const ASource, ADest: TFileName; AKeepExisting: Boolean): Boolean;
{$ifndef WIN_NATIVE}
var
  fs1, fs2: TFileStream;
{$endif}
begin
  {$ifdef WIN_NATIVE}
  Result := Windows.CopyFile(PChar(ASource), PChar(ADest), AKeepExisting);
  {$else}
  Result := False;
  if not FileExists(ASource) then Exit;

  if FileExists(ADest) then
    DeleteFile(ADest);

  fs1 := TFileStream.Create(ASource, fmOpenRead);
  try
    fs2 := TFileStream.Create(ADest, fmCreate);
    try
      fs2.CopyFrom(fs1, fs1.Size);
    finally
      fs2.Free();
    end;
  finally
    fs1.Free();
  end;
  {$endif}
end;

// Определение кол-ва файлов в определённой папке по маске
function GetFileCount(const Dir, Mask: string): Integer;
var
  SearchRec: TSearchRec;
  sf: string;
begin
  Result := 0;
  if FindFirst(Dir + Mask, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      inc(Result);
      sf := SearchRec.Name;
    until (FindNext(SearchRec) <> 0);
  end;
  FindClose(SearchRec);
end;

// сортировка списка файлов копий БД
function CustomFileSortProc(List: TStringList; Index1, Index2: Integer): Integer;
var
  Date1, Date2: TDateTime;
begin
  Date1 := FileDateToDateTime(StrToIntDef(List.ValueFromIndex[Index1], 0));
  Date2 := FileDateToDateTime(StrToIntDef(List.ValueFromIndex[Index2], 0));
  if Date2 > Date1 then
    Result := -1
  else
  if Date2 < Date1 then
    Result := 1
  else
    Result := 0;
end;

// Удалить первых N файлов в определённой папке по маске
procedure DeleteFileCount(const Dir, Mask: string; N: Integer);
var
  SearchRec: TSearchRec;
  i: Integer;
  BackupFileList: TStringList;
begin
  BackupFileList := TStringList.Create();
  try
    BackupFileList.NameValueSeparator := '*';
    if SysUtils.FindFirst(Dir + Mask, faAnyFile, SearchRec) = 0 then
    begin
      repeat
        BackupFileList.Add(SearchRec.Name + BackupFileList.NameValueSeparator + IntToStr(SearchRec.Time));
      until (SysUtils.FindNext(SearchRec) <> 0);
      SysUtils.FindClose(SearchRec);
    end;
    BackupFileList.CustomSort(CustomFileSortProc);
    for i := 0 to N - 1 do
      DeleteFile(Dir + BackupFileList.Names[i]);
  finally
    BackupFileList.Free();
  end;
end;
{$endif} // ANDROID

{$ifdef WIN_NATIVE}
// версия файла
type
  PVSFixedFileInfo = ^TVSFixedFileInfo;
  {$EXTERNALSYM tagVS_FIXEDFILEINFO}
  tagVS_FIXEDFILEINFO = packed record
    dwSignature: DWORD;        { e.g. $feef04bd }
    dwStrucVersion: DWORD;     { e.g. $00000042 = "0.42" }
    dwFileVersionMS: DWORD;    { e.g. $00030075 = "3.75" }
    dwFileVersionLS: DWORD;    { e.g. $00000031 = "0.31" }
    dwProductVersionMS: DWORD; { e.g. $00030010 = "3.10" }
    dwProductVersionLS: DWORD; { e.g. $00000031 = "0.31" }
    dwFileFlagsMask: DWORD;    { = $3F for version "0.42" }
    dwFileFlags: DWORD;        { e.g. VFF_DEBUG | VFF_PRERELEASE }
    dwFileOS: DWORD;           { e.g. VOS_DOS_WINDOWS16 }
    dwFileType: DWORD;         { e.g. VFT_DRIVER }
    dwFileSubtype: DWORD;      { e.g. VFT2_DRV_KEYBOARD }
    dwFileDateMS: DWORD;       { e.g. 0 }
    dwFileDateLS: DWORD;       { e.g. 0 }
  end;
  TVSFixedFileInfo = tagVS_FIXEDFILEINFO;
  {$EXTERNALSYM VS_FIXEDFILEINFO}
  VS_FIXEDFILEINFO = tagVS_FIXEDFILEINFO;

function GetFileVersion(const AFileName: string; var VersionMS, VersionLS: LongWord): LongWord;
var
  FileName: string;
  InfoSize, Wnd: LongWord;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := 0;
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);

  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          VersionMS := FI^.dwFileVersionMS;
          VersionLS := FI^.dwFileVersionLS;
          Result := FI^.dwFileVersionMS;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

// версия программы/DLL
function GetAppVersion: LongWord;
var
  ModuleFileName: array [0..MAX_PATH] of Char;
  VersionMS, VersionLS: LongWord;
begin
  GetModuleFileName(HInstance, ModuleFileName, MAX_PATH);
  GetFileVersion(ModuleFileName, VersionMS, VersionLS);

  Result := (Byte(HiWord(VersionMS)) shl 24)
         or (Byte(LoWord(VersionMS)) shl 16)
         or (Byte(HiWord(VersionLS)) shl 8)
         or (Byte(LoWord(VersionLS)));
end;
{$endif}

initialization

PrevTickCount := 0 ;
TickCount64Base := 0;
{$ifdef ANDROID}
CP1251Encoding := TMBCSEncoding.Create(1251);
{$endif}

//GetFormatSettings(GetThrea, RFFormatSettings);
RFFormatSettings.DecimalSeparator := '.';
RFFormatSettings.ThousandSeparator := '_';
RFFormatSettings.DateSeparator := '.';
RFFormatSettings.TimeSeparator := ':';
RFFormatSettings.ShortDateFormat := 'yyyy.mm.dd';
RFFormatSettings.ShortTimeFormat := 'hh:nn:ss';

{$ifndef FPC}
  {$ifdef MSWINDOWS}
GetTickCount64 := GetProcAddress(GetModuleHandle('Kernel32.dll'), 'GetTickCount64'); // Windows Vista+
if @GetTickCount64 = nil then // not supported
  {$endif}
  GetTickCount64 := @GetTickCount64Old;
{$endif}

end.


