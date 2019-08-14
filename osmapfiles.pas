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
  OsMap files handling routines
util\FileScanner (TFileScanner)
util\FileWriter  (TFileWriter...)
*)
unit OsMapFiles;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, OsMapTypes, OsMapGeometry, OsMapUtils;

type
  TFileScannerMode = (fsmSequential, fsmFastRandom, fsmLowMemRandom, fsmNormal);

  { FileScanner implements platform independent reading data from files. }

  { TFileScanner }

  TFileScanner = class(TObject)
  private
    FFilename: string;  // Filename
    FFile: TStream;     // Internal low level file handle
    FHasError: Boolean; // Flag to signal errors in the stream

    {$ifdef HAVE_MMAP}
    // For mmap usage
    FBuffer: PByte;       // Pointer to the file memory
    FSize: TFileOffset;   // Size of the memory/file
    FOffset: TFileOffset; // Current offset into the file memory
    {$endif}

    {$ifdef HAVE_MMAP_WIN}
    // For Windows mmap usage
    FmmfHandle: THandle;
    {$endif}

    // For GeoCoordList loading
    FByteBuffer: array of Byte; // Temporary buffer for loading of std::vector<GeoCoord>
    FByteBufferSize: Integer;   // Size of the temporary byte buffer

    procedure AssureByteBufferSize(ASize: Integer);
    procedure FreeBuffer();

    { Reads bytes to internal temporary buffer
      or just return pointer to memory mapped file.
      Returns pointer to byteBuffer or memory mapped file.

      In case of internal buffer, data are valid until
      next read. In case of memory mapped file, data
      are valid until closing the reader and method don't
      copy the memory - it should be fast. }
    //function ReadInternal(ABytes: Integer): PByte;

    function GetIsOpen: Boolean;
    function GetIsEOF(): Boolean;

    function GetPos(): TFileOffset;
    procedure SetPos(AValue: TFileOffset);

    function ReadEncodedSignedNumber(): Int64;
    function ReadEncodedUnsignedNumber(): UInt64;

  public
    constructor Create();
    destructor Destroy; override;

    procedure Open(const AFilename: string;
                   AMode: TFileScannerMode;
                   AUseMmap: Boolean);

    { Closes the file.
      If the file was never opened or was already closed an exception is thrown.
      If closing the file fails, an exception is thrown }
    procedure Close();
    { Closes the file. Does not throw any exceptions even if an error occurs.
      Use this variant of Close() in cases where the file already run into errors and you just
      want to clean up the resources using best effort. }
    procedure CloseFailsafe();

    procedure Read(var ABuffer; ABytesCount: Integer); overload;
    { Read 0-terminated string }
    procedure Read(out AValue: string); overload;
    procedure Read(out AValue: Boolean); overload;
    procedure Read(out AValue: Byte); overload;
    procedure Read(out AValue: SmallInt); overload;
    procedure Read(out AValue: LongInt); overload;
    procedure Read(out AValue: Int64); overload;
    procedure Read(out AValue: UInt64); overload;
    procedure Read(out AValue: Word); overload;
    procedure Read(out AValue: LongWord); overload;

    procedure Read(out AValue: TObjectFileRef); overload;

    procedure ReadNumber(out AValue: Byte); overload;
    procedure ReadNumber(out AValue: SmallInt); overload;
    procedure ReadNumber(out AValue: Word); overload;
    procedure ReadNumber(out AValue: LongInt); overload;
    procedure ReadNumber(out AValue: LongWord); overload;
    procedure ReadNumber(out AValue: Int64); overload;

    // See: TGeoCoord.ReadFromStream()
    //procedure ReadCoord(out ACoord: TGeoCoord);
    //procedure ReadConditionalCoord(out ACoord: TGeoCoord; out AIsSet: Boolean);


    { Reads vector of Point and pre-compute segments and bounding box for it }
    function ReadMapPoints(var ANodes: TGeoPointArray;
      var ASegments: TSegmentGeoBoxArray;
      var BBox: TGeoBox;
      AIsReadIds: Boolean): Boolean;

    // See: TGeoBox.ReadFromStream()
    //procedure ReadBox(out AValue: TGeoBox);

    { Read TypeId value. AMaxBytes: 1..2 }
    procedure ReadTypeId(out AValue: TTypeId; AMaxBytes: Byte);

    { Set length of AValue before calling!  }
    procedure ReadObjectFileRefs(var AValue: array of TObjectFileRef);

    property Stream: TStream read FFile;
    property IsOpen: Boolean read GetIsOpen;
    property Filename: string read FFilename;
    //property Position: TFileOffset read GetPos write SetPos;
  end;

  { Read back a stream of sorted ObjectFileRefs as written by the ObjectFileRefStreamWriter. }
  TObjectFileRefStreamReader = object
  private
    FReader: TFileScanner;
    FLastFileOffset: TFileOffset;
  public
    procedure Init(AReader: TFileScanner; AOffset: TFileOffset);
    procedure Reset();
    procedure Read(var AValue: TObjectFileRef);
  end;

  { TFileWriter }

  TFileWriter = class(TObject)
  private
    FFilename: string;  // Filename
    FFile: TStream;     // Internal low level file handle
    FHasError: Boolean; // Flag to signal errors in the stream
    function GetIsOpen: Boolean;

    procedure WriteEncodedSignedNumber(AValue: Int64);
    procedure WriteEncodedUnsignedNumber(AValue: UInt64);
  public
    procedure Open(const AFilename: string);
    procedure Close();


    procedure Write(AValue: Byte); overload;
    procedure Write(AValue: Word); overload;
    procedure Write(AValue: LongWord); overload;
    procedure Write(AValue: SmallInt); overload;
    procedure Write(AValue: LongInt); overload;
    procedure Write(AValue: Int64); overload;
    procedure Write(AValue: UInt64); overload;
    procedure Write(const AValue: string); overload;
    procedure Write(AValue: Boolean); overload;
    //procedure Write(AValue: Byte); overload;
    procedure Write(const AValue: TObjectFileRef); overload;

    procedure WriteNumber(AValue: Word); overload;
    procedure WriteNumber(AValue: LongWord); overload;
    procedure WriteNumber(AValue: SmallInt); overload;
    procedure WriteNumber(AValue: LongInt); overload;
    procedure WriteNumber(AValue: Int64); overload;

    { Write TypeId value. AMaxBytes: 1..2 }
    procedure WriteTypeId(ATypeId: TTypeId; AMaxBytes: Byte);

    function WriteMapPoints(const ANodes: TGeoPointArray; AIsWriteIds: Boolean): Boolean;

    property Stream: TStream read FFile;
    property IsOpen: Boolean read GetIsOpen;
    property Filename: string read FFilename;
    //property Pos: TFileOffset read GetPos write SetPos;
  end;

implementation

uses Math; // eliminate "end of source not found"

const
  MAX_NODES = $03FFFFFF; // 26 bits

{ TFileWriter }

function TFileWriter.GetIsOpen: Boolean;
begin
  Result := Assigned(FFile);
end;

procedure TFileWriter.WriteEncodedSignedNumber(AValue: Int64);
var
  Buf: array[0..9] of Byte;
  BufSize: Integer;
begin
  BufSize := EncodeNumber(AValue, Buf);
  FFile.Write(Buf, BufSize);
end;

procedure TFileWriter.WriteEncodedUnsignedNumber(AValue: UInt64);
var
  Buf: array[0..9] of Byte;
  BufSize: Integer;
begin
  BufSize := EncodeNumber(AValue, Buf);
  FFile.Write(Buf, BufSize);
end;

procedure TFileWriter.Open(const AFilename: string);
begin
  if not Assigned(FFile) then
  begin
    if not FileExists(AFilename) then
      FFile := TFileStream.Create(AFilename, fmCreate)
    else
      FFile := TFileStream.Create(AFilename, fmOpenReadWrite);

    if Assigned(FFile) then
      FFilename := AFilename;
  end;
end;

procedure TFileWriter.Close();
begin
  if Assigned(FFile) then
    FreeAndNil(FFile);
end;

procedure TFileWriter.Write(AValue: Byte);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: Word);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: LongWord);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: SmallInt);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: LongInt);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: Int64);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(AValue: UInt64);
begin
  Assert(Assigned(FFile));
  FFile.Write(AValue, SizeOf(AValue));
end;

procedure TFileWriter.Write(const AValue: string);
var
  n: Integer;
begin
  Assert(Assigned(FFile));
  n := Length(AValue)+1;
  FFile.Write(PChar(AValue)^, n);
end;

procedure TFileWriter.Write(AValue: Boolean);
var
  TmpByte: Byte;
begin
  if AValue then
    TmpByte := 1
  else
    TmpByte := 0;
  //FFile.WriteByte(TmpByte)
  FFile.Write(TmpByte, 1);
end;

procedure TFileWriter.Write(const AValue: TObjectFileRef);
begin
  Write(Byte(Ord(AValue.RefType)));
  Write(AValue.Offset);
end;

procedure TFileWriter.WriteNumber(AValue: Word);
begin
  WriteEncodedUnsignedNumber(UInt64(AValue));
end;

procedure TFileWriter.WriteNumber(AValue: LongWord);
begin
  WriteEncodedUnsignedNumber(UInt64(AValue));
end;

procedure TFileWriter.WriteNumber(AValue: SmallInt);
begin
  WriteEncodedSignedNumber(Int64(AValue));
end;

procedure TFileWriter.WriteNumber(AValue: LongInt);
begin
  WriteEncodedSignedNumber(Int64(AValue));
end;

procedure TFileWriter.WriteNumber(AValue: Int64);
begin
  WriteEncodedSignedNumber(AValue);
end;

procedure TFileWriter.WriteTypeId(ATypeId: TTypeId; AMaxBytes: Byte);
begin
  if AMaxBytes = 1 then
    Write(Byte(ATypeId))
  else if AMaxBytes = 2 then
    Write(ATypeId)
  else
  begin
    Assert(False);
  end;
end;

function TFileWriter.WriteMapPoints(const ANodes: TGeoPointArray;
  AIsWriteIds: Boolean): Boolean;
var
  NodesCount: Integer;
  DeltaBuffer: array of LongInt;
  DeltaBufferPos, CoordBitSize, BytesNeeded: Integer;
  LastLat, LastLon, CurrentLat, CurrentLon: LongWord;
  i: Integer;
  LatDelta, LonDelta: LongInt;
  HasNodes: Boolean;
  CoordSizeFlags, HasNodesFlags: Byte;
  BitsetByte, BitMaskByte: Byte;
  SizeBytes: array [0..3] of Byte;
  ByteBuffer: array of Byte;
  ByteBufferPos, IdCurrent, IdEnd: Integer;
begin
  Result := False;
   // Quick exit for empty vector arrays
  if Length(ANodes) = 0 then
  begin
    Write(Byte(0));
    Result := True;
    Exit;
  end;

  NodesCount := Length(ANodes);
  Assert(NodesCount <= MAX_NODES);

  // A lat and a lon delta for each coordinate delta
  SetLength(DeltaBuffer, (NodesCount-1) * 2);

  // Calculate deltas and required space
  LastLat := LongWord(Round((ANodes[0].Lat +  90.0) * GlobalLatConversionFactor));
  LastLon := LongWord(Round((ANodes[0].Lon + 180.0) * GlobalLonConversionFactor));
  DeltaBufferPos := 0;
  CoordBitSize := 16;

  for i := 1 to NodesCount-1 do
  begin
    CurrentLat := LongWord(Round((ANodes[i].Lat +  90.0) * GlobalLatConversionFactor));
    CurrentLon := LongWord(Round((ANodes[i].Lon + 180.0) * GlobalLonConversionFactor));

    // === Lat
    LatDelta := CurrentLat - LastLat;
    if (LatDelta >= -128) and (LatDelta <= 127) then
      CoordBitSize := max(CoordBitSize, 16) // 2x 8 bit
    else if (LatDelta >= -32768) and (LatDelta <= 32767) then
      CoordBitSize := max(CoordBitSize, 32) // 2* 16 bit
    else if (LatDelta >= -8388608) and (LatDelta <= 8388608) then
      CoordBitSize := max(CoordBitSize, 48) // 2 * 24 bit
    else
      raise EInOutError.Create('Cannot write coordinate - Delta between Lat coordinates too big');

    DeltaBuffer[DeltaBufferPos] := LatDelta;
    Inc(DeltaBufferPos);

    // === Lon
    LonDelta := CurrentLon - LastLon;

    if (LonDelta >= -128) and (LonDelta <= 127) then
      CoordBitSize := max(CoordBitSize, 16) // 2x 8 bit
    else if (LonDelta >= -32768) and (LonDelta <= 32767) then
      CoordBitSize := max(CoordBitSize, 32) // 2* 16 bit
    else if (LonDelta >= -8388608) and (LonDelta <= 8388608) then
      CoordBitSize := max(CoordBitSize, 48) // 2 * 24 bit
    else
      raise EInOutError.Create('Cannot write coordinate - Delta between Lon coordinates too big');

    DeltaBuffer[DeltaBufferPos] := LonDelta;
    Inc(DeltaBufferPos);

    LastLat := CurrentLat;
    LastLon := CurrentLon;
  end;

  BytesNeeded := (NodesCount-1) * CoordBitSize div 8; // all coordinates in the same encoding

  // do we need to store node ids?
  HasNodes := False;
  {if AIsWriteIds then
  begin
    for i := 0 to NodesCount-1 do
    begin
      if ANodes[i].IsRelevant() then
      begin
        HasNodes := True;
        Break;
      end;
    end;
  end;  }

  // Write starting length / signal bit section
  // We use the first two bits to signal encoding size for coordinates
  if (CoordBitSize = 16) then
    CoordSizeFlags := $00
  else if (CoordBitSize = 32) then
    CoordSizeFlags := $01
  else
    CoordSizeFlags := $02;

  if AIsWriteIds then
  begin
    if HasNodes then
      HasNodesFlags := $04
    else
      HasNodesFlags := $00;

    if (NodesCount <= $0F) then // 2^4 (8 -3 flags -1 continuation bit)
    begin
      SizeBytes[0] := (NodesCount and $0F) shl 3;
      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags or HasNodesFlags;
      Stream.Write(SizeBytes[0], 1);
    end
    else if (NodesCount <= $7FF) then // 2^(4+7)=2^11 (16-3-1-1)
    begin
      //uint8_t size[2];
      SizeBytes[0] := ((NodesCount and $0F) shl 3) or $80; // The initial 4 bits + continuation bit
      SizeBytes[1] := NodesCount shr 4;                    // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags or HasNodesFlags;
      Stream.Write(SizeBytes[0], 2);
    end
    else if (NodesCount <= $3FFFF) then // 2^(11+7)=2^18 (24-3-1-1-1)
    begin
      SizeBytes[0] := ((NodesCount and $0F) shl 3) or $80; // The initial 4 bits + continuation bit
      SizeBytes[1] := ((NodesCount shr 4) and $7F) or $80; // Further 7 bits + continuation bit
      SizeBytes[2] := NodesCount shr 11;                   // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags or HasNodesFlags;
      Stream.Write(SizeBytes[0], 3);
    end
    else // 2^(18+8)=2^26 (32-3-1-1-1)
    begin
      SizeBytes[0] := ((NodesCount and $0F) shl 3) or $80;  // The initial 4 bits + continuation bit
      SizeBytes[1] := ((NodesCount shr 4) and $7F) or $80;  // Further 7 bits + continuation bit
      SizeBytes[2] := ((NodesCount shr 11) and $7F) or $80; // further 7 bits + continuation bit
      SizeBytes[3] := NodesCount shr 18;                    // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags or HasNodesFlags;
      Stream.Write(SizeBytes[0], 4);
    end;
  end
  else
  begin
    if (NodesCount <= $1F) then    // 2^5 (8 bits - 2 flag bit -1 continuation bit)
    begin
      SizeBytes[0] := (NodesCount and $1F) shl 2;
      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags;
      Stream.Write(SizeBytes[0], 1);
    end
    else if (NodesCount <= $FFF) then // 2^(5+7)=2^12 (16-2-1-1)
    begin
      SizeBytes[0] := ((NodesCount and $1F) shl 2) or $80; // The initial 5 bits + continuation bit
      SizeBytes[1] := NodesCount shr 5; // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags;
      Stream.Write(SizeBytes[0], 2);
    end
    else if (NodesCount <= $7FFFF) then // 2^(12+7)=2^19 (24-2-1-1-1)
    begin
      SizeBytes[0] := ((NodesCount and $1F) shl 2) or $80; // The initial 5 bits + continuation bit
      SizeBytes[1] := ((NodesCount shr 5) and $7F) or $80; // Further 7 bits + continuation bit
      SizeBytes[2] := NodesCount shr 12; // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags;
      Stream.Write(SizeBytes[0], 3);
    end
    else // 2^(19+8)2^27 (32-2-1-1-1)
    begin
      SizeBytes[0] := ((NodesCount and $1F) shl 2) or $80; // The initial 5 bits + continuation bit
      SizeBytes[1] := ((NodesCount shr 5) and $7F) or $80; // Further 7 bits + continuation bit
      SizeBytes[2] := ((NodesCount shr 12) and $7F) or $80; // further 7 bits + continuation bit
      SizeBytes[3] := NodesCount shr 19; // The final bits

      SizeBytes[0] := SizeBytes[0] or CoordSizeFlags;
      Stream.Write(SizeBytes[0], 4);
    end;
  end;

  // Write data array
  ANodes[0].WriteToStream(Stream);

  SetLength(ByteBuffer, BytesNeeded);

  if (CoordBitSize = 16) then
  begin
    ByteBufferPos := 0;
    for i := 0 to Length(DeltaBuffer)-1 do
    begin
      ByteBuffer[ByteBufferPos] := DeltaBuffer[i];
      Inc(ByteBufferPos);
    end;
  end
  else if (CoordBitSize = 32) then
  begin
    ByteBufferPos := 0;
    for i := 0 to Length(DeltaBuffer)-1 do
    begin
      ByteBuffer[ByteBufferPos] := DeltaBuffer[i] and $FF;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := (DeltaBuffer[i] shr 8);
      Inc(ByteBufferPos);
    end;
  end
  else
  begin
    ByteBufferPos := 0;
    i := 0;
    while i < Length(DeltaBuffer)-1 do
    begin
      ByteBuffer[ByteBufferPos] := DeltaBuffer[i] and $FF;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := (DeltaBuffer[i] shr 8) and $FF;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := DeltaBuffer[i] shr 16;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := DeltaBuffer[i+1] and $FF;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := (DeltaBuffer[i+1] shr 8) and $FF;
      Inc(ByteBufferPos);

      ByteBuffer[ByteBufferPos] := DeltaBuffer[i+1] shr 16;
      Inc(ByteBufferPos);

      Inc(i, 2);
    end;
  end;

  Stream.Write(ByteBuffer[0], BytesNeeded);

  if (HasNodes) then
  begin
    IdCurrent := 0;

    while (IdCurrent < NodesCount) do
    begin
      BitsetByte := 0;
      BitMaskByte := 1;
      IdEnd := min(IdCurrent + 8, NodesCount);

      for i := IdCurrent to IdEnd-1 do
      begin
        //if (ANodes[i].IsRelevant()) then
        //  BitsetByte := BitsetByte or BitMaskByte;

        BitMaskByte := BitMaskByte * 2;
      end;

      Stream.Write(BitsetByte, 1);

      for i := idCurrent to idEnd-1 do
      begin
        //if (ANodes[i].IsRelevant()) then
        //  Stream.Write(ANodes[i].Serial, 1);

        //BitMaskByte := BitMaskByte * 2;
      end;

      Inc(IdCurrent, 8);
    end

    (*
    SetLength(ByteBuffer, NodesCount);
    for i := 0 to NodesCount-1 do
      ByteBuffer[i] := ANodes[i].Serial;

    Stream.Write(ByteBuffer[0], NodesCount);
    *)
  end;

end;

{ TFileScanner }

procedure TFileScanner.AssureByteBufferSize(ASize: Integer);
begin
  if FByteBufferSize >= ASize then
    Exit;

  SetLength(FByteBuffer, ASize);
  FByteBufferSize := ASize;
end;

procedure TFileScanner.FreeBuffer();
begin
{$ifdef HAVE_MMAP}
  if Assigned(FBuffer) then
  begin
    if (munmap(ABuffer, ASize) <> 0) then
    begin
      LogError('Error while calling munmap: ' + strerror(errno) + ' for file: ' + filename;
    end;

    ABuffer := nil;
  end;
{$endif}
{$ifdef HAVE_MMAP_WIN}
  if Assigned(FBuffer) then
  begin
    UnmapViewOfFile(FBuffer);
    FBuffer := nil;

    if (mmfHandle <> feInvalidHandle) then
    begin
      CloseHandle(mmfHandle);
      mmfHandle := feInvalidHandle;
    end;
  end;
{$endif}
end;

function TFileScanner.GetIsOpen: Boolean;
begin
  Result := Assigned(FFile);
end;

function TFileScanner.GetIsEOF(): Boolean;
begin
  Assert(Assigned(FFile));
  Result := (FFile.Position = FFile.Size);
end;

function TFileScanner.GetPos(): TFileOffset;
begin
  Assert(Assigned(FFile));
  Result := FFile.Position;
end;

procedure TFileScanner.SetPos(AValue: TFileOffset);
begin
  Assert(Assigned(FFile));
  if AValue <= FFile.Size then
    FFile.Position := AValue;
end;

{function TFileScanner.ReadInternal(ABytes: Integer): PByte;
begin

end; }

procedure TFileScanner.Open(const AFilename: string; AMode: TFileScannerMode;
  AUseMmap: Boolean);
begin
  if Assigned(FFile) then
    raise EInOutError.Create('File already opened: ' + FFilename);

  FHasError := True;
  FFilename := AFilename;

  //file=fopen(filename.c_str(),"rb");
  FFile := TFileStream.Create(AFileName, fmOpenRead);

  if not Assigned(FFile) then
    raise EInOutError.Create('Cannot open file for reading: '+ FFilename);

  {$ifdef HAVE_MMAP}
  FSize := FFile.Size;
  FOffset := 0;

  if AUseMmap and (FSize > 0) then
  begin
    FreeBuffer();

    FBuffer := mmap(nullptr, FSize, PROT_READ, MAP_PRIVATE, fileno(file), 0);
    if (FBuffer <> MAP_FAILED) then
    begin
      FOffset := 0;
    end
    else
    begin
      log.Error() << "Cannot mmap file '" << filename << "' of size " << size << " (" << strerror(errno) << ")";
      FBuffer := nil;
    end;
  end;
{$endif}
{$ifdef HAVE_MMAP_WIN}
  unused(mode);
  if AUseMmap and (FSize > 0) then
  begin
    FreeBuffer();

    mmfHandle := CreateFileMapping((HANDLE)_get_osfhandle(fileno(file)),
                                (LPSECURITY_ATTRIBUTES)nullptr,
                                PAGE_READONLY,
                                0,0,
                                (LPCTSTR)nullptr);

    if (mmfHandle <> nullptr) then
    begin
      FBuffer := MapViewOfFile(mmfHandle,
                                  FILE_MAP_READ,
                                  0,
                                  0,
                                  0);

      if Assigned(FBuffer) then
        FOffset := 0
      else
        log.Error() << "Cannot map view for file '" << filename << "' of size " << size << " (" << GetLastError() << ")";
    end
    else
    begin
      log.Error() << "Cannot create file mapping for file '" << filename << "' of size " << size << " (" << GetLastError() << ")";
    end;
  end;
{$endif}

  FHasError := False;
end;

procedure TFileScanner.Close();
begin
  if not Assigned(FFile) then
    raise EInOutError.Create('File already closed');

  CloseFailsafe();
end;

procedure TFileScanner.CloseFailsafe();
begin
  FreeBuffer();
  if Assigned(FFile) then
    FreeAndNil(FFile);
end;

function TFileScanner.ReadEncodedSignedNumber(): Int64;
var
  TmpBuf: array [0..9] of Byte;
  TmpPos: Int64;
  n: Integer;
begin
  TmpPos := FFile.Position;
  Result := 0;
  TmpBuf[0] := 0;
  n := FFile.Read(TmpBuf, SizeOf(TmpBuf));
  if n = 0 then
    Exit;

  n := DecodeNumberSigned(TmpBuf, Result);
  FFile.Position := TmpPos + n;
end;

function TFileScanner.ReadEncodedUnsignedNumber(): UInt64;
var
  TmpBuf: array [0..9] of Byte;
  TmpPos: Int64;
  n: Integer;
begin
  TmpPos := FFile.Position;
  Result := 0;
  TmpBuf[0] := 0;
  n := FFile.Read(TmpBuf, SizeOf(TmpBuf));
  if n = 0 then
    Exit;

  n := DecodeNumber(TmpBuf, Result);
  FFile.Position := TmpPos + n;
end;

constructor TFileScanner.Create();
begin
  inherited Create();
  FHasError := False;
  {$ifdef HAVE_MMAP}
  FBuffer := nil;
  FSize := 0;
  FOffset := 0;
  {$endif}
  SetLength(FByteBuffer, 0);
  FByteBufferSize := 0;
  {$ifdef HAVE_MMAP_WIN}
  FmmfHandle := feInvalidHandle;
  {$endif}
end;

destructor TFileScanner.Destroy;
begin
  if IsOpen then
  begin
    //LogWarning('Automatically closing FileScanner for file: ' + FFilename);
    CloseFailsafe();
  end;
  SetLength(FByteBuffer, 0);
  inherited Destroy;
end;

procedure TFileScanner.Read(var ABuffer; ABytesCount: Integer);
begin
  Assert(Assigned(FFile));
  if Assigned(FFile) then
  begin
    FFile.Read(ABuffer, ABytesCount);
    //FOffset := FFile.Pos;
  end;
end;

procedure TFileScanner.Read(out AValue: string);
var
  TmpPos: Int64;
  TmpBuffer: TBytes;  // AnsiString
  nSize, nRead, n, i: Integer;
  IsFinished: Boolean;
begin
  Assert(Assigned(FFile));
  AValue := '';
  IsFinished := False;
  if Assigned(FFile) then
  begin
    TmpPos := FFile.Position;
    nSize := 64;
    SetLength(TmpBuffer, nSize);
    while not IsFinished do
    begin
      nRead := FFile.Read(TmpBuffer[1], nSize);
      //n := Pos(#0, TmpBuffer)-1;
      n := -1;
      for i := 0 to nSize-1 do
      begin
        if TmpBuffer[i] = 0 then
        begin
          n := i;
          Break;
        end;
      end;

      IsFinished := (nRead < nSize) or (n >= 0) or (n > nRead);
      if (n > nRead) then
        n := nRead
      else if (n < 0) then
        n := nRead;

      if n > 0 then
      begin
        //AValue := AValue + Copy(TmpBuffer, 1, n);
        for i := 1 to n do
          AValue := AValue + Chr(TmpBuffer[i]);
      end
    end;

    TmpPos := TmpPos + Length(AValue) + 1;
    if TmpPos <= FFile.Size then
      FFile.Position := TmpPos
    else
      FFile.Position := FFile.Size;
    //FOffset := FFile.Position;
  end;
end;

procedure TFileScanner.Read(out AValue: Boolean);
var
  TmpByte: Byte;
begin
  Assert(Assigned(FFile));
  //TmpByte := FFile.ReadByte();
  FFile.Read(TmpByte, 1);
  AValue := (TmpByte <> 0);
end;

procedure TFileScanner.Read(out AValue: Byte);
begin
  Assert(Assigned(FFile));
  //AValue := FFile.ReadByte();
  FFile.Read(AValue, 1);
end;

procedure TFileScanner.Read(out AValue: SmallInt);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: LongInt);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: Int64);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: UInt64);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: Word);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: LongWord);
begin
  Assert(Assigned(FFile));
  AValue := 0;
  FFile.Read(AValue, SizeOf(AValue));
end;

procedure TFileScanner.Read(out AValue: TObjectFileRef);
var
  ValTypeByte: Byte;
  ValOffset: TFileOffset;
begin
  Read(ValTypeByte);
  Read(ValOffset);
  if ValTypeByte <= Ord(High(TRefType)) then
    AValue.RefType := TRefType(ValTypeByte);
  AValue.Offset := ValOffset;
end;

procedure TFileScanner.ReadNumber(out AValue: Byte);
begin
  AValue := Byte(ReadEncodedUnsignedNumber());
end;

procedure TFileScanner.ReadNumber(out AValue: SmallInt);
begin
  AValue := SmallInt(ReadEncodedSignedNumber());
end;

procedure TFileScanner.ReadNumber(out AValue: Word);
begin
  AValue := Word(ReadEncodedUnsignedNumber());
end;

procedure TFileScanner.ReadNumber(out AValue: LongInt);
begin
  AValue := LongInt(ReadEncodedSignedNumber());
end;

procedure TFileScanner.ReadNumber(out AValue: LongWord);
begin
  AValue := LongWord(ReadEncodedUnsignedNumber());
end;

procedure TFileScanner.ReadNumber(out AValue: Int64);
begin
  AValue := ReadEncodedSignedNumber();
end;

function TFileScanner.ReadMapPoints(var ANodes: TGeoPointArray;
  var ASegments: TSegmentGeoBoxArray;
  var BBox: TGeoBox;
  AIsReadIds: Boolean): Boolean;
var
  CoordBitSize, NodeCount, ShiftCount, SegmentCount: Integer;
  SizeByte, BitsetByte, TmpByte: Byte;
  HasNodes: Boolean;
  ByteBufferSize: Integer;
  FirstCoord: TGeoPoint;
  LatValue, LonValue: LongWord;
  LatDelta, LonDelta: LongInt;
  LatUDelta, LonUDelta: LongWord;
  TmpBuffer: array of Byte;
  CurrentCoordPos, IdCurrent: Integer;
  i, n: Integer;
  pSegment: ^TSegmentGeoBox;
  Bitmask: LongWord;
begin
  Result := False;
  //SizeByte := Stream.ReadByte();
  Read(SizeByte);

  if SizeByte = 0 then
    Exit;

  if (SizeByte and $03) = 0 then
    CoordBitSize := 16
  else if (SizeByte and $03) = 1 then
    CoordBitSize := 32
  else
    CoordBitSize := 48;

  if AIsReadIds then
  begin
    HasNodes := ((SizeByte and $04) <> 0);
    NodeCount := (SizeByte and $78) shr 3;
    ShiftCount := 4;
  end
  else
  begin
    HasNodes := False;
    NodeCount := (SizeByte and $7C) shr 2;
    ShiftCount := 5;
  end;
  // read rest of NodeCount
  while ((SizeByte and $80) <> 0) and (ShiftCount < 20) do
  begin
    Read(SizeByte);
    NodeCount := NodeCount or ((SizeByte and $7F) shl ShiftCount);
    Inc(ShiftCount, 7);
  end;

  SetLength(ANodes, NodeCount);

  if NodeCount = 0 then
    Exit;

  ByteBufferSize := (NodeCount - 1) * CoordBitSize div 8;
  if ByteBufferSize = 0 then
    Exit;

  if not FirstCoord.ReadFromStream(Stream) then
    Exit;

  LatValue := Round((FirstCoord.Lat +  90.0) * GlobalLatConversionFactor);
  LonValue := Round((FirstCoord.Lon + 180.0) * GlobalLonConversionFactor);

  ANodes[0] := FirstCoord;

  SetLength(TmpBuffer, ByteBufferSize);

  Stream.ReadBuffer(TmpBuffer[0], ByteBufferSize);

  if CoordBitSize = 16 then
  begin
    CurrentCoordPos := 1;
    n := 0;
    while n < (ByteBufferSize-1) do
    begin
      LatDelta := TmpBuffer[n+0];
      LonDelta := TmpBuffer[n+1];

      Inc(LatValue, LatDelta);
      Inc(LonValue, LonDelta);

      ANodes[CurrentCoordPos].Init(LatValue / GlobalLatConversionFactor - 90.0,
                                   LonValue / GlobalLonConversionFactor - 180.0);
      Inc(CurrentCoordPos);
      Inc(n, 2);
    end;
  end
  else if CoordBitSize = 32 then
  begin
    CurrentCoordPos := 1;
    n := 0;
    while n < (ByteBufferSize-1) do
    begin
      LatUDelta := TmpBuffer[n+0] or (TmpBuffer[n+1] shl 8);
      LonUDelta := TmpBuffer[n+2] or (TmpBuffer[n+3] shl 8);

      if (LatUDelta and $8000) <> 0 then
        LatDelta := LongInt(LatUDelta or $FFFF0000)
      else
        LatDelta := LongInt(LatUDelta);
      Inc(LatValue, LatDelta);

      if (LonUDelta and $8000) <> 0 then
        LonDelta := LongInt(LonUDelta or $FFFF0000)
      else
        LonDelta := LongInt(LonUDelta);
      Inc(LonValue, LonDelta);

      ANodes[CurrentCoordPos].Init(LatValue / GlobalLatConversionFactor - 90.0,
                                   LonValue / GlobalLonConversionFactor - 180.0);
      Inc(CurrentCoordPos);
      Inc(n, 4);
    end;
  end
  else if CoordBitSize = 48 then
  begin
    CurrentCoordPos := 1;
    n := 0;
    while n < (ByteBufferSize-1) do
    begin
      LatUDelta := TmpBuffer[n+0] or (TmpBuffer[n+1] shl 8) or (TmpBuffer[n+2] shl 16);
      LonUDelta := TmpBuffer[n+3] or (TmpBuffer[n+4] shl 8) or (TmpBuffer[n+5] shl 16);

      if (LatUDelta and $800000) <> 0 then
        LatDelta := LongInt(LatUDelta or $FF000000)
      else
        LatDelta := LongInt(LatUDelta);
      Inc(LatValue, LatDelta);

      if (LonUDelta and $800000) <> 0 then
        LonDelta := LongInt(LonUDelta or $FF000000)
      else
        LonDelta := LongInt(LonUDelta);
      Inc(LonValue, LonDelta);

      ANodes[CurrentCoordPos].Init(LatValue / GlobalLatConversionFactor - 90.0,
                                   LonValue / GlobalLonConversionFactor - 180.0);
      Inc(CurrentCoordPos);
      Inc(n, 6);
    end;
  end;

  BBox.InitForPoints(ANodes);

  // we will prepare segment bounding boxes just for long point vectors
  if NodeCount > 1024 then
  begin
    // initialise segments
    SegmentCount := ((NodeCount - 1) div 1024) + 1;
    SetLength(ASegments, SegmentCount);
    for i := 0 to SegmentCount-1 do
    begin
      pSegment := @ASegments[i];
      pSegment^.FromIndex := i * 1024;
      pSegment^.ToIndex := min(NodeCount, pSegment^.FromIndex + 1024); // exclusive
      pSegment^.BBox.InitForPoints(ANodes, pSegment^.FromIndex, pSegment^.ToIndex);
    end;
  end;

  if HasNodes then
  begin
    IdCurrent := 0;
    while (IdCurrent < NodeCount) do
    begin
      Bitmask := 1;
      Read(BitsetByte);
      n := 0;
      while (n < 8) and (IdCurrent < NodeCount) do
      begin
        if (BitsetByte and Bitmask) <> 0 then
        begin
          //ANodes[IdCurrent].Serial := Stream.ReadByte();
          Read(TmpByte);
        end;
        Bitmask := Bitmask shl 1;
        Inc(IdCurrent);
        Inc(n);
      end;
    end;
  end;
  Result := True;
end;

procedure TFileScanner.ReadTypeId(out AValue: TTypeId; AMaxBytes: Byte);
var
  TmpByte: Byte;
begin
  if AMaxBytes = 1 then
  begin
    Read(TmpByte);
    AValue := TmpByte;
  end
  else if AMaxBytes = 2 then
  begin
    Read(AValue)
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TFileScanner.ReadObjectFileRefs(var AValue: array of TObjectFileRef);
var
  i: Integer;
begin
  for i := 0 to Length(AValue)-1 do
  begin
    Read(AValue[i]);
  end;
end;

{ TObjectFileRefStreamReader }

procedure TObjectFileRefStreamReader.Init(AReader: TFileScanner;
  AOffset: TFileOffset);
begin
  FReader := AReader;
  FLastFileOffset := AOffset;
end;

procedure TObjectFileRefStreamReader.Reset();
begin
  FLastFileOffset := 0;
end;

procedure TObjectFileRefStreamReader.Read(var AValue: TObjectFileRef);
var
  TmpOffset: TFileOffset;
begin
  FReader.ReadNumber(TmpOffset);
  AValue.RefType := TRefType(TmpOffset mod 4);

  TmpOffset := TmpOffset shr 2;
  AValue.Offset := TmpOffset + FLastFileOffset;

  FLastFileOffset := AValue.Offset;
end;

end.

