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
  OsMap styles and color routines
util\Color
  Color -> TMapColor

StyleDescription:
  Style -> TStyle
  StyleAttributeType
  StyleAttributeDescriptor
  StyleDescriptor
unused:
  StyleVoidAttributeDescriptor
  StyleBoolAttributeDescriptor
  StyleStringAttributeDescriptor
  StyleColorAttributeDescriptor
  StyleMagnificationAttributeDescriptor
  StyleEnumAttributeDescriptor
  StyleDisplayAttributeDescriptor
  StyleUDisplayAttributeDescriptor
  StyleMapAttributeDescriptor
  StyleUMapAttributeDescriptor
  StyleDoubleAttributeDescriptor
  StyleUDoubleAttributeDescriptor
  StyleUDoubleArrayAttributeDescriptor
  StyleIntAttributeDescriptor
  StyleUIntAttributeDescriptor
  StyleLabelAttributeDescriptor
  StyleSymbolAttributeDescriptor

Styles:
  LineStyle ..
  CapStyleEnumAttributeDescriptor -> CreateCapStyleEnumAttributeDescriptor()
  OffsetRelAttributeDescriptor -> CreateOffsetRelAttributeDescriptor()
  FillStyle
  BorderStyle
  LabelStyle
  TextStyle  (note - disabled LabelProvider)
  TextStyleEnumAttributeDescriptor -> CreateTextStyleEnumAttributeDescriptor()
  ShieldStyle
  PathShieldStyle
  PathTextStyle
  IconStyle

  DrawPrimitive
  PolygonPrimitive
  RectanglePrimitive
  CirclePrimitive

  Symbol -> TMapSymbol
  PathSymbolStyle

*)
unit OsMapStyles;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$ZEROBASEDSTRINGS OFF}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fgl, Graphics,
  {$else}
  System.Generics.Collections,
  System.UITypes,
  FMX.Graphics,
  {$endif}
  OsMapTypes, OsMapUtils;

type
  TMapSymbol = class;

  {$ifndef FPC}
  TColor = TAlphaColor;
  {$endif}

  { TMapColor }

  TMapColor = object
  public
    R: Double; // Red   (0.0 .. 1.0)
    G: Double; // Green (0.0 .. 1.0)
    B: Double; // Blue  (0.0 .. 1.0)
    A: Double; // Alpha (0.0 .. 1.0)

    procedure Init(AR, AG, AB, AA: Double); inline;
    procedure Assign(const AValue: TMapColor); inline;
    { True if color not transparent (Alpha = 1) }
    function IsSolid(): Boolean; inline;
    { True if color transparent (Alpha = 0) }
    function IsVisible(): Boolean; inline;
    procedure InitFromBytes(AR, AG, AB, AA: Byte); inline;
    procedure InitRandom();

    function Lighten(AFactor: Double): TMapColor;
    function Darken(AFactor: Double): TMapColor;
    function Decolor(): TMapColor;

    function ToHexString(): string;
    { Convert the given color string to a color value
      The string must either be of the format
      - #HHHHHH
      - #HHHHHHHH
      where '#' is the symbol itself and 'H' represents a hexadecimal value }
    function FromHexString(const AHexString: string): TMapColor;
  end;

  { Interface class that offers a medium generic interface for styles classes. The
    interface defines methods for setting index attributes to a given value.

    Used by the style sheet parser. The parser uses the StyleDescriptor to get te attribute name,
    type and index. Attribute values are written back to the style instance using the index. }
  TStyle = class(TObject)
  public
    Name: string;
    {
    procedure SetBoolValue(AAttrIndex: Integer; AValue: Boolean); virtual;
    procedure SetStringValue(AAttrIndex: Integer; const AValue: string); virtual;
    procedure SetColorValue(AAttrIndex: Integer; const AValue: TColor); virtual;
    procedure SetMagnificationValue(AAttrIndex: Integer; const AValue: TMagnification); virtual;
    procedure SetDoubleValue(AAttrIndex: Integer; AValue: Double);
    procedure SetDoubleArrayValue(AAttrIndex: Integer; const AValue: array of Double); virtual;
    procedure SetSymbolValue(AAttrIndex: Integer; const AValue: TSymbol); virtual;
    procedure SetIntValue(AAttrIndex: Integer; AValue: Integer); virtual;
    procedure SetUIntValue(AAttrIndex: Integer; AValue: Cardinal); virtual;
    procedure SetLabelValue(AAttrIndex: Integer; const AValue: TLabelProviderRef); virtual;
    }
  end;

  {$ifdef FPC}
  TStyleList = specialize TFPGObjectList<TStyle>;
  {$else}
  TStyleList = TObjectList<TStyle>;
  {$endif}

  { Enumeration of different style sheet attribute value types }
  TStyleAttributeType = (
    TYPE_VOID,             // 'VOID' attribute. This attribute type is only used internally.
    TYPE_BOOL,             // bool attribute value
    TYPE_STRING,           // string attribute value
    TYPE_COLOR,            // color attribute value
    TYPE_MAGNIFICATION,    // magnification attribute value
    TYPE_ENUM,             // enumeration attribute value. The base class has to get derived for defining an actual enum type.
    TYPE_DISPLAY_SIZE,     // display size (using 'mm' unit) attribute value
    TYPE_UDISPLAY_SIZE,    // unsigned display size (using 'mm' unit) attribute value
    TYPE_MAP_SIZE,         // map size (using 'm' unit) attribute value
    TYPE_UMAP_SIZE,        // unsigned map size (using 'm' unit) attribute value
    TYPE_DOUBLE,           // unitless double attribute value
    TYPE_UDOUBLE,          // unitless unsigned double attribute value
    TYPE_UDOUBLE_ARRAY,    // unitless array of unsigned double attribute value
    TYPE_INT,              // unitless int attribute value
    TYPE_UINT,             // unitless unsigned int attribute value
    TYPE_LABEL,            // label attribute value
    TYPE_SYMBOL            // symbol attribute value
  );

  { Base class for all attribute metadata }

  { TStyleAttributeDescriptor }
  { not used }
  TStyleAttributeDescriptor = class
  private
    FAttrType: TStyleAttributeType; // Type of the attribute
    FName: string;         // Name of the attribute
    FAttributeId: Integer; // The id of the attribute to set
    { TODO : may be need optimize }
    FEnumNames: array of string;
    FEnumValues: array of Integer;
  public
    constructor Create(AAttrType: TStyleAttributeType;
       AName: string; AAttributeId: Integer);

    { for TYPE_ENUM }
    procedure AddEnumValue(const AName: string; AValue: Integer);
    function GetEnumValue(const AName: string): Integer;

    property AttrType: TStyleAttributeType read FAttrType;
    property Name: string read FName;
    property AttributeId: Integer read FAttributeId;
  end;

  TAttributeMap = class(TStringList)
  public
    procedure SetValue(const AName: string; AValue: TStyleAttributeDescriptor);
    function GetValue(const AName: string): TStyleAttributeDescriptor;
    property Values[const AName: string]: TStyleAttributeDescriptor read GetValue write SetValue; default;
  end;

  { Holds Meta information and technical description of a style. It currently holds
    a list of parameters and their types. It also allows to assign type safe values
    to a given style object. }

  { TStyleDescriptor }
  { not used }
  TStyleDescriptor = class
  private
    FAttributeMap: TAttributeMap;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure AddAttribute(AAttribute: TStyleAttributeDescriptor);
    function GetAttribute(const AName: string): TStyleAttributeDescriptor;
  end;


  { === Line style === }

  TLineCapStyle = (capButt, capRound, capSquare);

  TLineOffsetRel = (lorBase, lorLeftOutline, lorRightOutline, lorLaneDivider);

  TLineAttribute = (attrLineColor, attrGapColor, attrDisplayWidth,
                    attrWidth, attrDisplayOffset, attrOffset,
                    attrJoinCap, attrEndCap, attrDashes,
                    attrPriority, attrZIndex, attrOffsetRel);

  { Style options for a line. }

  { TLineStyle }

  TLineStyle = class(TStyle)
  private
  public
    Slot: string;
    LineColor: TMapColor;
    GapColor: TMapColor;
    DisplayWidth: Double;    // width in mm, added to Width
    Width: Double;           // width in meters
    DisplayOffset: Double;
    Offset: Double;          // for parallel line
    JoinCap: TLineCapStyle;  // capButt, capRound, capSquare
    EndCap: TLineCapStyle;   // capButt, capRound, capSquare
    Dash: array of Double;   // pairs of line and gap widths (in pixels?)
    Priority: Integer;
    ZIndex: Integer;
    OffsetRel: TLineOffsetRel;  // lorBase, lorLeftOutline, lorRightOutline, lorLaneDivider

    function IsVisible(): Boolean;
    function HasDashes(): Boolean;

    procedure AddDash(ALen, AGap: Double);
  end;

  {$ifdef FPC}
  TLineStyleList = specialize TFPGList<TLineStyle>;
  {$else}
  TLineStyleList = TList<TLineStyle>;
  {$endif}

  TFillStyleAttribute = (attrFillColor, attrPattern, attrPatternMinMag);

  { Style options for filling an area. }
  TFillStyle = class(TStyle)
  public
    FillColor: TMapColor;
    Pattern: string;
    PatternId: Integer;
    PatternMinMag: TMagnification;
  end;

  {TBorderStyleAttribute = (
      attrColor,
      attrGapColor,
      attrWidth,
      attrDashes,
      attrDisplayOffset,
      attrOffset,
      attrPriority);  }

  { Style options for borders around an area. }
  TBorderStyle = class(TStyle)
  private
  public
    Slot: string;
    Color: TMapColor;
    GapColor: TMapColor;
    Width: Double;          // border line width in mm
    Dash: array of Double;
    DisplayOffset: Double;
    Offset: Double;
    Priority: Integer;

    function IsVisible(): Boolean;
  end;

  {$ifdef FPC}
  TBorderStyleList = specialize TFPGList<TBorderStyle>;
  {$else}
  TBorderStyleList = TList<TBorderStyle>;
  {$endif}

  { Abstract base class for all (point) labels. All point labels have priority
    and a alpha value. }
  TLabelStyle = class(TStyle)
  public
    Priority: Integer;
    Size: Double;
  end;

  TTextStyleStyle = (tssNormal, tssEmphasize);
  {TTextStyleAttribute = (
      attrPriority,
      attrSize,
      attrLabel,
      attrPosition,
      attrTextColor,
      attrStyle,
      attrScaleAndFadeMag,
      attrAutoSize);  }

  { A textual label. }
  TTextStyle = class(TLabelStyle)
  public
    Slot: string;
    //LabelRef: TLabelProvider;    // The label - a reference to a feature and its label index
    FeatureType: TFeatureType;   // feature type, used for getting 'label' value
    Position: Integer;           // Relative vertical position of the label
    TextColor: TMapColor;        // Color of text
    Style: TTextStyleStyle;      // Style of the text
    ScaleAndFadeMagLevel: Byte;  // Automatic pseudo-autoSize scaling for nodes
    IsAutoSize: Boolean;         // Calculate the size of the label base don the height of the area

    // IsVisible() -> (FeatureType <> ftNone) and TextColor.IsVisible
    // GetAlpha() -> TextColor.A
  end;

  {$ifdef FPC}
  TTextStyleList = specialize TFPGList<TTextStyle>;
  {$else}
  TTextStyleList = TList<TTextStyle>;
  {$endif}

  {TShieldStyleAttribute = (
      attrPriority,
      attrSize,
      attrLabel,
      attrTextColor,
      attrBgColor,
      attrBorderColor);  }

  { A shield or plate label (text placed on a plate). }
  TShieldStyle = class(TLabelStyle)
  public
    //LabelRef: TLabelProvider; // The label - a reference to a feature and its label index
    FeatureType: TFeatureType;   // feature type, used for getting 'label' value
    TextColor: TMapColor;          // Color of text
    BgColor: TMapColor;            // Color of text
    BorderColor: TMapColor;        // Color of text
  end;

  {TPathShieldStyleAttribute = (
      attrPriority,
      attrSize,
      attrLabel,
      attrTextColor,
      attrBgColor,
      attrBorderColor,
      attrShieldSpace);  }

  { A style defining repetive drawing of a shield label along a path. It consists
    mainly of the attributes of the shield itself (it internally holds a shield
    label for this) and some more attributes defining the way of repetition. }
  TPathShieldStyle = class(TStyle)
  private
    FShieldStyle: TShieldStyle;
  public
    ShieldSpace: Double;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property ShieldStyle: TShieldStyle read FShieldStyle;
  end;

  {TPathTextStyleAttribute = (
      attrLabel,
      attrSize,
      attrTextColor,
      attrDisplayOffset,
      attrOffset,
      attrPriority); }

  { A style for drawing text onto a path, the text following the
    contour of the path. }
  TPathTextStyle = class(TStyle)
  public
    //LabelRef: TLabelProvider; // The label - a reference to a feature and its label index
    FeatureType: TFeatureType;   // feature type, used for getting 'label' value
    Size: Double;                // font height in mm
    TextColor: TMapColor;        // Color of text
    DisplayOffset: Double;
    Offset: Double;
    Priority: Integer;
  end;

  {TIconStyleAttribute = (
      attrSymbol,
      attrIconName,
      attrPosition); }

  { The icon style allow the rendering of external images or internal symbols. }
  TIconStyle = class(TStyle)
  public
    Symbol: TMapSymbol; // The label - a reference to a feature and its label index
    Size: Double;
    IconName: string;    // name of the icon as given in style
    IconId: Integer;     // Id for external resource binding
    Width: Integer;      // width of icon in pixels
    Height: Integer;     // height of icon in pixels
    Position: Integer;   // Relative vertical position of the label
  end;

  TDrawPrimitiveProjectionMode = (MAP, GROUND);

  { TDrawPrimitive }

  TDrawPrimitive = class
  private
    FProjectionMode: TDrawPrimitiveProjectionMode;
    FFillStyle: TFillStyle;
    FBorderStyle: TBorderStyle;
  public
    constructor Create(AProjectionMode: TDrawPrimitiveProjectionMode;
       AFillStyle: TFillStyle;
       ABorderStyle: TBorderStyle);

    procedure GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double); virtual; abstract;

    property ProjectionMode: TDrawPrimitiveProjectionMode read FProjectionMode;
    property FillStyle: TFillStyle read FFillStyle;
    property BorderStyle: TBorderStyle read FBorderStyle;
  end;

  {$ifdef FPC}
  TDrawPrimitiveList = specialize TFPGList<TDrawPrimitive>;
  {$else}
  TDrawPrimitiveList = TList<TDrawPrimitive>;
  {$endif}

  { TPolygonPrimitive }

  TPolygonPrimitive = class(TDrawPrimitive)
  public
    Coords: array of TVertex2D;

    procedure AddCoord(const ACoord: TVertex2D);

    procedure GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double); override;
  end;

  { TRectanglePrimitive }

  TRectanglePrimitive = class(TDrawPrimitive)
  public
    TopLeft: TVertex2D;
    Width: Double;
    Height: Double;

    constructor Create(AProjectionMode: TDrawPrimitiveProjectionMode;
       const ATopLeft: TVertex2D;
       AWidth: Double;
       AHeight: Double;
       AFillStyle: TFillStyle;
       ABorderStyle: TBorderStyle);

    procedure GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double); override;
  end;

  { TCirclePrimitive }

  TCirclePrimitive = class(TDrawPrimitive)
  public
    Center: TVertex2D;
    Radius: Double;

    constructor Create(AProjectionMode: TDrawPrimitiveProjectionMode;
       const ACenter: TVertex2D;
       ARadius: Double;
       AFillStyle: TFillStyle;
       ABorderStyle: TBorderStyle);

    procedure GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double); override;
  end;

  {  Definition of a symbol. A symbol consists of a list of DrawPrimitives
     with with assigned rendering styles. }

  { TMapSymbol }

  TMapSymbol = class
  private
    FPrimitives: TDrawPrimitiveList;
    FName: string;
    FMinX: Double;
    FMinY: Double;
    FMaxX: Double;
    FMaxY: Double;
  public

    constructor Create(AName: string);
    destructor Destroy; override;

    procedure AddPrimitive(APrimitive: TDrawPrimitive);

    procedure GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double);

    function GetWidth(): Double;
    function GetHeight(): Double;

    property Primitives: TDrawPrimitiveList read FPrimitives;
    property Name: string read FName;
    property Width: Double read GetWidth;
    property Height: Double read GetHeight;
  end;

  {TPathSymbolStyleAttribute = (
      attrSymbol,
      attrSymbolSpace,
      attrDisplayOffset,
      attrOffset);  }

  { Style for repetive drawing of symbols on top of a path. }
  TPathSymbolStyle = class(TStyle)
  private
    FSymbol: TMapSymbol;
    FSymbolSpace: Double;
    FDisplayOffset: Double;
    FOffset: Double;

  public
    property Symbol: TMapSymbol read FSymbol;
    property SymbolSpace: Double read FSymbolSpace;
    property DisplayOffset: Double read FDisplayOffset;
    property Offset: Double read FOffset;

    //procedure CopyAttributes(AOther: TPathSymbolStyle; AAttr: TPathSymbolStyleAttribute);
  end;

  function IsColorVisible(AColor: TColor): Boolean;
  function IsColorSolid(AColor: TColor): Boolean;
  function ColorLighten(AColor: TColor; AFactor: Real): TColor;
  function ColorDarken(AColor: TColor; AFactor: Real): TColor;
  function ColorGray(AColor: TColor): TColor;
  function ColorToHexString(AColor: TColor): string;
  function ColorFromHexString(AStr: string): TColor;

  {
  function MapColorBlack(): TMapColor;
  function MapColorWhite(): TMapColor;
  function MapColorRed(): TMapColor;
  function MapColorGreen(): TMapColor;
  function MapColorBlue(): TMapColor; }

  function CreateCapStyleEnumAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;
  function CreateOffsetRelAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;
  function CreateTextStyleEnumAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;

implementation

uses Math;

const
  COLOR_CONV_CONST = 1.0 / 255;

function fmod(a, b: Double): Double;
begin
  Result := a - b * Int(a / b);
end;

function IsColorVisible(AColor: TColor): Boolean;
begin
  Result := ((AColor and $FF000000) <> 0);
end;

function IsColorSolid(AColor: TColor): Boolean;
begin
  Result := ((AColor and $FF000000) = $FF000000);
end;

function ColorLighten(AColor: TColor; AFactor: Real): TColor;
var
  r, g, b, a: Real;
begin
  r := ((AColor and $000000FF) shr 0);
  g := ((AColor and $0000FF00) shr 8);
  b := ((AColor and $00FF0000) shr 16);
  a := ((AColor and $FF000000) shr 24);
  Result := Byte(Trunc(r + (1-r) * AFactor))
         or (Byte(Trunc(g + (1-g) * AFactor)) shl 8)
         or (Byte(Trunc(b + (1-b) * AFactor)) shl 16)
         or (Byte(Trunc(a)) shl 24);
end;

function ColorDarken(AColor: TColor; AFactor: Real): TColor;
var
  r, g, b, a: Real;
begin
  r := ((AColor and $000000FF) shr 0);
  g := ((AColor and $0000FF00) shr 8);
  b := ((AColor and $00FF0000) shr 16);
  a := ((AColor and $FF000000) shr 24);
  Result := Byte(Trunc(r - r * AFactor))
         or (Byte(Trunc(g - g * AFactor)) shl 8)
         or (Byte(Trunc(b - b * AFactor)) shl 16)
         or (Byte(Trunc(a)) shl 24);
end;

function ColorGray(AColor: TColor): TColor;
var
  r, g, b, a, gray: Real;
begin
  r := ((AColor and $000000FF) shr 0);
  g := ((AColor and $0000FF00) shr 8);
  b := ((AColor and $00FF0000) shr 16);
  a := ((AColor and $FF000000) shr 24);
  gray := (r + g + b) / 3.0;
  Result := Byte(Trunc(gray))
         or (Byte(Trunc(gray)) shl 8)
         or (Byte(Trunc(gray)) shl 16)
         or (Byte(Trunc(a)) shl 24);
end;

function ColorToHexString(AColor: TColor): string;
begin
  Result := '#' + IntToHex(AColor, 8);
end;

{ Convert the given color string to a color value
  The string must either be of the format
  - #HHHHHH
  - #HHHHHHHH
  where '#' is the symbol itself and 'H' represents a hexadecimal value }
function ColorFromHexString(AStr: string): TColor;
begin
  if Length(AStr) >= 7 then
  begin
    AStr[1] := 'x';
    Result := StrToIntDef(AStr, 0);
  end;
  Result := 0;
end;


function CreateCapStyleEnumAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;
begin
  Result := TStyleAttributeDescriptor.Create(TYPE_ENUM, AName, AAttr);
  Result.AddEnumValue('butt', Ord(TLineCapStyle.capButt));
  Result.AddEnumValue('round', Ord(TLineCapStyle.capRound));
  Result.AddEnumValue('square', Ord(TLineCapStyle.capSquare));
end;

function CreateOffsetRelAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;
begin
  Result := TStyleAttributeDescriptor.Create(TYPE_ENUM, AName, AAttr);
  Result.AddEnumValue('base', Ord(lorBase));
  Result.AddEnumValue('leftOutline', Ord(lorLeftOutline));
  Result.AddEnumValue('rightOutline', Ord(lorRightOutline));
  Result.AddEnumValue('laneDivider', Ord(lorLaneDivider));
end;

function CreateTextStyleEnumAttributeDescriptor(const AName: string; AAttr: Integer): TStyleAttributeDescriptor;
begin
  Result := TStyleAttributeDescriptor.Create(TYPE_ENUM, AName, AAttr);
  Result.AddEnumValue('normal', Ord(TTextStyleStyle.tssNormal));
  Result.AddEnumValue('emphasize', Ord(TTextStyleStyle.tssEmphasize));
end;

{ TMapColor }

procedure TMapColor.Init(AR, AG, AB, AA: Double);
begin
  R := AR;
  G := AG;
  B := AB;
  A := AA;
end;

procedure TMapColor.Assign(const AValue: TMapColor);
begin
  Assert((R >= 0.0) and (R <= 1.0));
  Assert((G >= 0.0) and (G <= 1.0));
  Assert((B >= 0.0) and (B <= 1.0));
  Assert((A >= 0.0) and (A <= 1.0));

  R := AValue.R;
  G := AValue.G;
  B := AValue.B;
  A := AValue.A;
end;

function TMapColor.IsSolid(): Boolean;
begin
  Result := (A = 0.0);
end;

function TMapColor.IsVisible(): Boolean;
begin
  Result := (A > 0.0);
end;

procedure TMapColor.InitFromBytes(AR, AG, AB, AA: Byte);
begin
  R := AR * COLOR_CONV_CONST;
  G := AG * COLOR_CONV_CONST;
  B := AB * COLOR_CONV_CONST;
  if AA > 250 then
    A := 1.0
  else
    A := AA * COLOR_CONV_CONST;
end;

procedure TMapColor.InitRandom();
begin
  R := 1 / (Random(31)+1);
  G := 1 / (Random(31)+1);
  B := 1 / (Random(31)+1);
  A := 1.0;
end;

function TMapColor.Lighten(AFactor: Double): TMapColor;
begin
  Result.R := R + (1-R) * AFactor;
  Result.G := G + (1-G) * AFactor;
  Result.B := B + (1-B) * AFactor;
  Result.A := A;
end;

function TMapColor.Darken(AFactor: Double): TMapColor;
begin
  Result.R := R - R * AFactor;
  Result.G := G - G * AFactor;
  Result.B := B - B * AFactor;
  Result.A := A;
end;

function TMapColor.Decolor(): TMapColor;
begin
  Result.R := (R + G + B) / 3.0; // grey
  Result.G := Result.R;
  Result.B := Result.R;
  Result.A := A;
end;

function TMapColor.ToHexString(): string;
var
  dwColor: LongWord;
  btR, btG, btB, btA: Byte;
begin
  btR := (Byte(Trunc(R * 255.0 / 16.0)) shl 4) or Byte(Trunc(fmod(R * 255.0, 16)));
  btG := (Byte(Trunc(G * 255.0 / 16.0)) shl 4) or Byte(Trunc(fmod(G * 255.0, 16)));
  btB := (Byte(Trunc(B * 255.0 / 16.0)) shl 4) or Byte(Trunc(fmod(B * 255.0, 16)));
  if A <> 1.0 then
  begin
    btA := (Byte(Trunc(A * 255.0 / 16.0)) shl 4) or Byte(Trunc(fmod(A * 255.0, 16)));
    dwColor := (btR shl 24) or (btG shl 16) or (btB shl 8) or btA;
    Result := '#' + IntToHex(dwColor, 8);
  end
  else
  begin
    dwColor := (btR shl 16) or (btG shl 8) or (btB shl 0);
    Result := '#' + IntToHex(dwColor, 6);
  end;
end;

function TMapColor.FromHexString(const AHexString: string): TMapColor;
begin
  if Length(AHexString) >= 7 then
  begin
    Result.R := (16 * (StrToIntDef('x' + AHexString[2], 0)))
              + (StrToIntDef('x' + AHexString[3], 0) / 255.0);
    Result.G := (16 * (StrToIntDef('x' + AHexString[4], 0)))
              + (StrToIntDef('x' + AHexString[5], 0) / 255.0);
    Result.B := (16 * (StrToIntDef('x' + AHexString[6], 0)))
              + (StrToIntDef('x' + AHexString[7], 0) / 255.0);
  end;
  if Length(AHexString) >= 9 then
  begin
    Result.R := (16 * (StrToIntDef('x' + AHexString[8], 0)))
              + (StrToIntDef('x' + AHexString[9], 0) / 255.0);

  end
  else
    Result.R := 1.0;
end;

{ TStyleAttributeDescriptor }

constructor TStyleAttributeDescriptor.Create(AAttrType: TStyleAttributeType;
  AName: string; AAttributeId: Integer);
begin
  FAttrType := AAttrType;
  FName := AName;
end;

procedure TStyleAttributeDescriptor.AddEnumValue(const AName: string;
  AValue: Integer);
var
  i, n, nn: Integer;
begin
  nn := Length(FEnumNames);
  n := -1;
  for i := 0 to nn-1 do
  begin
    if FEnumNames[i] = AName then
    begin
      n := i;
      Break;
    end;
  end;

  if n < 0 then
  begin
    n := nn;
    SetLength(FEnumNames, nn + 1);
    SetLength(FEnumValues, nn + 1);
    FEnumNames[n] := AName;
  end;

  FEnumValues[n] := AValue;
end;

function TStyleAttributeDescriptor.GetEnumValue(const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(FEnumNames)-1 do
  begin
    if FEnumNames[i] = AName then
    begin
      Result := FEnumValues[i];
      Exit;
    end;
  end;
  Result := 0;
end;

{ TStyleDescriptor }

procedure TStyleDescriptor.AfterConstruction();
begin
  inherited AfterConstruction();
  FAttributeMap := TAttributeMap.Create();
end;

procedure TStyleDescriptor.BeforeDestruction();
begin
  FreeAndNil(FAttributeMap);
  inherited BeforeDestruction();
end;

procedure TStyleDescriptor.AddAttribute(AAttribute: TStyleAttributeDescriptor);
begin
  FAttributeMap.Values[AAttribute.Name] := AAttribute;
end;

function TStyleDescriptor.GetAttribute(const AName: string): TStyleAttributeDescriptor;
begin
  Result := FAttributeMap.Values[AName];
end;

{ TLineStyle }

function TLineStyle.IsVisible(): Boolean;
begin
  Result := (DisplayWidth > 0.0) or (Width > 0.0);
  //Result := Result and IsColorVisible(LineColor);
end;

function TLineStyle.HasDashes(): Boolean;
begin
  Result := (Length(Dash) <> 0);
end;

procedure TLineStyle.AddDash(ALen, AGap: Double);
var
  n: Integer;
begin
  n := Length(Dash);
  SetLength(Dash, n+2);
  Dash[n+0] := ALen;
  Dash[n+1] := AGap;
end;

{ TBorderStyle }

function TBorderStyle.IsVisible(): Boolean;
begin
  Result := (Width > 0);
  //Result := (Width > 0) and IsColorVisible(Color);
end;

{ TDrawPrimitive }

constructor TDrawPrimitive.Create(
  AProjectionMode: TDrawPrimitiveProjectionMode; AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
begin
  FProjectionMode := AProjectionMode;
  FFillStyle := AFillStyle;
  FBorderStyle := ABorderStyle;
end;

{ TPolygonPrimitive }

procedure TPolygonPrimitive.AddCoord(const ACoord: TVertex2D);
var
  n: Integer;
begin
  n := Length(Coords);
  SetLength(Coords, n + 1);
  Coords[n].Assign(ACoord);
end;

procedure TPolygonPrimitive.GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double);
var
  i: Integer;
begin
  AMinX := MaxDouble;
  AMinY := MaxDouble;
  AMaxX := -MaxDouble;
  AMaxY := -MaxDouble;

  for i := 0 to Length(Coords)-1 do
  begin
    AMinX := min(AMinX, Coords[i].X);
    AMinY := min(AMinY, Coords[i].Y);

    AMaxX := max(AMaxX, Coords[i].X);
    AMaxY := max(AMaxY, Coords[i].Y);
  end;
end;

{ TRectanglePrimitive }

constructor TRectanglePrimitive.Create(
  AProjectionMode: TDrawPrimitiveProjectionMode; const ATopLeft: TVertex2D;
  AWidth: Double; AHeight: Double; AFillStyle: TFillStyle;
  ABorderStyle: TBorderStyle);
begin
  inherited Create(AProjectionMode, AFillStyle, ABorderStyle);
  TopLeft.Assign(ATopLeft);
  Width := AWidth;
  Height := AHeight;
end;

procedure TRectanglePrimitive.GetBoundingBox(out AMinX, AMinY, AMaxX,
  AMaxY: Double);
begin
  AMinX := TopLeft.X;
  AMinY := TopLeft.Y;
  AMaxX := TopLeft.X + Width;
  AMaxY := TopLeft.Y + Height;
end;

{ TCirclePrimitive }

constructor TCirclePrimitive.Create(
  AProjectionMode: TDrawPrimitiveProjectionMode; const ACenter: TVertex2D;
  ARadius: Double; AFillStyle: TFillStyle; ABorderStyle: TBorderStyle);
begin
  inherited Create(AProjectionMode, AFillStyle, ABorderStyle);
  Center.Assign(ACenter);
  Radius := ARadius;
end;

procedure TCirclePrimitive.GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double);
begin
  AMinX := Center.X - Radius;
  AMinY := Center.Y - Radius;

  AMaxX := Center.X + Radius;
  AMaxY := Center.Y + Radius;
end;

{ TMapSymbol }

constructor TMapSymbol.Create(AName: string);
begin
  inherited Create();
  FPrimitives := TDrawPrimitiveList.Create();

  FName := AName;
  FMinX := MaxDouble;
  FMinY := MaxDouble;
  FMaxX := -MaxDouble;
  FMaxY := -MaxDouble;
end;

destructor TMapSymbol.Destroy;
begin
  FreeAndNil(FPrimitives);
  inherited Destroy;
end;

procedure TMapSymbol.AddPrimitive(APrimitive: TDrawPrimitive);
var
  _MinX, _MinY, _MaxX, _MaxY: Double;
begin
  APrimitive.GetBoundingBox(_MinX, _MinY, _MaxX, _MaxY);
  FMinX := min(FMinX, _MinX);
  FMinY := min(FMinY, _MinY);
  FMaxX := max(FMaxX, _MaxX);
  FMaxY := max(FMaxY, _MaxY);
  Primitives.Add(APrimitive);
end;

procedure TMapSymbol.GetBoundingBox(out AMinX, AMinY, AMaxX, AMaxY: Double);
begin
  AMinX := FMinX;
  AMinY := FMinY;
  AMaxX := FMaxX;
  AMaxY := FMaxY;
end;

function TMapSymbol.GetWidth(): Double;
begin
  Result := FMaxX - FMinX;
end;

function TMapSymbol.GetHeight(): Double;
begin
  Result := FMaxY - FMinY;
end;

{ TPathShieldStyle }

procedure TPathShieldStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  FShieldStyle := TShieldStyle.Create();
end;

procedure TPathShieldStyle.BeforeDestruction;
begin
  FreeAndNil(FShieldStyle);
  inherited BeforeDestruction;
end;


{ TAttributeMap }

function TAttributeMap.GetValue(const AName: string): TStyleAttributeDescriptor;
var
  n: Integer;
begin
  n := IndexOf(AName);
  if n <> -1 then
    Result := TStyleAttributeDescriptor(GetObject(n))
  else
    Result := nil;
end;

procedure TAttributeMap.SetValue(const AName: string;
  AValue: TStyleAttributeDescriptor);
var
  n: Integer;
begin
  n := IndexOf(AName);
  if n <> -1 then
    Objects[n] := AValue
  else
    AddObject(AName, AValue);
end;

end.

