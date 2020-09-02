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
  OsMap object types and features
TypeFeature:
  FeatureValue -> string
  Feature
  FeatureInstance -> TFeatureInfo

TypeConfig:
  TypeInfo  -> TTypeInfo
  TypeCondition -> TTypeCondition
  FeatureValueBuffer -> TFeatureValueBuffer
  TypeConfig ...

TypeInfoSet:
  TypeInfoSet ...
*)
unit OsMapObjTypes;

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
  OsMapUtils, OsMapTypes, OsMapTags, OsMapFiles;

const
  ObjTypeIgnore   = 0;
  ObjTypeNode     = 1 shl 0; // Condition applies to nodes
  ObjTypeWay      = 1 shl 1; // Condition applies to ways
  ObjTypeArea     = 1 shl 2; // Condition applies to areas
  ObjTypeRelation = 1 shl 3; // Condition applies to releations

type

  TConditionType = (ctIgnore, ctNode, ctWay, ctArea, ctRelation);
  TConditionTypes = set of TConditionType;

  { TFeatureValue }

  (*
  TFeatureValue = class
  public
    procedure Assign(AValue: TFeatureValue); virtual; abstract;
    function IsEqual(): Boolean; virtual;

    function GetLabel(AIndex: Integer): string; virtual; abstract;
    function IsFlagSet(AIndex: Integer): Boolean; virtual;

    { Read the value of the Feature from the FileScanner }
    procedure Read(AScanner: TFileScanner); virtual; abstract;
    { Write the FeatureValue to disk. }
    procedure Write(AWriter: TFileWriter); virtual; abstract;
  end; *)

  //TFeatureLabelsMap = specialize TFPGMap<string, Integer>;
  //TFeatureFlagsMap = specialize TFPGMap<string, Integer>;
  //TDescriptionsMap = specialize TFPGMap<string, string>;
  TDescriptionsMap = TStringList;

  { TFeatureValueStorage }
  { When value added to storage, it get SID (string ID) and can be retrieved by
    that SID }
  TFeatureValueStorage = class(TObject)
  private
    FStrList: TStringList;
    FStrHash: TSimpleStringHash;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    procedure ClearHash();

    { Add string to dictionary, return it SID }
    function AddStr(const AValue: string): Integer;
    { Get sring value by SID, return empty string if SID not exists }
    function GetStr(ASID: Integer): string;

    property StrList: TStringList read FStrList;
  end;

  TTypeInfo = class;
  //TFeatureValueBuffer = class;
  TFeature = class;

  { An instantiation of a feature for a certain type. }
  TFeatureInfo = object
  public
    Feature: TFeature;     // The feature we are an instance of
    TypeInfo: TTypeInfo;   // The type we are assigned to (we are no Ref type to avoid circular references)
    FeatureBit: Integer;   // index of the bit that signals that the feature is available
    Index: Integer;        // The index we have in the list of features
    //Offset: Integer;       // Our offset into the value buffer for our data
  end;

  TFeatureInfoArray = array of TFeatureInfo;

  { A FeatureValueBuffer is instantiated by an object and holds information
    about the type of the object, the features and feature values available for the given object. }
  TFeatureValueBuffer = object
  private
    { Features info }
    FTypeInfo: TTypeInfo;
    { Bitmask of enabled features, when bit is on, then correspondig feature value was set
      also, can contain optional special flags (at index over FeatureCount )}
    FFeatureMask: LongWord;
    { Array of feature values }
    FFeatureValueList: TFeatureValueList;

    procedure DeleteData();
    procedure AllocateValueBufferLazy();
    { Return the number of features defined for this type }
    function GetFeatureCount(): Integer;

    procedure SetFeatureBit(AIndex: Integer);

    { Return a raw pointer to the value (as reserved in the internal featureValueBuffer). If the
      featureValueBuffer doe snot yet exist, it will be created lazy. }
    // used only with AllocateValue()
    //function GetValueAndAllocateBuffer(AIndex: Integer): TFeatureValue;
  public
    // assigned from outside
    FeatureValueStorage: TFeatureValueStorage;

    { Deletes the current feature values and assign the type and values
     of the passed featur evalue buffer. }
    procedure Assign(const AValue: TFeatureValueBuffer);

    { Maintains the current values and types and copies over
      all not already set feature values of the passed instance to the current instance.

      Both FeatureValueBuffers do not have to have the same type. Copying is a based on
      best effort. The copy opperation is expensive. }
    procedure CopyMissingValue(const AValue: TFeatureValueBuffer);

    { Clears all feature buffer values }
    procedure ClearFeatureValues();

    procedure SetType(const AType: TTypeInfo);
    function GetType(): TTypeInfo;

    { Get a feature description for the feature with the given index ([0..featureCount[) }
    function GetFeatureInfo(AIndex: Integer): TFeatureInfo;
    { Get a feature for the given index ([0..FeatureCount) }
    function GetFeature(AIndex: Integer): TFeature;
    { Return true, if the given feature is set (available), else false.
      aka HasFeature }
    function HasFeatureValue(AIndex: Integer): Boolean;
    { Return a feature value }
    function GetValue(AIndex: Integer): TFeatureValue;
    { Set a feature value, AIndex - index of feature in TypeInfo }
    procedure SetValue(AIndex: Integer; const AValue: TFeatureValue);
    { Set feature value for specified feature type }
    procedure SetFeatureValue(AFeatureType: TFeatureType; const AValue: TFeatureValue);
    { Return feature value for specified feature type }
    function GetFeatureValue(AFeatureType: TFeatureType): string;
    { Return feature value ID for specified feature type, -1 if not found }
    function GetFeatureValueId(AFeatureType: TFeatureType): TFeatureValueId;
    { Return feature label for specified feature type }
    function GetFeatureLabel(AFeatureType: TFeatureType): string;

    // use SetValue()
    //function AllocateValue(AIndex: Integer): TFeatureValue;

    procedure FreeValue(AIndex: Integer);

    procedure Parse(AErrorReporter: TTagErrorReporter;
                    ATagRegistry: TTagRegistry;
                    const AObject: TObjectOSMRef;
                    const ATags: TTagMap);

    procedure Read(AScanner: TFileScanner); overload;

    procedure Read(AScanner: TFileScanner;
                   out ASpecialFlag1: Boolean); overload;

    procedure Read(AScanner: TFileScanner;
                   out ASpecialFlag1: Boolean;
                   out ASpecialFlag2: Boolean;
                   AFlagsCount: Integer = 2); overload;

    procedure Write(AWriter: TFileWriter); overload;

    procedure Write(AWriter: TFileWriter;
                   ASpecialFlag1: Boolean); overload;

    procedure Write(AWriter: TFileWriter;
                   ASpecialFlag1: Boolean;
                   ASpecialFlag2: Boolean;
                   AFlagsCount: Integer = 2); overload;

    { Note: Can return NULL value! }
    function FindValue(AValueType: TClass): TFeatureValue;

    property TypeInfo: TTypeInfo read FTypeInfo;

    property FeatureCount: Integer read GetFeatureCount;
  end;

  { A feature combines one or multiple tags  to build information attribute for a type.

    The class "Feature" is the abstract base class for a concrete feature implementation
    like "NameFeature" or "AccessFeature".

    A feature could just be an alias for one tag (like "name") but it could also combine
    a number of attributes (e.g. access and all its variations). }
  TFeature = class
  private
    FFeatureType: TFeatureType;
    FLabels: TSimpleStringHash;
    FFlags: TSimpleStringHash;
    FDescriptions: TDescriptionsMap; // Map of descriptions for given language codes
  protected
    FHasLabel: Boolean;
    FHasFlag: Boolean;
    procedure RegisterLabel(AIndex: Integer; ALabelName: string);
    procedure RegisterFlag(AIndex: Integer; AFlagName: string);
  public
    constructor Create(AFeatureType: TFeatureType);
    destructor Destroy; override;

    { Does further initialization based on the current TagRegistry. For example
     it registers Tags (and stores their TagId) for further processing. }
    procedure Initialize(ATagRegistry: TTagRegistry); virtual;

    procedure AddDescription(ALanguageCode, ADescription: string);

    { Returns the name of the feature }
    function GetName(): string; virtual;

    function GetLabel(const AValue: string = ''): string; virtual;
    function IsFlagSet(const AValue: string; ABitIndex: Integer): Boolean; virtual;

    { A feature, if set for an object, can hold a value. If there is no value object,
      this method returns 0, else it returns the C++ size of the value object. }
    // not used
    //function GetValueSize(): Integer;

    { This method returns the number of additional feature bits reserved. If there are
      additional features bit, 0 is returned.
      A feature may reserve additional feature bits. Feature bits should be used
      if a custom value object is too expensive. Space for feature bits is always reserved
      even if the feature itself is not set for a certain object. }
    // not used, always = 0
    function GetFeatureBitCount(): Integer; virtual;

    { Returns 'true' if the feature has an value object. }
    function HasValue(): Boolean; virtual;
    { Returns 'true' if the feature provides labels. }
    function HasLabel(): Boolean; virtual;
    { Returns 'true' if the feature provides flags. }
    function HasFlags(): Boolean; virtual;

    { Returns the index of the label with the given name. Method returns 'true'
      if the feature has labels and a label with the given name exists. Else
      'false' is returned. }
    function ReadLabelIndex(const ALabelName: string; var AIndex: Integer): Boolean;
    { Returns the index of the feature flag with the given name. Method returns 'true'
      if the feature has the named flag. Else 'false' is returned. }
    function ReadFlagIndex(const AFlagName: string; var AIndex: Integer): Boolean;

    function GetDescription(const ALanguageCode: string): string;


    { Create new TFeatureValue instance }
    // use TFeatureValueBuffer.SetValue()
    //function AllocateValue(const ABuffer: TFeatureValueList): TFeatureValue; virtual;

    procedure Parse(AReporter: TTagErrorReporter;
                    ATagRegistry: TTagRegistry;
                    const AFeatureInfo: TFeatureInfo;
                    const AObjectOSMRef: TObjectOSMRef;
                    const ATagMap: TTagMap;
                    var ABuffer: TFeatureValueBuffer); virtual;

    { Read the value of the Feature from the FileScanner }
    procedure Read(AScanner: TFileScanner; out AValue: string); virtual;
    { Write the FeatureValue to disk. }
    procedure Write(AWriter: TFileWriter; const AValue: string); virtual;

    property FeatureType: TFeatureType read FFeatureType;
    { Map of descriptions for given language codes }
    property Descriptions: TDescriptionsMap read FDescriptions;
  end;

  {$ifdef FPC}
  TFeatureList = specialize TFPGList<TFeature>;
  {$else}
  TFeatureList = TList<TFeature>;
  {$endif}

  { TTypeCondition }

  TTypeCondition = class(TObject)
  public
    // Bitset of types the condition can be applied to
    Types: TConditionTypes;
    // The root condition
    Condition: TTagCondition;

    constructor Create(ATypes: TConditionTypes; ACondition: TTagCondition);
  end;

  {$ifdef FPC}
  TTypeConditionList = specialize TFPGList<TTypeCondition>;
  {$else}
  TTypeConditionList = TList<TTypeCondition>;
  {$endif}

  //TNameToFeatureMap = specialize TFPGMap<string, Integer>;

  TFeatureTypeIndexArray = array[TFeatureType] of Integer;

  { Detailed information about one object type }

  { TTypeInfo }

  TTypeInfo = class(TObject)
  private
    // created
    FConditions: TTypeConditionList;   // One of this conditions must be fulfilled for a object to match this type
    FGroups: TStringList;              // Set of idents that server as categorizing groups
    FDescriptions: TDescriptionsMap;   // Map of descriptions for given language codes

    FNodeId: TTypeId;                  // Type if in case the object is a node
    FWayId: TTypeId;                   // Type if in case the object is a way
    FAreaId: TTypeId;                  // Type if in case the object is a area
    FTypeName: string;                 // Name of the type
    FIndex: Integer;                   // Internal unique index of the type

    FIsInternal: Boolean;              // This type is only internally used, there is no OSM date for this type

    FFeatureInfoArray: TFeatureInfoArray;  // List of feature this type has
    FFeatureMaskBytes: Integer;        // Size of the feature bitmask in bytes
    FSpecialFeatureMaskBytes: Integer; // Size of the feature bitmask in bytes
    //FFeatureValueBufferSize: Integer;  // Size of the value buffer holding values for all feature of the type
    FFeatureValueCount: Integer;       // feature values count
    FFeatureIndexMap: TFeatureTypeIndexArray; // array of features indexes

    FCanBeNode: Boolean;               // Type can be a node
    FCanBeWay: Boolean;                // Type can be a way
    FCanBeArea: Boolean;               // Type can be a area
    FCanBeRelation: Boolean;
    FIsPath: Boolean;                  // Type has path characteristics (features like bridges, tunnels, names,...)
    FCanRouteFoot: Boolean;            // Object of this type are by default routable for foot
    FCanRouteBicycle: Boolean;         // Object of this type are by default routable for bicylce
    FCanRouteCar: Boolean;             // Object of this type are by default routable for car
    FIndexAsAddress: Boolean;          // Objects of this type are addressable
    FIndexAsLocation: Boolean;         // Objects of this type are defining a location (e.g. street)
    FIndexAsRegion: Boolean;           // Objects of this type are defining a administrative region (e.g. city, county,...)
    FIndexAsPOI: Boolean;              // Objects of this type are defining a POI
    FOptimizeLowZoom: Boolean;         // Optimize objects of this type for low zoom rendering
    FIsMultipolygon: Boolean;
    FIsPinWay: Boolean;                // If there is no way/area information treat this object as way even it the way is closed
    FIsMergeAreas: Boolean;            // Areas of this type are merged under certain conditions
    FIsIgnoreSeaLand: Boolean;         // Ignore objects of this type for sea/land calculation
    FIsIgnore: Boolean;                // Ignore objects of this type
    FLanes: Byte;                      // Number of expected lanes (default: 1)
    FOnewayLanes: Byte;                // Number of expected lanes (default: 1)
    FZOrder: Byte;                     // In what order will objects drawn on map (for sorting)

    function GetCanRoute(): Boolean;
    function GetCondition(AIndex: Integer): TTypeCondition;
    function GetConditionCount(): Integer;
    function GetFeatureCount(): Integer;

  public
    { We forbid copying of TypeInfo instances}
    //constructor Create(AValue: TTypeInfo);
    constructor Create(const AName: string);
    destructor Destroy(); override;

    function AddCondition(ATypes: TConditionTypes; ACondition: TTagCondition): TTypeInfo;

    { TODO : Get rid of array }
    { Add a feature to this type }
    function AddFeature(AFeature: TFeature): TTypeInfo;
    { Add a categorizing group name to the type. }
    function AddGroup(const AGroupName: string): TTypeInfo;
    { Add a description of the type for the given language code
      ALanguageCode - language code like for example 'en' or 'de' }
    function AddDescription(const ALanguageCode: string;
                            const ADescription: string): TTypeInfo;

    function HasFeatures(): Boolean;
    { Returns true, if the feature with the given name has already been
      assigned to this type. }
    //function HasFeature(const AFeatureName: string): Boolean;
    function HasFeature(AFeatureType: TFeatureType): Boolean;
    { Return the feature with the given name }
    //function FindFeature(const AFeatureName: string; var AIndex: Integer): Boolean;

    { Return the feature with the given type }
    function FindFeature(AFeatureType: TFeatureType; var AIndex: Integer): Boolean;
    { Return TFeature at given index }
    function GetFeature(AIndex: Integer): TFeature;
    { Return the feature at the given index }
    function GetFeatureInfo(AIndex: Integer): TFeatureInfo;

    //function GetFeatureIndex(AFeatureType)

    function GetDefaultAccess(): Byte;

    { Returns the description for the given language code. Returns an empty string, if
      no description is available for the given language code. }
    function GetDescription(const ALanguageCode: string): string;
    { Returns true, if there are any conditions bound to the type. If the conditions
      are met for a given object, the object is in turn of the given type. }
    function IsHasConditions(): Boolean;

    { If set to 'true', an object of this type can be traveled by the given vehicle by default. }
    function CanRouteBy(AVehicle: TVehicleType): Boolean;

    function IsInGroup(const AName: string): Boolean;

    { Save TypeInfo properties, without lists }
    function Save(AWriter: TFileWriter): Boolean;
    { Load TypeInfo properties, without lists }
    function Load(AScanner: TFileScanner): Boolean;

    { Unique id of this type. You should not use the type id as an index. }
    property NodeId: TTypeId read FNodeId write FNodeId;
    { Unique id of this type. You should not use the type id as an index. }
    property WayId: TTypeId read FWayId write FWayId;
    { Unique id of this type. You should not use the type id as an index. }
    property AreaId: TTypeId read FAreaId write FAreaId;
    { Internal unique index of the type. The index is assured to in the interval [0..GetTypeCount()] }
    property Index: Integer read FIndex write FIndex;
    { This type is only internally used, there is no OSM date for this type }
    property IsInternal: Boolean read FIsInternal write FIsInternal;
    { Name of this type }
    property TypeName: string read FTypeName write FTypeName;

    { Returns the number of features of the asisgned type }
    property FeatureCount: Integer read GetFeatureCount;
    { List of features assigned to this type }
    property FeatureInfos: TFeatureInfoArray read FFeatureInfoArray;

    { Returns the (rounded) number of bytes required for storing the feature mask }
    property FeatureMaskBytes: Integer read FFeatureMaskBytes;
    { Returns the (rounded) number of bytes required for storing the feature mask and one additional
      general purpose signal byte. }
    property SpecialFeatureMaskBytes: Integer read FSpecialFeatureMaskBytes;
    { Returns the size of the buffer required to store all FeatureValues of this type into }
    //property FeatureValueBufferSize: Integer read FFeatureValueBufferSize;
    { feature values count }
    property FeatureValueCount: Integer read FFeatureValueCount;

    property ConditionCount: Integer read GetConditionCount;
    { List of conditions for the given type }
    property Conditions[AIndex: Integer]: TTypeCondition read GetCondition;

    { Node can be of this type. }
    property CanBeNode: Boolean read FCanBeNode write FCanBeNode;
    { Way can be of this type. }
    property CanBeWay: Boolean read FCanBeWay write FCanBeWay;
    { Area can be of this type. }
    property CanBeArea: Boolean read FCanBeArea write FCanBeArea;
    { Relation can be of this type. }
    property CanBeRelation: Boolean read FCanBeRelation write FCanBeRelation;
    { True if this is a path }
    property IsPath: Boolean read FIsPath write FIsPath;
    { Object of this type can be traveled by feet (by default). }
    property CanRouteFoot: Boolean read FCanRouteFoot write FCanRouteFoot;
    { Node can be of this type. }
    property CanRouteBicycle: Boolean read FCanRouteBicycle write FCanRouteBicycle;
    { Object of this type can be traveled by car (by default). }
    property CanRouteCar: Boolean read FCanRouteCar write FCanRouteCar;
    { Object of this type can be traveled. }
    property CanRoute: Boolean read GetCanRoute;


    { Object of this type should be indexed as an address }
    property IndexAsAddress: Boolean read FIndexAsAddress write FIndexAsAddress;
    { Object of this type should be indexed as a location. }
    property IndexAsLocation: Boolean read FIndexAsLocation write FIndexAsLocation;
    { Object of this type should be indexed as a region. }
    property IndexAsRegion: Boolean read FIndexAsRegion write FIndexAsRegion;

    { Object of this type should be indexed as a POI. }
    property IndexAsPOI: Boolean read FIndexAsPOI write FIndexAsPOI;
    { Object of this type should be optimized for low zoom. }
    property OptimizeLowZoom: Boolean read FOptimizeLowZoom write FOptimizeLowZoom;

    { Object is handled as multipolygon even though it may not have
      type=multipolygon set explicitly. }
    property IsMultipolygon: Boolean read FIsMultipolygon write FIsMultipolygon;
    {  }
    property IsPinWay: Boolean read FIsPinWay write FIsPinWay;
    { "touching" areas of this type should get merged. }
    property IsMergeAreas: Boolean read FIsMergeAreas write FIsMergeAreas;
    {  Object of this type should be ignored for land/sea calculation. }
    property IsIgnoreSeaLand: Boolean read FIsIgnoreSeaLand write FIsIgnoreSeaLand;
    { Object of this typoe should be ignored (not exported for renderng, routing,
      location indexing or other services). }
    property IsIgnore: Boolean read FIsIgnore write FIsIgnore;

    { Number of expected lanes (default: 1) }
    property Lanes: Byte read FLanes write FLanes;
    { Number of expected one-way lanes (default: 1) }
    property OnewayLanes: Byte read FOnewayLanes write FOnewayLanes;
    { In what order will objects drawn on map (for sorting) }
    property ZOrder: Byte read FZOrder write FZOrder;
    { Set of idents that server as categorizing groups }
    property Groups: TStringList read FGroups;
    { Map of descriptions for given language codes }
    property Descriptions: TDescriptionsMap read FDescriptions;
    { Feature index for feature type, -1 if not defined }
    property FeatureIndexMap: TFeatureTypeIndexArray read FFeatureIndexMap;
  end;

  {$ifdef FPC}
  TTypeInfoList = specialize TFPGList<TTypeInfo>;
  {$else}
  TTypeInfoList = TList<TTypeInfo>;
  {$endif}
  //TNameToTypeMap = specialize TFPGMap<string, TTypeInfo>;

  { The TypeConfig class holds information about object types
    defined by a database instance. }
  { TTypeConfig }

  TTypeConfig = class(TObject)
  private
    FTagRegistry: TTagRegistry; // Tags

    // Types

    FTypes: TTypeInfoList;
    FNodeTypes: TTypeInfoList;
    FWayTypes: TTypeInfoList;
    FAreaTypes: TTypeInfoList;

    FNodeTypeIdBytes: Byte;
    FWayTypeIdBytes: Byte;
    FAreaTypeIdBits: Byte;
    FAreaTypeIdBytes: Byte;

    FNameToTypeMap: TSimpleStringHash;

    // Features

    FFeatures: TFeatureList;
    // pair of Name and Index in FFeatures
    //FNameToFeatureMap: TNameToFeatureMap;
    FFeatureIndexMap: TFeatureTypeIndexArray;

    FFeatureName: TFeature;
    FFeatureAltName: TFeature;
    FFeatureRef: TFeature;
    FFeatureLocation: TFeature;
    FFeatureAddress: TFeature;
    FFeaturePostalCode: TFeature;
    FFeatureWebsite: TFeature;
    FFeaturePhone: TFeature;
    FFeatureAccess: TFeature;
    FFeatureAccessRestricted: TFeature;
    FFeatureLayer: TFeature;
    FFeatureWidth: TFeature;
    FFeatureMaxSpeed: TFeature;
    FFeatureGrade: TFeature;
    FFeatureAdminLevel: TFeature;
    FFeatureBridge: TFeature;
    FFeatureTunnel: TFeature;
    FFeatureEmbankment: TFeature;
    FFeatureRoundabout: TFeature;
    FFeatureLanes: TFeature;

    function CreateRegisterFeature(AFeatureType: TFeatureType): TFeature;
  protected
    // Internal use (only available during preprocessing)
    FTagArea: TTagId;
    FTagNatural: TTagId;
    FTagDataPolygon: TTagId;
    FTagType: TTagId;
    FTagRestriction: TTagId;
    FTagJunction: TTagId;

    FTypeInfoIgnore: TTypeInfo;
    FTypeInfoRoute: TTypeInfo;
    FTypeInfoTileLand: TTypeInfo;         // Internal type for ground tiles of type "land"
    FTypeInfoTileSea: TTypeInfo;          // Internal type for ground tiles of type "sea"
    FTypeInfoTileCoast: TTypeInfo;        // Internal type for ground tiles of type "coast"
    FTypeInfoTileUnknown: TTypeInfo;      // Internal type for ground tiles of type "unknown"
    FTypeInfoCoastline: TTypeInfo;        // Internal type for coastlines
    FTypeInfoOSMTileBorder: TTypeInfo;    // Internal type for OSM tile borders
    FTypeInfoOSMSubTileBorder: TTypeInfo; // Internal type for OSM tile borders

  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    { Methods for dealing with tags }
    function GetTagId(const AName: string): TTagId;

    { Methods for dealing with mappings for surfaces and surface grades. }
    procedure RegisterSurfaceToGradeMapping(const ASurface: string; AGrade: Integer);
    procedure RegisterMaxSpeedAlias(const AAlias: string; AMaxSpeed: Byte);

    { Add type. If type already exists, return that type info  }
    function RegisterType(const ATypeInfo: TTypeInfo): TTypeInfo;

    { Returns the number of types available. The index of a type is guaranteed
      to be in the interval [0..GetTypeCount()] }
    function GetTypeCount(): Integer;
    { Return the highest used type id }
    function GetMaxTypeId(): TTypeId;

    { Returns the type definition for the given type id }
    function GetTypeInfo(AId: TTypeId): TTypeInfo; overload;
    { Returns the node type definition for the given type id }
    function GetNodeTypeInfo(AId: TTypeId): TTypeInfo;
    { Returns the way type definition for the given type id }
    function GetWayTypeInfo(AId: TTypeId): TTypeInfo;
    { Returns the area type definition for the given type id }
    function GetAreaTypeInfo(AId: TTypeId): TTypeInfo;

    { Returns the type definition for the given type name. If there is no
      type definition for the given name and invalid reference is returned. }
    function GetTypeInfo(const AName: string): TTypeInfo; overload;
    { Return a node type (or an invalid reference if no type got detected)
      based on the given map of tag and tag values. The method iterates over all
      node type definitions, evaluates their conditions and returns the first matching
      type. }
    function GetNodeType(const ATagMap: TTagMap): TTypeInfo;

    { Return a way/area type (or an invalid reference if no type got detected)
      based on the given map of tag and tag values. The method iterates over all
      way/area type definitions, evaluates their conditions and returns the first matching
      type. }
    function GetWayAreaType(const ATagMap: TTagMap;
                            out AWayType: TTypeInfo;
                            out AAreaType: TTypeInfo): Boolean;

    { Return a relation type (or an invalid reference if no type got detected)
      based on the given map of tag and tag values. The method iterates over all
      relation type definitions, evaluates their conditions and returns the first matching
      type. }
    function GetRelationType(const ATagMap: TTagMap): TTypeInfo;

    procedure RegisterFeature(AFeature: TFeature);
    { Return the feature with the given name or an invalid reference
      if no feature with the given name is registered. }
    function GetFeatureByName(const AName: string): TFeature;
    function GetFeature(AFeatureType: TFeatureType): TFeature;

    { Methods for loading/storing of type information from/to files. }

    { Loads the type configuration from the given *.ost file.
      Note: Make sure that you load from a OST file only onto a freshly initialized
      TypeConfig instance.
      AFilename - Full filename including path of the OST file}
    function LoadFromOSTFile(const AFilename: string): Boolean;
    { Loads the type configuration from the given binary data file.
      Note: Make sure that you load from file only onto a freshly initialized
      TypeConfig instance.
      ADirectory - Full path excluding the actual filename of the data file
      (filename is always "types.dat") }
    function LoadFromDataFile(const ADirectory: string): Boolean;
    { Store the part of the TypeConfig information to a data file,
      which is necessary to review later on when reading and
      evaluation an import. }
    function StoreToDataFile(const ADirectory: string): Boolean;

    { tags }
    property TagRegistry: TTagRegistry read FTagRegistry;

    { array of all types available }
    property Types: TTypeInfoList read FTypes;
    { array of the (ignore=false) node types available }
    property NodeTypes: TTypeInfoList read FNodeTypes;
    property NodeTypeIdBytes: Byte read FNodeTypeIdBytes;
    { array of the (ignore=false) way types available }
    property WayTypes: TTypeInfoList read FWayTypes;
    property WayTypeIdBytes: Byte read FWayTypeIdBytes;
    { array of the (ignore=false) area types available }
    property AreaTypes: TTypeInfoList read FAreaTypes;
    property AreaTypeIdBytes: Byte read FAreaTypeIdBytes;

    { A feature is a attribute set based on parsed tags. Features can get assigned to a type. }
    property Features: TFeatureList read FFeatures;
  end;

  { Custom data structure to efficiently handle a set of TypeInfoRef.
    All operations on the set are O(1) using the fact, that TypeInfo internally
    have a continuously running index variable (Set may be slower if the
    internal array was not preinitialized to it maximum size by passing a
    TypeConfig or another TypeInfoSet in the constructor }

  { TTypeInfoSet }

  TTypeInfoSet = object
    Types: array of TTypeInfo;

    procedure Init(ATypeConfig: TTypeConfig); overload;
    procedure Init(ATypes: TTypeInfoList); overload;
    procedure Init(const AOther: TTypeInfoSet); overload;

    procedure Adapt(ATypeConfig: TTypeConfig);
    procedure Clear();

    procedure Add(AType: TTypeInfo); overload;
    procedure Add(ATypes: TTypeInfoList); overload;
    procedure Add(const AOther: TTypeInfoSet); overload;

    procedure Remove(AType: TTypeInfo); overload;
    procedure Remove(const AOther: TTypeInfoSet); overload;

    procedure Intersection(const AOther: TTypeInfoSet);

    function IsSet(AType: TTypeInfo): Boolean;

    function Intersects(const AOther: TTypeInfoSet): Boolean;
  end;


function FeatureInstance(AFeature: TFeature;
                         ATypeInfo: TTypeInfo;
                         AFeatureBit: Integer;
                         AIndex: Integer): TFeatureInfo;

function GlobalFeatureValueStorage(): TFeatureValueStorage;

implementation

uses OsMapObjFeatures;

const
  FILE_FORMAT_VERSION = 19;
  FILE_TYPES_DAT = 'types.dat';
  MIN_FORMAT_VERSION = FILE_FORMAT_VERSION;
  MAX_FORMAT_VERSION = FILE_FORMAT_VERSION;

var
  FeatureValueStorage: TFeatureValueStorage;

function GlobalFeatureValueStorage(): TFeatureValueStorage;
begin
  if not Assigned(FeatureValueStorage) then
    FeatureValueStorage := TFeatureValueStorage.Create();

  Result := FeatureValueStorage;
end;

function FeatureInstance(AFeature: TFeature;
                         ATypeInfo: TTypeInfo;
                         AFeatureBit: Integer;
                         AIndex: Integer): TFeatureInfo;
begin
  Result.Feature := AFeature;
  Result.TypeInfo := ATypeInfo;
  Result.FeatureBit := AFeatureBit;
  Result.Index := AIndex;
end;

{ TFeatureValueBuffer }

procedure TFeatureValueBuffer.DeleteData();
begin
  FFeatureMask := 0;
  SetLength(FFeatureValueList, 0);
  FTypeInfo := nil;
end;

procedure TFeatureValueBuffer.AllocateValueBufferLazy();
begin
  if (Length(FFeatureValueList) = 0) and Assigned(TypeInfo) and (TypeInfo.HasFeatures()) then
    SetLength(FFeatureValueList, TypeInfo.FeatureValueCount);
end;

procedure TFeatureValueBuffer.SetFeatureBit(AIndex: Integer);
var
  BitIndex: Integer;
begin
  BitIndex := GetFeatureInfo(AIndex).FeatureBit;
  //Assert(BitIndex >= 0);
  Assert(BitIndex < Sizeof(FFeatureMask));
  FFeatureMask := FFeatureMask or (1 shl BitIndex );
end;

{function TFeatureValueBuffer.GetValueAndAllocateBuffer(AIndex: Integer): TFeatureValue;
var
  n: Integer;
begin
  AllocateValueBufferLazy();
  n := TypeInfo.GetFeature(AIndex).Offset;
  if n < FFeatureValueList.Count then
    Result := FFeatureValueList.Items[n]
  else
    Result := nil;
end; }

procedure TFeatureValueBuffer.Assign(const AValue: TFeatureValueBuffer);
var
  i: Integer;
begin
  DeleteData();
  if Assigned(AValue.TypeInfo) then
  begin
    SetType(AValue.TypeInfo);
    for i := 0 to AValue.GetFeatureCount()-1 do
    begin
      if AValue.HasFeatureValue(i) then
      begin
        if AValue.GetFeatureInfo(i).Feature.HasValue() then
        begin
          SetValue(i, AValue.GetValue(i));
        end
        else
        begin
          SetFeatureBit(i);
        end;
      end;
    end;
  end;
end;

procedure TFeatureValueBuffer.CopyMissingValue(const AValue: TFeatureValueBuffer);
var
  i, n: Integer;
  //sName: string;
  ft: TFeatureType;
begin
  for i := 0 to AValue.GetFeatureCount()-1 do
  begin
    // Feature set?
    if not AValue.HasFeatureValue(i) then
      Continue;

    ft := AValue.GetFeature(i).FeatureType;
    // Does our type has this feature, too?
    if not TypeInfo.FindFeature(ft, n) then
      Continue;

    // We do not overwrite existing feature values
    if HasFeatureValue(i) then
      Continue;

    // Copy feature value
    SetValue(n, AValue.GetValue(i));
  end;
end;

procedure TFeatureValueBuffer.ClearFeatureValues();
var
  i: Integer;
begin
  for i := 0 to Length(FFeatureValueList)-1 do
  begin
    FFeatureValueList[i] := -1;
  end;
end;

procedure TFeatureValueBuffer.SetType(const AType: TTypeInfo);
begin
  if TypeInfo <> AType then
  begin
    DeleteData();
    FTypeInfo := AType;
    FeatureValueStorage := GlobalFeatureValueStorage();
  end;
end;

function TFeatureValueBuffer.GetType(): TTypeInfo;
begin
  Result := FTypeInfo;
end;

function TFeatureValueBuffer.GetFeatureCount(): Integer;
begin
  if Assigned(FTypeInfo) then
    Result := FTypeInfo.FeatureCount
  else
    Result := 0;
end;

function TFeatureValueBuffer.GetFeatureInfo(AIndex: Integer): TFeatureInfo;
begin
  Assert(Assigned(FTypeInfo));
  Assert(AIndex < FTypeInfo.FeatureCount);
  Result := FTypeInfo.FeatureInfos[AIndex];
end;

function TFeatureValueBuffer.GetFeature(AIndex: Integer): TFeature;
begin
  Assert(Assigned(FTypeInfo));
  Result := FTypeInfo.FeatureInfos[AIndex].Feature;
end;

function TFeatureValueBuffer.HasFeatureValue(AIndex: Integer): Boolean;
var
  BitIndex: Integer;
begin
  BitIndex := TypeInfo.FeatureInfos[AIndex].FeatureBit;
  Assert(BitIndex < Sizeof(FFeatureMask));
  Result := ((FFeatureMask and (1 shl BitIndex )) <> 0)
end;

function TFeatureValueBuffer.GetValue(AIndex: Integer): TFeatureValue;
var
  n: Integer;
begin
  n := TypeInfo.FeatureInfos[AIndex].Index;
  if (n < Length(FFeatureValueList)) then
    // ! assigned by reference
    Result := FeatureValueStorage.GetStr(FFeatureValueList[n])
  else
    Result := '';
end;

procedure TFeatureValueBuffer.SetValue(AIndex: Integer;
  const AValue: TFeatureValue);
var
  n: Integer;
begin
  SetFeatureBit(AIndex);
  //if TypeInfo.Features[AIndex].Feature.HasValue() then
  begin
    AllocateValueBufferLazy();
    n := TypeInfo.FeatureInfos[AIndex].Index;
    Assert(n < Length(FFeatureValueList));
    // ! assigned by reference
    FFeatureValueList[n] := FeatureValueStorage.AddStr(AValue);
  end;
end;

procedure TFeatureValueBuffer.SetFeatureValue(AFeatureType: TFeatureType;
  const AValue: TFeatureValue);
var
  n: Integer;
begin
  n := TypeInfo.FeatureIndexMap[AFeatureType];
  if n <> -1 then
  begin
    SetValue(n, AValue);
  end;
end;

function TFeatureValueBuffer.GetFeatureValue(AFeatureType: TFeatureType): string;
var
  n: Integer;
begin
  n := TypeInfo.FeatureIndexMap[AFeatureType];
  if n <> -1 then
    Result := GetValue(n)
  else
    Result := '';
end;

function TFeatureValueBuffer.GetFeatureValueId(AFeatureType: TFeatureType): TFeatureValueId;
var
  n, nn: Integer;
begin
  n := TypeInfo.FeatureIndexMap[AFeatureType];
  if n <> -1 then
  begin
    nn := TypeInfo.FeatureInfos[n].Index;
    if (nn < Length(FFeatureValueList)) then
      Result := FFeatureValueList[nn]
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TFeatureValueBuffer.GetFeatureLabel(AFeatureType: TFeatureType): string;
var
  n: Integer;
begin
  n := TypeInfo.FeatureIndexMap[AFeatureType];
  if n <> -1 then
    //Result := GetValue(n)
    Result := TypeInfo.FeatureInfos[n].Feature.GetLabel(GetValue(n))
  else
    Result := '';
end;

procedure TFeatureValueBuffer.FreeValue(AIndex: Integer);
begin
  Assert(AIndex < Length(FFeatureValueList));
  FFeatureValueList[AIndex] := -1;
end;

procedure TFeatureValueBuffer.Parse(AErrorReporter: TTagErrorReporter;
  ATagRegistry: TTagRegistry; const AObject: TObjectOSMRef; const ATags: TTagMap);
begin
  Assert(False, 'not implemented');
  { TODO : implement }
end;

procedure TFeatureValueBuffer.Read(AScanner: TFileScanner);
var
  Flag1, Flag2: Boolean;
begin
  Read(AScanner, Flag1, Flag2, 0);
end;

procedure TFeatureValueBuffer.Read(AScanner: TFileScanner;
  out ASpecialFlag1: Boolean);
var
  Flag2: Boolean;
begin
  Read(AScanner, ASpecialFlag1, Flag2, 1);
end;

procedure TFeatureValueBuffer.Read(AScanner: TFileScanner;
  out ASpecialFlag1: Boolean; out ASpecialFlag2: Boolean; AFlagsCount: Integer);
var
  i, n: Integer;
  TmpValue: TFeatureValue;
  TmpByte: Byte;
begin
  //AScanner.Read(FFeatureMask[0], TypeInfo.FeatureMaskBytes);
  n := TypeInfo.FeatureMaskBytes;
  FFeatureMask := 0;
  for i := 0 to n-1 do
  begin
    AScanner.Read(TmpByte);
    FFeatureMask := FFeatureMask or (TmpByte shl (8 * i));
  end;

  if AFlagsCount > 0 then
  begin
    if BitsToBytes(TypeInfo.FeatureCount) = BitsToBytes(TypeInfo.FeatureCount + AFlagsCount) then
    begin
      ASpecialFlag1 := GetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 1);
      if AFlagsCount >= 2 then
        ASpecialFlag2 := GetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 2);
    end
    else
    begin
      AScanner.Read(TmpByte);
      ASpecialFlag1 := ((TmpByte and $80) <> 0);
      if AFlagsCount >= 2 then
        ASpecialFlag2 := ((TmpByte and $40) <> 0);
    end;
  end;

  for i := 0 to TypeInfo.FeatureCount - 1 do
  begin
    // if bit in FFeatureMask is set
    if HasFeatureValue(i) then
    begin
      TypeInfo.FeatureInfos[i].Feature.Read(AScanner, TmpValue);
      SetValue(i, TmpValue);
    end;
  end;
end;

procedure TFeatureValueBuffer.Write(AWriter: TFileWriter);
begin
  Write(AWriter, False, False, 0);
end;

procedure TFeatureValueBuffer.Write(AWriter: TFileWriter; ASpecialFlag1: Boolean);
begin
  Write(AWriter, ASpecialFlag1, False, 1);
end;

procedure TFeatureValueBuffer.Write(AWriter: TFileWriter;
  ASpecialFlag1: Boolean; ASpecialFlag2: Boolean; AFlagsCount: Integer);
var
  i: Integer;
  TmpValue: TFeatureValue;
  TmpByte: Byte;
  IsAddTmpByte: Boolean;
begin
  IsAddTmpByte := False;
  TmpByte := 0;
  if AFlagsCount > 0 then
  begin
    if BitsToBytes(TypeInfo.FeatureCount) = BitsToBytes(TypeInfo.FeatureCount + AFlagsCount) then
    begin
      if ASpecialFlag1 then
        SetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 1, True)
      else
        SetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 1, False);

      if AFlagsCount >= 2 then
      begin
        if ASpecialFlag2 then
          SetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 2, True)
        else
          SetBit(FFeatureMask, (TypeInfo.FeatureMaskBytes * 8) - 2, False);
      end;
    end
    else
    begin
      if ASpecialFlag1 then
        TmpByte := TmpByte or $80;

      if (AFlagsCount >= 2) and (ASpecialFlag2) then
        TmpByte := TmpByte or $40;

      IsAddTmpByte := True;
    end;
  end;

  for i := 0 to TypeInfo.FeatureMaskBytes-1 do
  begin
    AWriter.Write(Byte(FFeatureMask shr (i * 8)));
  end;

  if IsAddTmpByte then
    AWriter.Write(TmpByte);

  for i := 0 to TypeInfo.FeatureCount - 1 do
  begin
    if HasFeatureValue(i) then
    begin
      TmpValue := GetValue(i);
      TypeInfo.FeatureInfos[i].Feature.Write(AWriter, TmpValue);
    end;
  end;
end;

function TFeatureValueBuffer.FindValue(AValueType: TClass): TFeatureValue;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to TypeInfo.FeatureCount-1 do
  begin
    if HasFeatureValue(i) then
    begin
      if TypeInfo.FeatureInfos[i].Feature.ClassType = AValueType then
      begin
        Result := GetValue(i);
      end;
    end;
  end;
end;

{ TTypeCondition }

constructor TTypeCondition.Create(ATypes: TConditionTypes;
  ACondition: TTagCondition);
begin
  inherited Create();
  Types := ATypes;
  Condition := ACondition;
end;

{ TFeature }

procedure TFeature.RegisterLabel(AIndex: Integer; ALabelName: string);
var
  n: Integer;
begin
  Assert(not FLabels.FindValue(ALabelName, n));
  FLabels.Add(ALabelName, AIndex);
  FHasLabel := True;
end;

procedure TFeature.RegisterFlag(AIndex: Integer; AFlagName: string);
var
  n: Integer;
begin
  Assert(FFlags.FindValue(AFlagName, n));
  FFlags.Add(AFlagName, AIndex);
  FHasFlag := True;
end;

constructor TFeature.Create(AFeatureType: TFeatureType);
begin
  inherited Create();
  Assert(AFeatureType <> ftNone);
  FFeatureType := AFeatureType;
  FLabels.Init(8);
  FFlags.Init(8);
  FDescriptions := TDescriptionsMap.Create();
  FDescriptions.Sorted := True;

  case FFeatureType of
    ftName,
    ftNameAlt,
    ftRef,
    ftAddress:
    begin
      RegisterLabel(0, 'name');
    end;
  end;
end;

destructor TFeature.Destroy;
begin
  FreeAndNil(FDescriptions);

  FFlags.Clear();
  FLabels.Clear();

  inherited Destroy;
end;

procedure TFeature.Initialize(ATagRegistry: TTagRegistry);
begin
  case FFeatureType of
    ftName,
    ftNameAlt,
    ftRef,
    ftLocation,
    ftAddress,
    ftAccess,
    ftAccessRestricted,
    ftLayer,
    ftWidth,
    ftMaxSpeed,
    ftGrade,
    ftAdminLevel,
    ftPostalCode,
    ftBridge,
    ftTunnel,
    ftEmbankment,
    ftRoundabout,
    ftEle,
    ftDestination,
    ftBuilding,
    ftWebsite,
    ftPhone,
    ftIsIn,
    ftConstructionYear,
    ftSideway,
    ftLanes:
    begin

    end;
  end;
end;

procedure TFeature.AddDescription(ALanguageCode, ADescription: string);
begin
  if not Assigned(FDescriptions) then
  begin
    FDescriptions := TDescriptionsMap.Create();
    FDescriptions.Sorted := True;
  end;
  FDescriptions.Values[ALanguageCode] := ADescription;
end;

function TFeature.GetName(): string;
begin
  Result := FeatureNames[FFeatureType];
end;

function TFeature.GetLabel(const AValue: string): string;
begin
  case FFeatureType of
    ftName:             Result := AValue;
    ftNameAlt:          Result := AValue;
    ftRef:              Result := AValue;
    ftLocation:         Result := AValue;
    ftAddress:          Result := AValue;
    ftPostalCode:       Result := AValue;
    ftEle:              Result := AValue + 'm';
    ftDestination:      Result := AValue;
    ftWebsite:          Result := AValue;
    ftPhone:            Result := AValue;
    ftConstructionYear:
    begin
      Result := AValue;
      //Result := <StartYear>-<EndYear>
    end;
    ftLanes:
    begin
      Result := AValue;
      {if HasSinjleLane then
        Result := '1'
      else
        Result := ForwardLanes + ' ' + BackwardLanes; }
    end;
  else
    Assert(False);
    Result := '';
  end;
end;

function TFeature.IsFlagSet(const AValue: string; ABitIndex: Integer): Boolean;
var
  Bits: Byte;
begin
  if ABitIndex < Sizeof(Bits) then
  begin
    Bits := StrToIntDef(AValue, 0);
    Result := ((Bits and (1 shl ABitIndex)) <> 0);
  end
  else
    Result := False;
end;

function TFeature.GetFeatureBitCount(): Integer;
begin
  Result := 0;
end;

function TFeature.HasValue(): Boolean;
begin
  Result := True;
end;

function TFeature.HasLabel(): Boolean;
begin
  Result := FHasLabel;
end;

function TFeature.HasFlags(): Boolean;
begin
  Result := FHasFlag;
end;

function TFeature.ReadLabelIndex(const ALabelName: string; var AIndex: Integer): Boolean;
begin
  Result := FLabels.FindValue(ALabelName, AIndex);
end;

function TFeature.ReadFlagIndex(const AFlagName: string; var AIndex: Integer): Boolean;
begin
  Result := FFlags.FindValue(AFlagName, AIndex);
end;

function TFeature.GetDescription(const ALanguageCode: string): string;
var
  n: Integer;
begin
  if Assigned(FDescriptions) and FDescriptions.Find(ALanguageCode, n) then
    Result := FDescriptions.ValueFromIndex[n]
  else
    Result := '';
end;

procedure TFeature.Parse(AReporter: TTagErrorReporter;
  ATagRegistry: TTagRegistry; const AFeatureInfo: TFeatureInfo;
  const AObjectOSMRef: TObjectOSMRef; const ATagMap: TTagMap;
  var ABuffer: TFeatureValueBuffer);
begin
  { TODO : implement }
end;

procedure TFeature.Read(AScanner: TFileScanner; out AValue: string);
var
  TmpByte: Byte;
  Int1, Int2: Integer;
  s: string;
begin
  case FFeatureType of
    ftName,
    ftNameAlt,
    ftRef,
    ftLocation,
    ftAddress,
    ftPostalCode,
    ftDestination,
    ftWebsite,
    ftPhone,
    ftIsIn:
    begin
      // string
      AScanner.Read(AValue);
    end;

    ftAccess,
    ftAccessRestricted,
    ftLayer,
    ftWidth,
    ftMaxSpeed,
    ftGrade,
    ftSideway:
    begin
      // byte
      AScanner.Read(TmpByte);
      AValue := IntToStr(TmpByte);
    end;

    ftAdminLevel:
    begin
      AScanner.Read(TmpByte);
      AScanner.Read(s);
      AValue := IntToStr(TmpByte) + ' ' + s;
    end;

    //ftBridge,
    //ftTunnel,
    //ftEmbankment,
    //ftRoundabout,
    //ftBuilding,

    ftEle:
    begin
      // integer
      AScanner.Read(Int1);
      AValue := IntToStr(Int1);
    end;

    ftConstructionYear:
    begin
      AScanner.Read(Int1);
      AScanner.Read(Int2);
      AValue := IntToStr(Int1)+'-'+IntToStr(Int2);
    end;

    ftLanes:
    begin
      AScanner.Read(TmpByte);
      AValue := IntToStr(TmpByte);
      if (TmpByte and $01) <> 0 then
      begin
        AScanner.Read(s);
        AValue := AValue + '|' + s;
        AScanner.Read(s);
        AValue := AValue + '|' + s;
        AScanner.Read(s);
        AValue := AValue + '|' + s;
        AScanner.Read(s);
        AValue := AValue + '|' + s;
      end;
    end;
  else
    AValue := '';
  end;
end;

procedure TFeature.Write(AWriter: TFileWriter; const AValue: string);
var
  TmpByte: Byte;
  Int1, Int2: Integer;
  s, ss: string;
begin
  case FFeatureType of
    ftName,
    ftNameAlt,
    ftRef,
    ftLocation,
    ftAddress,
    ftPostalCode,
    ftDestination,
    ftWebsite,
    ftPhone,
    ftIsIn:
    begin
      // string
      AWriter.Write(AValue);
    end;

    ftAccess,
    ftAccessRestricted,
    ftLayer,
    ftWidth,
    ftMaxSpeed,
    ftGrade,
    ftSideway:
    begin
      // byte
      TmpByte := StrToIntDef(AValue, 0);
      AWriter.Write(TmpByte);
    end;

    ftAdminLevel:
    begin
      ss := Copy(AValue, 1, MaxInt);
      s := Fetch(ss, ' ');
      TmpByte := StrToIntDef(s, 0);
      AWriter.Write(TmpByte);
      AWriter.Write(ss);
    end;

    //ftBridge,
    //ftTunnel,
    //ftEmbankment,
    //ftRoundabout,
    //ftBuilding,

    ftEle:
    begin
      // integer
      Int1 := StrToIntDef(AValue, 0);
      AWriter.Write(Int1);
    end;

    ftConstructionYear:
    begin
      ss := Copy(AValue, 1, MaxInt);
      s := Fetch(ss, '-');
      Int1 := StrToIntDef(s, 0);
      Int2 := StrToIntDef(ss, 0);
      AWriter.Write(Int1);
      AWriter.Write(Int2);
    end;

    ftLanes:
    begin
      ss := Copy(AValue, 1, MaxInt);
      s := Fetch(ss, '|');
      TmpByte := StrToIntDef(s, 0);
      AWriter.Write(TmpByte);

      if (TmpByte and $01) <> 0 then
      begin
        s := Fetch(ss, '|');
        AWriter.Write(s);
        s := Fetch(ss, '|');
        AWriter.Write(s);
        s := Fetch(ss, '|');
        AWriter.Write(s);
        AWriter.Write(ss);
      end;
    end;
  else
    Assert(False);
  end;
end;

{ TTypeInfo }

function TTypeInfo.GetCanRoute(): Boolean;
begin
  Result := CanRouteFoot or CanRouteBicycle or CanRouteCar;
end;

function TTypeInfo.GetCondition(AIndex: Integer): TTypeCondition;
begin
  Assert(AIndex < FConditions.Count);
  Result := FConditions[AIndex];
end;

function TTypeInfo.GetConditionCount(): Integer;
begin
  Result := FConditions.Count;
end;

function TTypeInfo.GetFeatureCount(): Integer;
begin
  Result := Length(FFeatureInfoArray);
end;

constructor TTypeInfo.Create(const AName: string);
var
  ft: TFeatureType;
begin
  inherited Create();
  FConditions := TTypeConditionList.Create();
  //FNameToFeatureMap := TNameToFeatureMap.Create();
  //FNameToFeatureMap.Sorted := True;
  FGroups := TStringList.Create();
  FDescriptions := TDescriptionsMap.Create();
  FDescriptions.Sorted := True;

  for ft := Low(TFeatureType) to High(TFeatureType) do
    FFeatureIndexMap[ft] := -1;

  FLanes := 1;
  FOnewayLanes := 1;
  FTypeName := AName;
end;

destructor TTypeInfo.Destroy();
begin
  FreeAndNil(FDescriptions);
  FreeAndNil(FGroups);
  //FreeAndNil(FNameToFeatureMap);
  FreeAndNil(FConditions);
  inherited Destroy();
end;

function TTypeInfo.AddCondition(ATypes: TConditionTypes; ACondition: TTagCondition): TTypeInfo;
var
  TmpItem: TTypeCondition;
begin
  FCanBeNode     := FCanBeNode or (ctNode in ATypes);
  FCanBeWay      := FCanBeWay or (ctWay in ATypes);
  FCanBeArea     := FCanBeArea or (ctArea in ATypes);
  FCanBeRelation := FCanBeRelation or (ctRelation in ATypes);

  TmpItem := TTypeCondition.Create(ATypes, ACondition);
  FConditions.Add(TmpItem);
  Result := Self;
end;

function TTypeInfo.AddFeature(AFeature: TFeature): TTypeInfo;
var
  n: Integer;
  FeatureBitIndex, FeatureBitCount: Integer;
begin
  Assert(Assigned(AFeature));
  Assert(FFeatureIndexMap[AFeature.FeatureType] = -1);

  FeatureBitIndex := 0;

  n := Length(FFeatureInfoArray) - 1; // last item index
  if (n >= 0) then
  begin
    FeatureBitIndex := FFeatureInfoArray[n].FeatureBit + FFeatureInfoArray[n].Feature.GetFeatureBitCount() + 1;
  end;

  Inc(n);
  SetLength(FFeatureInfoArray, n+1);
  FFeatureInfoArray[n].Feature := AFeature;
  FFeatureInfoArray[n].TypeInfo := Self;
  FFeatureInfoArray[n].FeatureBit := FeatureBitIndex;
  FFeatureInfoArray[n].Index := n;
  //FFeatures[n].Offset := Offset;
  FFeatureIndexMap[AFeature.FeatureType] := n;

  FeatureBitCount := FFeatureInfoArray[n].FeatureBit + AFeature.GetFeatureBitCount() + 1;

  FFeatureMaskBytes := BitsToBytes(FeatureBitCount);
  FSpecialFeatureMaskBytes := BitsToBytes(FeatureBitCount + 1);

  //FFeatureValueBufferSize := Offset + AFeature.GetValueSize();
  Inc(FFeatureValueCount);

  Result := Self;
end;

function TTypeInfo.AddGroup(const AGroupName: string): TTypeInfo;
begin
  FGroups.Add(AGroupName);
  Result := Self;
end;

function TTypeInfo.AddDescription(const ALanguageCode: string;
  const ADescription: string): TTypeInfo;
begin
  FDescriptions.Sorted := True;
  FDescriptions.Values[ALanguageCode] := ADescription;
  Result := Self;
end;

function TTypeInfo.HasFeatures(): Boolean;
begin
  Result := (Length(FFeatureInfoArray) <> 0);
end;

{function TTypeInfo.HasFeature(const AFeatureName: string): Boolean;
var
  n: Integer;
begin
  //FNameToFeatureMap.Sorted := True;
  //Result := FNameToFeatureMap.Find(AFeatureName, n);
end; }

function TTypeInfo.HasFeature(AFeatureType: TFeatureType): Boolean;
begin
  Result := (FFeatureIndexMap[AFeatureType] <> -1)
end;

function TTypeInfo.FindFeature(AFeatureType: TFeatureType; var AIndex: Integer): Boolean;
begin
  AIndex := FFeatureIndexMap[AFeatureType];
  Result := (AIndex <> -1);
end;

function TTypeInfo.GetFeature(AIndex: Integer): TFeature;
begin
  Result := FFeatureInfoArray[AIndex].Feature;
end;

function TTypeInfo.GetFeatureInfo(AIndex: Integer): TFeatureInfo;
begin
  Result := FFeatureInfoArray[AIndex];
end;

function TTypeInfo.GetDefaultAccess(): Byte;
begin
  Result := GetDefaultRouteAccess(CanRouteFoot, CanRouteBicycle, CanRouteCar);
end;

function TTypeInfo.GetDescription(const ALanguageCode: string): string;
var
  n: Integer;
begin
  FDescriptions.Sorted := True;
  if FDescriptions.Find(ALanguageCode, n) then
    Result := FDescriptions.ValueFromIndex[n]
  else
    Result := '';
end;

function TTypeInfo.IsHasConditions(): Boolean;
begin
  Result := (FConditions.Count <> 0);
end;

function TTypeInfo.CanRouteBy(AVehicle: TVehicleType): Boolean;
begin
  case AVehicle of
    vehicleFoot:    Result := CanRouteFoot;
    vehicleBicycle: Result := CanRouteBicycle;
    vehicleCar:     Result := CanRouteCar;
  else
    Result := False;
  end;
end;

function TTypeInfo.IsInGroup(const AName: string): Boolean;
var
  n: Integer;
begin
  Result := FGroups.Find(AName, n);
end;

function TTypeInfo.Save(AWriter: TFileWriter): Boolean;
begin
  AWriter.Write(FTypeName);
  AWriter.Write(FCanBeNode);
  AWriter.Write(FCanBeWay);
  AWriter.Write(FCanBeArea);
  AWriter.Write(FCanBeRelation);
  AWriter.Write(FIsPath);
  AWriter.Write(FCanRouteFoot);
  AWriter.Write(FCanRouteBicycle);
  AWriter.Write(FCanRouteCar);
  AWriter.Write(FIndexAsAddress);
  AWriter.Write(FIndexAsLocation);
  AWriter.Write(FIndexAsRegion);
  AWriter.Write(FIndexAsPOI);
  AWriter.Write(FOptimizeLowZoom);
  AWriter.Write(FIsMultipolygon);
  AWriter.Write(FIsPinWay);
  AWriter.Write(FIsMergeAreas);
  AWriter.Write(FIsIgnoreSeaLand);
  AWriter.Write(FIsIgnore);
  AWriter.Write(FLanes);
  AWriter.Write(FOnewayLanes);
  Result := True;
end;

function TTypeInfo.Load(AScanner: TFileScanner): Boolean;
begin
  AScanner.Read(FTypeName);
  AScanner.Read(FCanBeNode);
  AScanner.Read(FCanBeWay);
  AScanner.Read(FCanBeArea);
  AScanner.Read(FCanBeRelation);
  AScanner.Read(FIsPath);
  AScanner.Read(FCanRouteFoot);
  AScanner.Read(FCanRouteBicycle);
  AScanner.Read(FCanRouteCar);
  AScanner.Read(FIndexAsAddress);
  AScanner.Read(FIndexAsLocation);
  AScanner.Read(FIndexAsRegion);
  AScanner.Read(FIndexAsPOI);
  AScanner.Read(FOptimizeLowZoom);
  AScanner.Read(FIsMultipolygon);
  AScanner.Read(FIsPinWay);
  AScanner.Read(FIsMergeAreas);
  AScanner.Read(FIsIgnoreSeaLand);
  AScanner.Read(FIsIgnore);
  AScanner.Read(FLanes);
  AScanner.Read(FOnewayLanes);

  Result := True;
end;

{ TTypeConfig }

function TTypeConfig.CreateRegisterFeature(AFeatureType: TFeatureType): TFeature;
begin
  Result := TFeature.Create(AFeatureType);
  RegisterFeature(Result);
end;

procedure TTypeConfig.AfterConstruction();
var
  ft: TFeatureType;
begin
  inherited AfterConstruction();
  FNodeTypeIdBytes := 1;
  FWayTypeIdBytes := 1;
  FAreaTypeIdBytes := 1;
  FAreaTypeIdBits := 1;

  FTagRegistry := TTagRegistry.Create();
  FFeatures := TFeatureList.Create();

  FTypes := TTypeInfoList.Create();
  FNodeTypes := TTypeInfoList.Create();
  FWayTypes := TTypeInfoList.Create();
  FAreaTypes := TTypeInfoList.Create();

  FNameToTypeMap.Init();
  //FNameToFeatureMap := TNameToFeatureMap.Create();
  for ft := Low(TFeatureType) to High(TFeatureType) do
    FFeatureIndexMap[ft] := -1;

  //_LogDebug('TTypeConfig.AfterConstruction()');
  FFeatureName := CreateRegisterFeature(ftName);
  FFeatureAltName := CreateRegisterFeature(ftNameAlt);
  FFeatureRef := CreateRegisterFeature(ftRef);
  FFeatureLocation := CreateRegisterFeature(ftLocation);
  FFeatureAddress := CreateRegisterFeature(ftAddress);
  FFeatureAccess := CreateRegisterFeature(ftAccess);
  FFeatureAccessRestricted := CreateRegisterFeature(ftAccessRestricted);
  FFeatureLayer := CreateRegisterFeature(ftLayer);
  FFeatureWidth := CreateRegisterFeature(ftWidth);
  FFeatureMaxSpeed := CreateRegisterFeature(ftMaxSpeed);
  FFeatureGrade := CreateRegisterFeature(ftGrade);
  FFeatureAdminLevel := CreateRegisterFeature(ftAdminLevel);
  FFeaturePostalCode := CreateRegisterFeature(ftPostalCode);
  FFeatureWebsite := CreateRegisterFeature(ftWebsite);
  FFeaturePhone := CreateRegisterFeature(ftPhone);
  FFeatureBridge := CreateRegisterFeature(ftBridge);
  FFeatureTunnel := CreateRegisterFeature(ftTunnel);
  FFeatureEmbankment := CreateRegisterFeature(ftEmbankment);
  FFeatureRoundabout := CreateRegisterFeature(ftRoundabout);
  FFeatureLanes := CreateRegisterFeature(ftLanes);

  CreateRegisterFeature(ftEle);
  CreateRegisterFeature(ftDestination);
  CreateRegisterFeature(ftBuilding);
  CreateRegisterFeature(ftIsIn);
  CreateRegisterFeature(ftConstructionYear);
  CreateRegisterFeature(ftSideway);


  // Make sure, that this is always registered first.
  // It assures that id 0 is always reserved for typeIgnore
  FTypeInfoIgnore := TTypeInfo.Create('');
  FTypeInfoIgnore.IsIgnore := True;

  RegisterType(FTypeInfoIgnore);

  // Internal type for showing routes
  FTypeInfoRoute := TTypeInfo.Create('_route');
  FTypeInfoRoute.IsInternal := True;
  FTypeInfoRoute.CanBeWay := True;
  RegisterType(FTypeInfoRoute);

  // --- Internal types for the land/sea/coast tiles building the base layer for map drawing
  FTypeInfoTileLand := TTypeInfo.Create('_tile_land');
  FTypeInfoTileLand.IsInternal := True;
  FTypeInfoTileLand.CanBeArea := True;
  RegisterType(FTypeInfoTileLand);

  FTypeInfoTileSea := TTypeInfo.Create('_tile_sea');
  FTypeInfoTileSea.IsInternal := True;
  FTypeInfoTileSea.CanBeArea := True;
  RegisterType(FTypeInfoTileSea);

  FTypeInfoTileCoast := TTypeInfo.Create('_tile_coast');
  FTypeInfoTileCoast.IsInternal := True;
  FTypeInfoTileCoast.CanBeArea := True;
  RegisterType(FTypeInfoTileCoast);

  FTypeInfoTileUnknown := TTypeInfo.Create('_tile_unknown');
  FTypeInfoTileUnknown.IsInternal := True;
  FTypeInfoTileUnknown.CanBeArea := True;
  RegisterType(FTypeInfoTileUnknown);

  FTypeInfoCoastline := TTypeInfo.Create('_tile_coastline');
  FTypeInfoCoastline.IsInternal := True;
  FTypeInfoCoastline.CanBeWay := True;
  RegisterType(FTypeInfoCoastline);

  FTypeInfoOSMTileBorder := TTypeInfo.Create('_osm_tile_border');
  FTypeInfoOSMTileBorder.IsInternal := True;
  FTypeInfoOSMTileBorder.CanBeWay := True;
  RegisterType(FTypeInfoOSMTileBorder);

  FTypeInfoOSMSubTileBorder := TTypeInfo.Create('_osm_subtile_border');
  FTypeInfoOSMSubTileBorder.IsInternal := True;
  FTypeInfoOSMSubTileBorder.CanBeWay := True;
  RegisterType(FTypeInfoOSMSubTileBorder);

  FTagArea := TagRegistry.GetTagId('area');
  FTagNatural := TagRegistry.GetTagId('natural');
  FTagDataPolygon := TagRegistry.GetTagId('datapolygon');
  FTagType := TagRegistry.GetTagId('type');
  FTagRestriction := TagRegistry.GetTagId('restriction');
  FTagJunction := TagRegistry.GetTagId('junction');

  Assert(FTagArea <> TAG_IGNORE);
  Assert(FTagNatural <> TAG_IGNORE);
  Assert(FTagDataPolygon <> TAG_IGNORE);
  Assert(FTagType <> TAG_IGNORE);
  Assert(FTagRestriction <> TAG_IGNORE);
  Assert(FTagJunction <> TAG_IGNORE);
end;

procedure TTypeConfig.BeforeDestruction();
begin
  { TODO : cleanup }
  //FreeAndNil(FNameToFeatureMap);
  //FreeAndNil(FNameToTypeMap);

  FreeAndNil(FAreaTypes);
  FreeAndNil(FWayTypes);
  FreeAndNil(FNodeTypes);
  FreeAndNil(FTypes);
  FreeAndNil(FFeatures);
  FreeAndNil(FTagRegistry);
  inherited BeforeDestruction();
end;

function TTypeConfig.GetTagId(const AName: string): TTagId;
begin
  Result := FTagRegistry.GetTagId(AName);
end;

procedure TTypeConfig.RegisterSurfaceToGradeMapping(const ASurface: string;
  AGrade: Integer);
begin
  TagRegistry.RegisterSurfaceToGradeMapping(ASurface, AGrade);
end;

procedure TTypeConfig.RegisterMaxSpeedAlias(const AAlias: string;
  AMaxSpeed: Byte);
begin
  TagRegistry.RegisterMaxSpeedAlias(AAlias, AMaxSpeed);
end;

function TTypeConfig.RegisterType(const ATypeInfo: TTypeInfo): TTypeInfo;

{ Add feature to type if it not exists }
procedure _AddFeatureToTypeInfo(AFeature: TFeature);
begin
  if not ATypeInfo.HasFeature(AFeature.FeatureType) then
    ATypeInfo.AddFeature(AFeature);
end;

var
  n: Integer;
begin
  Assert(Assigned(ATypeInfo));
  if FNameToTypeMap.FindValue(ATypeInfo.TypeName, n) then
  begin
    Result := FTypes.Items[n];
    Exit;
  end;

  // All ways have a layer
  if ATypeInfo.CanBeWay then
  begin
    _AddFeatureToTypeInfo(FFeatureLayer);
  end;

  // All that is PATH-like automatically has a number of features,
  // even if it is not routable
  if ATypeInfo.CanBeWay and ATypeInfo.IsPath then
  begin
    _AddFeatureToTypeInfo(FFeatureWidth);
    _AddFeatureToTypeInfo(FFeatureGrade);
    _AddFeatureToTypeInfo(FFeatureBridge);
    _AddFeatureToTypeInfo(FFeatureTunnel);
    _AddFeatureToTypeInfo(FFeatureEmbankment);
    _AddFeatureToTypeInfo(FFeatureRoundabout);
    _AddFeatureToTypeInfo(FFeatureLanes);
  end;

  // Everything routable should have access information and max speed information
  if (ATypeInfo.CanBeArea or ATypeInfo.CanBeWay) and ATypeInfo.CanRoute then
  begin
    _AddFeatureToTypeInfo(FFeatureAccess);
    _AddFeatureToTypeInfo(FFeatureAccessRestricted);
    _AddFeatureToTypeInfo(FFeatureMaxSpeed);
  end;

  // All addressable areas and nodes get the postal code, location and address feature
  if (ATypeInfo.CanBeArea or ATypeInfo.CanBeNode) and ATypeInfo.IndexAsAddress then
  begin
    _AddFeatureToTypeInfo(FFeaturePostalCode);
    _AddFeatureToTypeInfo(FFeatureLocation);
    _AddFeatureToTypeInfo(FFeatureAddress);
  end;

  // All ways with a name have a postal code and a location
  if ATypeInfo.CanBeWay and ATypeInfo.HasFeature(ftName) then
  begin
    _AddFeatureToTypeInfo(FFeaturePostalCode);
  end;

  // Something that has a name and is a POI automatically gets the
  // postal code, location, address, website and phone features, too.
  if ATypeInfo.HasFeature(ftName) and ATypeInfo.IndexAsPOI then
  begin
    _AddFeatureToTypeInfo(FFeaturePostalCode);
    _AddFeatureToTypeInfo(FFeatureLocation);
    _AddFeatureToTypeInfo(FFeatureAddress);
    _AddFeatureToTypeInfo(FFeatureWebsite);
    _AddFeatureToTypeInfo(FFeaturePhone);
  end;

  ATypeInfo.Index := FTypes.Add(ATypeInfo);

  { For all lists, Id=0 reserved for TypeInfoIgnore }
  // node types list
  if ((not ATypeInfo.FIsIgnore)
      and (not ATypeInfo.IsInternal)
      and (ATypeInfo.CanBeNode or ATypeInfo.CanBeWay or ATypeInfo.CanBeArea))
  then
  begin
    ATypeInfo.NodeId := FNodeTypes.Add(ATypeInfo) + 1;
    FNodeTypeIdBytes := BytesNeededToEncodeNumber(ATypeInfo.NodeId);
  end;

  // way types list
  if ATypeInfo.CanBeWay then
  begin
    ATypeInfo.WayId := FWayTypes.Add(ATypeInfo) + 1;
    FWayTypeIdBytes := BytesNeededToEncodeNumber(ATypeInfo.WayId);
  end;

  // area types list
  if ATypeInfo.CanBeArea then
  begin
    ATypeInfo.AreaId := FAreaTypes.Add(ATypeInfo) + 1;
    FAreaTypeIdBytes := BytesNeededToEncodeNumber(ATypeInfo.AreaId);
    FAreaTypeIdBits := BitsNeededToEncodeNumber(ATypeInfo.AreaId);
  end;

  FNameToTypeMap.Add(ATypeInfo.TypeName, ATypeInfo.Index);

  Result := ATypeInfo;
end;

function TTypeConfig.GetTypeCount(): Integer;
begin
  Result := FTypes.Count;
end;

function TTypeConfig.GetMaxTypeId(): TTypeId;
begin
  Result := TTypeId(FTypes.Count);
end;

function TTypeConfig.GetTypeInfo(AId: TTypeId): TTypeInfo;
begin
  // Id is index
  Assert(AId < FTypes.Count);
  Result := FTypes[AId];
end;

function TTypeConfig.GetNodeTypeInfo(AId: TTypeId): TTypeInfo;
begin
  Assert(AId <= FNodeTypes.Count);
  if (AId = ObjTypeIgnore) then
    Result := FTypeInfoIgnore
  else
    Result := FNodeTypes[AId-1];
end;

function TTypeConfig.GetWayTypeInfo(AId: TTypeId): TTypeInfo;
begin
  Assert(AId <= FWayTypes.Count);
  if (AId = ObjTypeIgnore) then
    Result := FTypeInfoIgnore
  else
    Result := FWayTypes[AId-1];
end;

function TTypeConfig.GetAreaTypeInfo(AId: TTypeId): TTypeInfo;
begin
  Assert(AId <= FAreaTypes.Count);
  if (AId = ObjTypeIgnore) then
    Result := FTypeInfoIgnore
  else
    Result := FAreaTypes[AId-1];
end;

function TTypeConfig.GetTypeInfo(const AName: string): TTypeInfo;
var
  n: Integer;
begin
  if FNameToTypeMap.FindValue(AName, n) then
  begin
    Assert(n < FTypes.Count);
    Result := FTypes.Items[n];
  end
  else
  begin
    //Assert(False, 'TTypeConfig.GetTypeInfo() Not found type: ' + AName);
    Result := FTypeInfoIgnore;
  end;
end;

function TTypeConfig.GetNodeType(const ATagMap: TTagMap): TTypeInfo;
var
  i, ii: Integer;
begin
  if ATagMap.Count = 0 then
  begin
    Result := FTypeInfoIgnore;
    Exit;
  end;

  for i := 0 to FTypes.Count-1 do
  begin
    Result := FTypes[i];
    if (not Result.IsHasConditions()) or (not Result.CanBeNode) then
      Continue;

    for ii := 0 to Result.ConditionCount-1 do
    begin
      if not (ctNode in Result.Conditions[ii].Types) then
        Continue;

      if Result.Conditions[ii].Condition.Evaluate(ATagMap) then
        Exit;
    end;
  end;
  Result := FTypeInfoIgnore;
end;

function TTypeConfig.GetWayAreaType(const ATagMap: TTagMap;
  out AWayType: TTypeInfo; out AAreaType: TTypeInfo): Boolean;
var
  i, ii: Integer;
  TmpItem: TTypeInfo;
  TmpCond: TTypeCondition;
begin
  AWayType := FTypeInfoIgnore;
  AAreaType := FTypeInfoIgnore;
  Result := False;

  if ATagMap.Count = 0 then
    Exit;

  for i := 0 to FTypes.Count-1 do
  begin
    TmpItem := FTypes[i];
    if (not ( (TmpItem.CanBeWay or TmpItem.CanBeArea) and TmpItem.IsHasConditions() )) then
      Continue;

    for ii := 0 to TmpItem.ConditionCount-1 do
    begin
      TmpCond := TmpItem.Conditions[ii];
      if not ([ctWay, ctArea] <= TmpCond.Types) then
        Continue;

      if TmpCond.Condition.Evaluate(ATagMap) then
      begin
        if (AWayType = FTypeInfoIgnore) and (ctWay in TmpCond.Types) then
          AWayType := TmpItem;

        if (AAreaType = FTypeInfoIgnore) and (ctArea in TmpCond.Types) then
          AAreaType := TmpItem;

        Result := (AWayType <> FTypeInfoIgnore) and (AAreaType <> FTypeInfoIgnore);
        if Result then
          Exit;
      end;
    end;
  end;
end;

function TTypeConfig.GetRelationType(const ATagMap: TTagMap): TTypeInfo;
var
  i, ii, n: Integer;
  //s: string;
begin
  if ATagMap.Count = 0 then
  begin
    Result := FTypeInfoIgnore;
    Exit;
  end;

  if ATagMap.FindValue(FTagType, n) then
  begin
    if ATagMap.ValueFromIndex[n] = 'multipolygon' then
    begin
      for i := 0 to FTypes.Count-1 do
      begin
        Result := FTypes[i];
        if (not Result.IsHasConditions()) or (not Result.CanBeArea) then
          Continue;

        for ii := 0 to Result.ConditionCount - 1 do
        begin
          if not (ctArea in Result.Conditions[ii].Types) then
            Continue;

          if Result.Conditions[ii].Condition.Evaluate(ATagMap) then
            Exit;
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to FTypes.Count-1 do
    begin
      Result := FTypes[i];
      if (not Result.IsHasConditions()) or (not Result.CanBeArea) then
        Continue;

      for ii := 0 to Result.ConditionCount - 1 do
      begin
        if not (ctRelation in Result.Conditions[ii].Types) then
          Continue;

        if Result.Conditions[ii].Condition.Evaluate(ATagMap) then
          Exit;
      end;
    end;
  end;
  Result := FTypeInfoIgnore;
end;

procedure TTypeConfig.RegisterFeature(AFeature: TFeature);
var
  n: Integer;
begin
  Assert(Assigned(AFeature));
  Assert(AFeature.GetName() <> '');


  if FFeatureIndexMap[AFeature.FeatureType] = -1 then
  begin
    n := FFeatures.Add(AFeature);
    FFeatureIndexMap[AFeature.FeatureType] := n;

    AFeature.Initialize(FTagRegistry);
  end;
end;

function TTypeConfig.GetFeatureByName(const AName: string): TFeature;
var
  i: Integer;
begin
  for i := 0 to FFeatures.Count-1 do
  begin
    Result := FFeatures[i];
    if Result.GetName() = AName then
      Exit;
  end;
  Result := nil;
end;

function TTypeConfig.GetFeature(AFeatureType: TFeatureType): TFeature;
var
  n: Integer;
begin
  //Result := GetFeatureByName(FeatureNames[AFeatureType]);
  n := FFeatureIndexMap[AFeatureType];
  if n <> -1 then
    Result := FFeatures[n]
  else
    Result := nil;
end;

function TTypeConfig.LoadFromOSTFile(const AFilename: string): Boolean;
{var
  FileSize: TFileOffset;
  Scanner: TOstScanner; }
begin
  Result := False;
  Assert(False, 'Not implemented!');
  { TODO : implement }

  {try
    //FileSize := GetFil

    Scanner := TOstScanner.Create();
  finally
  end;}
end;

function TTypeConfig.LoadFromDataFile(const ADirectory: string): Boolean;
var
  Timer: TStopClock;
  Scanner: TFileScanner;
  FileFormatVersion, FeatureCount, DescriptionCount, TypeCount: LongWord;
  GroupCount: LongWord;
  i, ii: Integer;
  sFeatureName, sLanguageCode, sDescription, sName: string;
  Feature: TFeature;
  TmpTypeInfo: TTypeInfo;
  //TmpBool: Boolean;
begin
  Result := False;
  Scanner := TFileScanner.Create();
  try
    Timer.Init();
    Scanner.Open(IncludeTrailingPathDelimiter(ADirectory) + 'types.dat', fsmSequential, True);

    Scanner.Read(FileFormatVersion);
    if (FileFormatVersion <> FILE_FORMAT_VERSION) then
    begin
      //LogError('File ' + Scanner.Filename + ' format version ('
      //  + IntToStr(FileFormatVersion) + ') <> ' + IntToStr(FILE_FORMAT_VERSION);
      Exit;
    end;

    // Features
    Scanner.Read(FeatureCount);
    for i := 1 to FeatureCount do
    begin
      Scanner.Read(sFeatureName);
      Scanner.Read(DescriptionCount);
      Feature := GetFeatureByName(sFeatureName);

      for ii := 1 to DescriptionCount do
      begin
        Scanner.Read(sLanguageCode);
        Scanner.Read(sDescription);

        if Assigned(Feature) then
        begin
          Feature.AddDescription(sLanguageCode, sDescription);
        end;
      end;
    end;

    // Types
    Scanner.Read(TypeCount);
    for i := 1 to TypeCount do
    begin
      TmpTypeInfo := TTypeInfo.Create('');
      TmpTypeInfo.Load(Scanner);

      // Type Features
      Scanner.Read(FeatureCount);
      for ii := 1 to FeatureCount do
      begin
        Scanner.Read(sFeatureName);
        Feature := GetFeatureByName(sFeatureName);
        if not Assigned(Feature) then
        begin
          //LogError('Feature not found: ' + sFeatureName);
          Exit;
        end;

        TmpTypeInfo.AddFeature(Feature);
      end;

      // Type Groups
      Scanner.Read(GroupCount);
      for ii := 1 to GroupCount do
      begin
        Scanner.Read(sName);
        TmpTypeInfo.AddGroup(sName);
      end;

      // Type Descriptions
      Scanner.Read(DescriptionCount);
      for ii := 1 to DescriptionCount do
      begin
        Scanner.Read(sLanguageCode);
        Scanner.Read(sDescription);

        TmpTypeInfo.AddDescription(sLanguageCode, sDescription);
      end;

      RegisterType(TmpTypeInfo);
    end;

    Scanner.Close();

    Timer.Stop();
    WriteLn('Opening TypeConfig: ' + Timer.ResultString());
    Result := True;
  finally
    Scanner.Free();
  end;

end;

function TTypeConfig.StoreToDataFile(const ADirectory: string): Boolean;
var
  Writer: TFileWriter;
  TypeCount, FeatureCount, DescriptionCount, GroupsCount: LongWord;
  i, ii: Integer;
  TmpFeature: TFeature;
  TmpTypeInfo: TTypeInfo;
  //sLangCode, sDescription: string;
begin
  Result := False;

  Writer := TFileWriter.Create();
  try
    Writer.Open(IncludeTrailingPathDelimiter(ADirectory) + 'types.dat');
    Writer.Write(FILE_FORMAT_VERSION);

    TypeCount := 0;
    for i := 0 to Types.Count-1 do
    begin
      if not Types[i].IsInternal then
        Inc(TypeCount);
    end;

    FeatureCount := 0;
    for i := 0 to Features.Count-1 do
    begin
      if not Features[i].Descriptions.Count <> 0 then
        Inc(FeatureCount);
    end;

    // write features
    Writer.Write(FeatureCount);
    for i := 0 to Features.Count-1 do
    begin
      TmpFeature := Features[i];
      DescriptionCount := TmpFeature.Descriptions.Count;
      if DescriptionCount <> 0 then
      begin
        Writer.Write(TmpFeature.GetName());
        Writer.WriteNumber(DescriptionCount);
        for ii := 0 to TmpFeature.Descriptions.Count-1 do
        begin
          Writer.Write(TmpFeature.Descriptions.Names[ii]);
          Writer.Write(TmpFeature.Descriptions.ValueFromIndex[ii]);
        end;
      end;
    end;

    // write types
    Writer.Write(TypeCount);
    for i := 0 to Types.Count-1 do
    begin
      TmpTypeInfo := Types[i];
      if TmpTypeInfo.IsInternal then
        Continue;

      TmpTypeInfo.Save(Writer);

      // TypeInfo features
      FeatureCount := TmpTypeInfo.FeatureCount;
      Writer.Write(FeatureCount);
      for ii := 0 to TmpTypeInfo.FeatureCount-1 do
      begin
        TmpFeature := TmpTypeInfo.GetFeature(ii);
        Writer.Write(TmpFeature.GetName());
      end;

      // TypeInfo groups
      GroupsCount := TmpTypeInfo.Groups.Count;
      Writer.Write(GroupsCount);
      for ii := 0 to TmpTypeInfo.Groups.Count-1 do
      begin
        Writer.Write(TmpTypeInfo.Groups[ii]);
      end;

      // TypeInfo descriptions
      DescriptionCount := TmpTypeInfo.Descriptions.Count;
      Writer.WriteNumber(DescriptionCount);
      for ii := 0 to TmpTypeInfo.Descriptions.Count-1 do
      begin
        Writer.Write(TmpTypeInfo.Descriptions.Names[ii]);
        Writer.Write(TmpTypeInfo.Descriptions.ValueFromIndex[ii]);
      end;
    end;

    Writer.Close();

    Result := True;
  finally
    Writer.Free();
  end;
end;

{ TFeatureValueStorage }

procedure TFeatureValueStorage.AfterConstruction();
begin
  inherited AfterConstruction();
  FStrList := TStringList.Create();
  FStrHash.Init();
end;

procedure TFeatureValueStorage.BeforeDestruction();
begin
  ClearHash();
  FreeAndNil(FStrList);
  inherited BeforeDestruction();
end;

procedure TFeatureValueStorage.ClearHash();
begin
  FStrHash.Clear();
end;

function TFeatureValueStorage.AddStr(const AValue: string): Integer;
begin
  Result := FStrHash.ValueOf(AValue);
  if Result <> -1 then
    Exit;

  Result := FStrList.Add(AValue);
  FStrHash.Add(AValue, Result);
end;

function TFeatureValueStorage.GetStr(ASID: Integer): string;
begin
  if (ASID >= 0) and (ASID < FStrList.Count) then
    Result := FStrList[ASID]
  else
    Result := '';
end;

{ TTypeInfoSet }

procedure TTypeInfoSet.Init(ATypeConfig: TTypeConfig);
begin
  SetLength(Types, ATypeConfig.GetTypeCount());
end;

procedure TTypeInfoSet.Init(ATypes: TTypeInfoList);
var
  i: Integer;
begin
  Clear();
  for i := 0 to ATypes.Count-1 do
    Add(ATypes[i]);
end;

procedure TTypeInfoSet.Init(const AOther: TTypeInfoSet);
begin
  Clear();
  Add(AOther);
end;

procedure TTypeInfoSet.Adapt(ATypeConfig: TTypeConfig);
begin

end;

procedure TTypeInfoSet.Clear();
begin
  SetLength(Types, 0);
end;

procedure TTypeInfoSet.Add(AType: TTypeInfo);
var
  i, PrevLen: Integer;
begin
  Assert(Assigned(AType));
  if (AType.Index >= Length(Types)) then
  begin
    PrevLen := Length(Types);
    SetLength(Types, AType.Index + 1);
    for i := PrevLen to AType.Index do
      Types[AType.Index] := nil;
  end;

  Types[AType.Index] := AType;
end;

procedure TTypeInfoSet.Add(ATypes: TTypeInfoList);
var
  i: Integer;
begin
  for i := 0 to ATypes.Count-1 do
    Add(ATypes[i]);
end;

procedure TTypeInfoSet.Add(const AOther: TTypeInfoSet);
begin

end;

procedure TTypeInfoSet.Remove(AType: TTypeInfo);
begin
  if (AType.Index < Length(Types)) and (Types[AType.Index] <> nil) then
    Types[AType.Index] := nil;
end;

procedure TTypeInfoSet.Remove(const AOther: TTypeInfoSet);
begin

end;

procedure TTypeInfoSet.Intersection(const AOther: TTypeInfoSet);
begin

end;

function TTypeInfoSet.IsSet(AType: TTypeInfo): Boolean;
begin
  Assert(Assigned(AType));
  Result := (AType.Index < Length(Types)) and (Types[AType.Index] <> nil);
end;

function TTypeInfoSet.Intersects(const AOther: TTypeInfoSet): Boolean;
begin

end;

initialization

finalization

if Assigned(FeatureValueStorage) then
  FreeAndNil(FeatureValueStorage);

end.

