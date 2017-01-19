unit Core.Database.Entities;

interface

uses
  SysUtils, Aurelius.Mapping.Attributes, Aurelius.Types.Blob, Aurelius.Types.DynamicProperties, Aurelius.Types.Nullable, Aurelius.Types.Proxy, Aurelius.Criteria.Dictionary;

type
  TCustomers = class;
  TCustomersTableDictionary = class;
  
  [Entity]
  [Table('Customers')]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TCustomers = class
  private
    [Column('ID', [TColumnProp.Required, TColumnProp.NoInsert, TColumnProp.NoUpdate])]
    FId: Integer;
    
    [Column('FirstName', [], 65535)]
    FFirstname: Nullable<string>;
    
    [Column('LastName', [], 65535)]
    FLastname: Nullable<string>;
    
    [Column('Photo', [TColumnProp.Lazy])]
    FPhoto: TBlob;
  public
    property Id: Integer read FId write FId;
    property Firstname: Nullable<string> read FFirstname write FFirstname;
    property Lastname: Nullable<string> read FLastname write FLastname;
    property Photo: TBlob read FPhoto write FPhoto;
  end;
  
  TDicDictionary = class
  private
    FCustomers: TCustomersTableDictionary;
    function GetCustomers: TCustomersTableDictionary;
  public
    destructor Destroy; override;
    property Customers: TCustomersTableDictionary read GetCustomers;
  end;
  
  TCustomersTableDictionary = class
  private
    FId: TDictionaryAttribute;
    FFirstname: TDictionaryAttribute;
    FLastname: TDictionaryAttribute;
    FPhoto: TDictionaryAttribute;
  public
    constructor Create;
    property Id: TDictionaryAttribute read FId;
    property Firstname: TDictionaryAttribute read FFirstname;
    property Lastname: TDictionaryAttribute read FLastname;
    property Photo: TDictionaryAttribute read FPhoto;
  end;
  
function Dic: TDicDictionary;

implementation

var
  __Dic: TDicDictionary;

function Dic: TDicDictionary;
begin
  if __Dic = nil then __Dic := TDicDictionary.Create;
  result := __Dic
end;

{ TDicDictionary}

destructor TDicDictionary.Destroy;
begin
  if FCustomers <> nil then FCustomers.Free;
  inherited;
end;

function TDicDictionary.GetCustomers: TCustomersTableDictionary;
begin
  if FCustomers = nil then FCustomers := TCustomersTableDictionary.Create;
  result := FCustomers;
end;

{ TCustomersTableDictionary}

constructor TCustomersTableDictionary.Create;
begin
  inherited;
  FId := TDictionaryAttribute.Create('Id');
  FFirstname := TDictionaryAttribute.Create('Firstname');
  FLastname := TDictionaryAttribute.Create('Lastname');
  FPhoto := TDictionaryAttribute.Create('Photo');
end;

initialization
  RegisterEntity(TCustomers);

finalization
  if __Dic <> nil then __Dic.Free

end.
