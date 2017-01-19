unit Model.Main.Types;

interface

uses
	System.Generics.Collections, Core.Database.Entities;

type
  IModelMain = interface
    ['{3B6359FE-F45F-4E6C-8B06-5322BF7F442E}']
    procedure getCustomerList(var aList: TObjectList<TCustomers>);
    procedure addCustomer (var aCustomer: TCustomers);
    procedure updateCustomer (var aCustomer: TCustomers);
    procedure deleteCustomer (var aCustomer: TCustomers);
    procedure addOrUpdate (var aCustomer: TCustomers);
    procedure getCustomer (const aID: integer; var aCustomer: TCustomers);
  end;

implementation

end.
