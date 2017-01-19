unit ViewModel.Main.Types;

interface

uses
	FMX.Graphics, System.Classes, ViewModel.Types;

type
  IViewModelMain = interface
    ['{5F9C41E0-ED9E-4C80-9166-503D0315A615}']
    function getCustomerList: TStringList;
    function getComponentsRecord: TComponentsRecord;

    procedure getCustomer (const aID: Integer; var aCustomer: TCustomerTransientRecord);
    procedure deleteCustomer (const aID: Integer);

    property CustomerList: TStringList read getCustomerList;
    property ComponentsRecord: TComponentsRecord read getComponentsRecord;
  end;

const
  NoCustomers = 'There are no customers';
  NumOfCustomers ='%s customers';

implementation

end.
