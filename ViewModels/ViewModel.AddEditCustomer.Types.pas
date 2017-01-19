unit ViewModel.AddEditCustomer.Types;

interface

uses
	ViewModel.Types;

type
  IViewModelAddEditCustomer = interface
    ['{08A3E513-FF16-4EFB-B53E-CF45C0E4511A}']
    function getComponentsRecord: TComponentsRecord;
    function getCustomer: TCustomerTransientRecord;

    procedure setCustomer (const aID: Integer);

    procedure saveCustomer (var aCustomer: TCustomerTransientRecord);

    property ComponentsRecord: TComponentsRecord read getComponentsRecord;
    property Customer: TCustomerTransientRecord read getCustomer;
  end;

implementation

end.
