unit ViewModel.Types;

interface

uses
	FMX.Graphics;

type
  TCustomerTransientRecord = record
    PhotoBitmap: TBitmap;
    FirstName,
    LastName: string;
    ID: Integer;
  end;

  TComponentsRecord = record
    TotalLabel: string;

    AddButtonEnabled,
    DeleteButtonEnabled,
    EditButtonEnabled,
    PanelLoadButtonVisile,
    PanelLabel1Visible,
    PanelEdit1Visible,
    PanelLabel2Visible,
    PanelEdit2Visible,
    PanelSaveButtonVisible,
    PanelCancelButtonVisible: Boolean;

    PanelLabel1Text,
    PanelEdit1Text,
    PanelLabel2Text,
    PanelEdit2Text,
    PanelSaveButtonText,

    WindowCaption: string;
  end;

implementation

end.
