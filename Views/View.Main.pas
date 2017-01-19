unit View.Main;

interface

uses
  System.SysUtils, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  View.Frame.CustomerDetails, System.ImageList, FMX.ImgList, ViewModel.Main.Types,
  System.Generics.Collections;

type
  TFormMain = class(TForm)
    ListBoxCustomers: TListBox;
    LabelTitle: TLabel;
    LabelTotalCustomers: TLabel;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    TFrameCustomerDetails1: TFrameCustomerDetails;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxCustomersChange(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
  private
    fViewModel: IViewModelMain;
    fIndexID: TDictionary<Integer, Integer>;
    procedure updateGUI;
    procedure updateList;
    procedure updateCustomer (const aID: Integer);
  public
    procedure setViewModel (const aViewModel: IViewModelMain);
  end;

implementation

uses
	System.StrUtils, System.Types,
  Model.Main.Types, Model.Main, Model.Database, ViewModel.Types, ViewModel.AddEditCustomer,
  ViewModel.AddEditCustomer.Types, View.AddEditCustomer;

{$R *.fmx}

procedure TFormMain.ButtonAddClick(Sender: TObject);
var
  newModel: IModelMain;
  newEdit: TFormAddEditCustomer;
  newViewModel: IViewModelAddEditCustomer;
begin
  newModel:=createModelMain (database);
  newViewModel:=createViewModelAddEditCustomer(newModel);
  newViewModel.setCustomer(0);
  newEdit:=TFormAddEditCustomer.Create(self);
  newEdit.Parent:=Self;
  newEdit.setViewModel(newViewModel);
  newEdit.ShowModal;
  updateList;
  if ListBoxCustomers.Count>0 then
    ListBoxCustomers.ItemIndex:=0;
end;

procedure TFormMain.ButtonDeleteClick(Sender: TObject);
begin
  if ListBoxCustomers.ItemIndex>-1 then
    if MessageDlg('Are you sure you want to delete the selected customer?',
          TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],0)=mrYes then
    begin
      fViewModel.deleteCustomer(fIndexID.Items[ListBoxCustomers.ItemIndex]);
      updateList;
      updateGUI;
      if ListBoxCustomers.Count>-1 then
        ListBoxCustomers.ItemIndex:=0;
    end;
end;

procedure TFormMain.ButtonEditClick(Sender: TObject);
var
  editModel: IModelMain;
  editEdit: TFormAddEditCustomer;
  editViewModel: IViewModelAddEditCustomer;
begin
  if not ListBoxCustomers.ItemIndex>-1 then
    Exit;
  editModel:=createModelMain (database);
  editViewModel:=createViewModelAddEditCustomer(editModel);
  editViewModel.setCustomer(fIndexID.Items[ListBoxCustomers.ItemIndex]);
  editEdit:=TFormAddEditCustomer.Create(self);
  editEdit.Parent:=Self;
  editEdit.setViewModel(editViewModel);
  editEdit.ShowModal;
  updateList;
  if ListBoxCustomers.Count>0 then
    ListBoxCustomers.ItemIndex:=0;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TFrameCustomerDetails1.ImagePhoto.Bitmap:=ImageList1.Source[0].MultiResBitmap.Bitmaps[1];
  fIndexID:=TDictionary<Integer, Integer>.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  fIndexID.Free;
end;

procedure TFormMain.ListBoxCustomersChange(Sender: TObject);
begin
  if (ListBoxCustomers.ItemIndex>-1) and
      (fIndexID.ContainsKey(ListBoxCustomers.ItemIndex)) then
    updateCustomer(fIndexID.Items[ListBoxCustomers.ItemIndex]);
end;

procedure TFormMain.setViewModel(const aViewModel: IViewModelMain);
begin
  if not Assigned(aViewModel) then
    raise Exception.Create('The ViewModel is nil')
  else
  begin
    fViewModel:=aViewModel;
    updateGUI;
    updateList;
  end;
end;

procedure TFormMain.updateCustomer (const aID: Integer);
var
  tmpCustomer: TCustomerTransientRecord;
begin
  fViewModel.getCustomer(aID, tmpCustomer);
  updateGUI;
  TFrameCustomerDetails1.ImagePhoto.Visible:=true;
  if assigned(tmpCustomer.PhotoBitmap) then
    TFrameCustomerDetails1.ImagePhoto.Bitmap:=tmpCustomer.PhotoBitmap
  else
    TFrameCustomerDetails1.ImagePhoto.Bitmap:=ImageList1.Source[0].MultiResBitmap.Bitmaps[1];
end;

procedure TFormMain.updateGUI;
begin
  LabelTotalCustomers.Text:=fViewModel.ComponentsRecord.TotalLabel;
  ButtonAdd.Enabled:=fViewModel.ComponentsRecord.AddButtonEnabled;
  ButtonDelete.Enabled:=fViewModel.ComponentsRecord.DeleteButtonEnabled;
  ButtonEdit.Enabled:=fViewModel.ComponentsRecord.EditButtonEnabled;
  TFrameCustomerDetails1.ButtonLoadImage.Visible:=fViewModel.ComponentsRecord.PanelLoadButtonVisile;
  TFrameCustomerDetails1.ButtonSave.Visible:=fViewModel.ComponentsRecord.PanelSaveButtonVisible;
  TFrameCustomerDetails1.ButtonCancel.Visible:=fViewModel.ComponentsRecord.PanelCancelButtonVisible;
  TFrameCustomerDetails1.LabelFirstName.Visible:=fViewModel.ComponentsRecord.PanelLabel1Visible;
  TFrameCustomerDetails1.LabelFirstName.Text:=fViewModel.ComponentsRecord.PanelLabel1Text;
  TFrameCustomerDetails1.LabelLastName.Visible:=fViewModel.ComponentsRecord.PanelLabel2Visible;
  TFrameCustomerDetails1.LabelLastName.Text:=fViewModel.ComponentsRecord.PanelLabel2Text;
  TFrameCustomerDetails1.EditFirstName.Visible:=fViewModel.ComponentsRecord.PanelEdit1Visible;
  TFrameCustomerDetails1.EditFirstName.Text:=fViewModel.ComponentsRecord.PanelEdit1Text;
  TFrameCustomerDetails1.EditLastName.Visible:=fViewModel.ComponentsRecord.PanelEdit2Visible;
  TFrameCustomerDetails1.EditLastName.Text:=fViewModel.ComponentsRecord.PanelEdit2Text;
  TFrameCustomerDetails1.ImagePhoto.Visible:=False;
end;

procedure TFormMain.updateList;
var
  tmpList: TStringList;
  tmpListItem: TListBoxItem;
  tmpStr: string;
  i: integer;
begin
  ListBoxCustomers.Clear;
  fIndexID.Clear;
  tmpList:=fViewModel.CustomerList;
  for i := 0 to tmpList.Count-1 do
  begin
    tmpStr:=tmpList.Strings[i];
    tmpListItem:=TListBoxItem.Create(ListBoxCustomers);
    tmpListItem.Parent:=ListBoxCustomers;
    tmpListItem.Text:=SplitString(tmpStr,'|')[0];
    fIndexID.Add(i, SplitString(tmpStr,'|')[1].ToInteger);
  end;
  LabelTotalCustomers.Text:=fViewModel.ComponentsRecord.TotalLabel;
end;

end.
