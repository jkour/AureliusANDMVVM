unit View.AddEditCustomer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  View.Frame.CustomerDetails, ViewModel.AddEditCustomer.Types, System.ImageList,
  FMX.ImgList, ViewModel.Types;

type
  TFormAddEditCustomer = class(TForm)
    TFrameCustomerDetails1: TFrameCustomerDetails;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    procedure TFrameCustomerDetails1ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TFrameCustomerDetails1ButtonLoadImageClick(Sender: TObject);
    procedure TFrameCustomerDetails1ButtonSaveClick(Sender: TObject);
  private
    fViewModel: IViewModelAddEditCustomer;
    fCustomer: TCustomerTransientRecord;
    procedure updateGUI;
  public
    procedure setViewModel (const aViewModel: IViewModelAddEditCustomer);
  end;

implementation


{$R *.fmx}

procedure TFormAddEditCustomer.FormCreate(Sender: TObject);
begin
  TFrameCustomerDetails1.ImagePhoto.Bitmap:=ImageList1.Source[0].MultiResBitmap.Bitmaps[1];
end;

procedure TFormAddEditCustomer.setViewModel(
  const aViewModel: IViewModelAddEditCustomer);
begin
  if not Assigned(aViewModel) then
    raise Exception.Create('The ViewModel is nil')
  else
  begin
    fViewModel:=aViewModel;
    fCustomer:=fViewModel.Customer;
    updateGUI;
  end;
end;

procedure TFormAddEditCustomer.TFrameCustomerDetails1ButtonCancelClick(
  Sender: TObject);
begin
  self.Close;
end;

procedure TFormAddEditCustomer.TFrameCustomerDetails1ButtonLoadImageClick(
  Sender: TObject);
var
  tmpBitmap: TBitmap;
begin
  if OpenDialog1.Execute then
  begin
    tmpBitmap:=TBitmap.Create;
    tmpBitmap.LoadFromFile(OpenDialog1.Files.Strings[0]);
    TFrameCustomerDetails1.ImagePhoto.Bitmap:=tmpBitmap;
    fCustomer.PhotoBitmap:=tmpBitmap;
  end;
end;

procedure TFormAddEditCustomer.TFrameCustomerDetails1ButtonSaveClick(
  Sender: TObject);
begin
  fCustomer.FirstName:=Trim(TFrameCustomerDetails1.EditFirstName.Text);
  fCustomer.LastName:=Trim(TFrameCustomerDetails1.EditLastName.Text);
  fViewModel.saveCustomer(fCustomer);
  Self.Close;
end;

procedure TFormAddEditCustomer.updateGUI;
begin
  self.Caption:=fViewModel.ComponentsRecord.WindowCaption;
  with TFrameCustomerDetails1 do
  begin
    if Assigned(fCustomer.PhotoBitmap) then
      ImagePhoto.Bitmap:=fCustomer.PhotoBitmap
    else
      ImagePhoto.Bitmap:=ImageList1.Source[0].MultiResBitmap.Bitmaps[1];

    ButtonLoadImage.Visible:=fViewModel.ComponentsRecord.PanelLoadButtonVisile;

    LabelFirstName.Text:=fViewModel.ComponentsRecord.PanelLabel2Text;
    LabelLastName.Text:=fViewModel.ComponentsRecord.PanelLabel1Text;
    EditFirstName.Text:=fViewModel.ComponentsRecord.PanelEdit1Text;
    EditLastName.Text:=fViewModel.ComponentsRecord.PanelEdit2Text;

    ButtonSave.Text:=fViewModel.ComponentsRecord.PanelSaveButtonText;
  end;
end;

end.
