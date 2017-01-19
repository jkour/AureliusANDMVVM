unit ViewModel.AddEditCustomer;

interface

uses
	Model.Main.Types, ViewModel.AddEditCustomer.Types;

function createViewModelAddEditCustomer (const aModel: IModelMain): IViewModelAddEditCustomer;

implementation

uses
	ViewModel.Types, System.SysUtils, Core.Database.Entities, Core.Helpers, FMX.Graphics;

type
  TViewModelAddEditCustomer = class (TInterfacedObject, IViewModelAddEditCustomer)
  private
    fModel: IModelMain;
    fComponentsRecord: TComponentsRecord;
    fCustomer: TCustomerTransientRecord;

    function getComponentsRecord: TComponentsRecord;
    function getCustomer: TCustomerTransientRecord;
    procedure setCustomer (const aID: Integer);
    procedure saveCustomer (var aCustomer: TCustomerTransientRecord);

  public
    constructor Create (const aModel: IModelMain);
  end;

{ TViewModelAddEditCustomer }

constructor TViewModelAddEditCustomer.Create(const aModel: IModelMain);
begin
  inherited Create;
  if Assigned(aModel) then
    fModel:=aModel
  else
    raise Exception.Create('The Model is nil in AddEditCustomer ViewModel');
  fComponentsRecord.PanelCancelButtonVisible:=true;
  fComponentsRecord.PanelLoadButtonVisile:=True;
  fComponentsRecord.PanelSaveButtonVisible:=true;
  fComponentsRecord.PanelEdit1Visible:=true;
  fComponentsRecord.PanelEdit2Visible:=True;
  fComponentsRecord.PanelLabel1Text:='Last Name:';
  fComponentsRecord.PanelLabel2Text:='First Name:';
end;

function TViewModelAddEditCustomer.getComponentsRecord: TComponentsRecord;
begin
  Result:=fComponentsRecord;
end;

function TViewModelAddEditCustomer.getCustomer: TCustomerTransientRecord;
begin
  result:=fCustomer;
end;

procedure TViewModelAddEditCustomer.saveCustomer(
  var aCustomer: TCustomerTransientRecord);
var
  tmpCustomer: TCustomers;
begin
  if aCustomer.ID=0 then
    tmpCustomer:=TCustomers.Create
  else
  begin
    try
      fModel.getCustomer(aCustomer.ID, tmpCustomer);
    except
      raise;
    end;
  end;
  tmpCustomer.Firstname:=aCustomer.FirstName;
  tmpCustomer.Lastname:=aCustomer.LastName;
  if Assigned(aCustomer.PhotoBitmap) then
    Bitmap2BlobAs(aCustomer.PhotoBitmap, tmpCustomer.Photo, 'png');
  try
    if aCustomer.ID=0 then
      fModel.addCustomer(tmpCustomer)
    else
      fModel.updateCustomer(tmpCustomer);

    //Or just call this instead of the above if-else block
    //  fmodel.addOrUpdate(tmpCustomer);
  except
    raise;
  end;
end;

procedure TViewModelAddEditCustomer.setCustomer(const aID: Integer);
var
  tmpCustomer: TCustomers;
begin
  FreeAndNil(fCustomer.PhotoBitmap);
  fCustomer.ID:=aId;
  if aID=0 then
  begin
    fComponentsRecord.PanelSaveButtonText:='Save';
    fComponentsRecord.WindowCaption:='Add New Customer';
  end
  else
  begin
    try
      fModel.getCustomer(aID, tmpCustomer);
      if not Assigned(tmpCustomer) then
        Exit;
      if tmpCustomer.Firstname.HasValue then
        fCustomer.FirstName:=trim(tmpCustomer.Firstname.Value)
      else
        fCustomer.FirstName:='';
      if tmpCustomer.Lastname.HasValue then
        fCustomer.LastName:=trim(tmpCustomer.Lastname.Value)
      else
        fCustomer.LastName:='';
      if not tmpCustomer.Photo.IsNull then
      begin
        fCustomer.PhotoBitmap:=TBitmap.Create;
        Blob2bitmap(tmpCustomer.Photo, fCustomer.PhotoBitmap);
      end;
      fComponentsRecord.PanelSaveButtonText:='Update';
      fComponentsRecord.WindowCaption:='Edit Customer';
      fComponentsRecord.PanelEdit1Text:=fCustomer.FirstName;
      fComponentsRecord.PanelEdit2Text:=fCustomer.LastName;
    except
      raise;
    end;
  end;
end;

function createViewModelAddEditCustomer (const aModel: IModelMain): IViewModelAddEditCustomer;
begin
  Result:=TViewModelAddEditCustomer.Create(aModel);
end;

end.
