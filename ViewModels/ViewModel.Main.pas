unit ViewModel.Main;

interface

uses
	Model.Main.Types, ViewModel.Main.Types;

function createViewModelMain (const aModel: IModelMain): IViewModelMain;

implementation

uses
	System.Classes, Core.Database.Entities, System.SysUtils, System.Generics.Collections,
  Aurelius.Types.Blob, FMX.Graphics, ViewModel.Types, Core.Helpers;

type
  TViewModelMain = class (TInterfacedObject, IViewModelMain)
  private
    fCustomerList: TStringList;
    fComponentsRecord: TComponentsRecord;
    fModel: IModelMain;

    procedure setMainComponents;

    function getCustomerList: TStringList;
    function getComponentsRecord: TComponentsRecord;

    procedure getCustomer (const aID: Integer; var aCustomer: TCustomerTransientRecord);
    procedure deleteCustomer (const aID: Integer);
  public
    constructor Create (const aModel: IModelMain);
    destructor Destroy; override;
  end;


{ TViewModelMain }

constructor TViewModelMain.Create(const aModel: IModelMain);
begin
  inherited Create;
  if Assigned(aModel) then
    fModel:=aModel
  else
    raise Exception.Create('The Main Model is nil in Main ViewModel');
  fCustomerList:=TStringList.Create;
  setMainComponents;
  fComponentsRecord.TotalLabel:=NoCustomers;
end;

procedure TViewModelMain.deleteCustomer(const aID: Integer);
var
  tmpCustomer: TCustomers;
begin
  try
    try
      fModel.getCustomer(aID, tmpCustomer);
    except
      raise;
    end;
    if Assigned(tmpCustomer) then
      fModel.deleteCustomer(tmpCustomer);
  except
    raise;
  end;
end;

destructor TViewModelMain.Destroy;
begin
  fCustomerList.Free;
  inherited;
end;

procedure TViewModelMain.setMainComponents;
begin
  fComponentsRecord.AddButtonEnabled := True;
  fComponentsRecord.DeleteButtonEnabled := False;
  fComponentsRecord.EditButtonEnabled := false;
  fComponentsRecord.PanelCancelButtonVisible := False;
  fComponentsRecord.PanelEdit1Text := '';
  fComponentsRecord.PanelEdit1Visible := false;
  fComponentsRecord.PanelEdit2Text := '';
  fComponentsRecord.PanelEdit2Visible := False;
  fComponentsRecord.PanelLabel1Text := '';
  fComponentsRecord.PanelLabel1Visible := true;
  fComponentsRecord.PanelLabel2Text := '';
  fComponentsRecord.PanelLabel2Visible := True;
  fComponentsRecord.PanelLoadButtonVisile := False;
  fComponentsRecord.PanelSaveButtonVisible := False;
end;

function TViewModelMain.getComponentsRecord: TComponentsRecord;
begin
  Result:=fComponentsRecord;
end;

procedure TViewModelMain.getCustomer (const aID: Integer; var aCustomer: TCustomerTransientRecord);
var
  tmpCustomer: TCustomers;
begin
  if not aID>0 then
    Exit;

  try
    fModel.getCustomer(aID, tmpCustomer);

    aCustomer.PhotoBitmap:=nil;
    if Assigned(tmpCustomer) then
    begin
      if tmpCustomer.Firstname.HasValue then
        aCustomer.FirstName:=Trim(tmpCustomer.Firstname.Value)
      else
        aCustomer.FirstName:='';
      if tmpCustomer.Lastname.HasValue then
        aCustomer.LastName:=Trim(tmpCustomer.Lastname.Value)
      else
        aCustomer.LastName:='';

      if not tmpCustomer.Photo.IsNull then
      begin
        aCustomer.PhotoBitmap:=TBitmap.Create;
        Blob2bitmap(tmpCustomer.Photo, aCustomer.PhotoBitmap);
      end;

      fComponentsRecord.EditButtonEnabled:=true;
      fComponentsRecord.DeleteButtonEnabled:=true;
    end
    else
    begin
      aCustomer.FirstName:='';
      aCustomer.LastName:='';
      fComponentsRecord.EditButtonEnabled:=false;
      fComponentsRecord.DeleteButtonEnabled:=false;
    end;
    fComponentsRecord.PanelLabel1Text:=aCustomer.FirstName;
    fComponentsRecord.PanelLabel2Text:=aCustomer.LastName;
  except
    raise;
  end;
end;

function TViewModelMain.getCustomerList: TStringList;
var
  tmpList: TObjectList<TCustomers>;
  tmpCustomer: TCustomers;
  tmpStr: string;
begin
  fCustomerList.Clear;
  try
    fModel.getCustomerList(tmpList);

    if (not Assigned(tmpList)) or (tmpList.Count=0) then
    begin
      fComponentsRecord.TotalLabel:=NoCustomers;
      setMainComponents;
      tmpList.Free;
    end
    else
    begin
      for tmpCustomer in tmpList do
      begin
        tmpStr:='';
        if tmpCustomer.Lastname.HasValue then
          tmpStr:=trim(tmpCustomer.Lastname.Value)+', ';
        if tmpCustomer.Firstname.HasValue then
          tmpStr:=tmpStr+trim(tmpCustomer.Firstname.Value);
        tmpStr:=tmpStr+'|'+tmpCustomer.Id.ToString;
        fCustomerList.Add(tmpStr);
        fComponentsRecord.TotalLabel:=Format(NumOfCustomers,[tmpList.Count.ToString]);
        fComponentsRecord.EditButtonEnabled:=true;
        fComponentsRecord.DeleteButtonEnabled:=true;
      end;
    end;
    Result:=fCustomerList;
  except
    raise;
  end;
end;


function createViewModelMain (const aModel: IModelMain): IViewModelMain;
begin
  Result:=TViewModelMain.Create(aModel);
end;

end.
