unit Model.Main;

interface

uses
	Core.Database.Aurelius.Types, Model.Main.Types;

function createModelMain (const aDatabase: IAurelius): IModelMain;

implementation

uses
	System.Generics.Collections,
	Core.Database.Entities, Aurelius.Engine.ObjectManager, System.SysUtils,
  Aurelius.Types.Blob;

type
  TModelMain = class (TInterfacedObject, IModelMain)
  private
    fDatabase: IAurelius;
    fObjectManager: TObjectManager;
    procedure getCustomerList(var aList: TObjectList<TCustomers>);
    procedure addCustomer (var aCustomer: TCustomers);
    procedure updateCustomer (var aCustomer: TCustomers);
    procedure deleteCustomer (var aCustomer: TCustomers);
    procedure addOrUpdate (var aCustomer: TCustomers);
    procedure getCustomer (const aID: integer; var aCustomer: TCustomers);
  public
    constructor Create (const aDatabase: IAurelius);
    destructor Destroy; override;
  end;


{ TModelMain }

procedure TModelMain.addCustomer(var aCustomer: TCustomers);
begin
  if Assigned(aCustomer) then
  begin
    fObjectManager.Save(aCustomer);
    fObjectManager.Flush;
  end;
end;

procedure TModelMain.addOrUpdate(var aCustomer: TCustomers);
begin
  if Assigned(aCustomer) then
  begin
    fObjectManager.SaveOrUpdate(aCustomer);
    fObjectManager.Flush;
  end;
end;

constructor TModelMain.Create(const aDatabase: IAurelius);
begin
  inherited Create;
  if not Assigned(aDatabase) then
    raise Exception.Create('The Database is nil. Please pass a valid IAurelius database');
  fDatabase:=aDatabase;
  fObjectManager:=fDatabase.createDatabaseObjectManager;
end;

procedure TModelMain.deleteCustomer(var aCustomer: TCustomers);
begin
  if Assigned(aCustomer) then
    fObjectManager.Remove(aCustomer);
end;

destructor TModelMain.Destroy;
begin
  fObjectManager.Free;
  inherited;
end;

procedure TModelMain.getCustomer(const aID: integer; var aCustomer: TCustomers);
begin
  aCustomer:=fObjectManager.Find<TCustomers>(aId);
end;

procedure TModelMain.getCustomerList(var aList: TObjectList<TCustomers>);
begin
  fObjectManager.Clear;
  aList:=fObjectManager.Find<TCustomers>.OrderBy('LastName').List;
end;

procedure TModelMain.updateCustomer(var aCustomer: TCustomers);
begin
  if Assigned(aCustomer) then
  begin
    fObjectManager.Update(aCustomer);
    fObjectManager.Flush;
  end;
end;

function createModelMain (const aDatabase: IAurelius): IModelMain;
begin
  Result:=TModelMain.Create(aDatabase);
end;

end.
