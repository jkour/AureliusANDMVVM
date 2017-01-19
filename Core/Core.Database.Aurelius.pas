unit Core.Database.Aurelius;

interface

uses
	Core.Database.Aurelius.Types;

function createAureliusDatabase (const path: string=''): IAurelius;

implementation

uses
	Aurelius.Engine.DatabaseManager,
	Aurelius.Drivers.Interfaces,
  System.SysUtils, Aurelius.Drivers.SQLite,
  Aurelius.Sql.SQLite,
  System.IOUtils, Aurelius.Engine.ObjectManager;

type
  TAurelius = class (TInterfacedObject, IAurelius)
  private
    fDatabaseConnection: IDBConnection;
    fDatabaseManager: TDatabaseManager;
    fDatabasePath: string;
    function getDatabaseConnection: IDBConnection;
    function getDatabaseManager: TDatabaseManager;
    procedure createDatabaseManager;
    function createDatabaseObjectManager: TObjectManager;
  public
    constructor Create (const path: string='');
    destructor Destroy; override;
  end;

const
  databaseFilename='test.sqlite';

{ TAurelius }

constructor TAurelius.Create(const path: string);
begin
  inherited Create;
  fDatabasePath:=Trim(path);
  try
    fDatabaseConnection:=
    TSQLiteNativeConnectionAdapter.Create(
      TPath.Combine(fDatabasePath, databaseFilename));
   except
    raise Exception.Create('Local database can''t be created');
   end;
  createDatabaseManager;
  if not FileExists(TPath.Combine(fDatabasePath, databaseFilename)) then
    fDatabaseManager.BuildDatabase;
end;

procedure TAurelius.createDatabaseManager;
begin
  if Assigned(fDatabaseConnection) then
    fDatabaseManager:=TDatabaseManager.Create(fDatabaseConnection)
  else
    raise Exception.Create('Database Connection is nil');
end;

function TAurelius.createDatabaseObjectManager: TObjectManager;
begin
  result:=TObjectManager.Create(fDatabaseConnection);
end;

destructor TAurelius.Destroy;
begin
  fDatabaseManager.Free;
  inherited;
end;

function TAurelius.getDatabaseConnection: IDBConnection;
begin
  Result:=fDatabaseConnection;
end;

function TAurelius.getDatabaseManager: TDatabaseManager;
begin
  Result:=fDatabaseManager;
end;

function createAureliusDatabase (const path: string=''): IAurelius;
begin
  Result:=TAurelius.Create(path);
end;

end.
