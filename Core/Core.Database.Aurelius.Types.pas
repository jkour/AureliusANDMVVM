unit Core.Database.Aurelius.Types;

interface

uses
  Aurelius.Engine.ObjectManager, Aurelius.Drivers.Interfaces, Aurelius.Engine.DatabaseManager;

type
  IAurelius = interface
    ['{BC578F5C-4D4F-43CD-8B7C-C5D4A57CE164}']
    function getDatabaseConnection: IDBConnection;
    function getDatabaseManager: TDatabaseManager;
    function createDatabaseObjectManager: TObjectManager;
    procedure createDatabaseManager;
    property DatabaseConnection: IDBConnection read getDatabaseConnection;
    property DatabaseManager: TDatabaseManager read getDatabaseManager;
  end;

implementation

end.


