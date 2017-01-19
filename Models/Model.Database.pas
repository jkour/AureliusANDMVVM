unit Model.Database;

interface

uses
	Core.Database.Aurelius.Types;

var
  database: IAurelius;

implementation

uses
	Core.Database.Aurelius;

initialization
  database:=createAureliusDatabase;
end.
