unit Core.Helpers;

interface

uses
	FMX.Graphics, Aurelius.Types.Blob;

procedure Bitmap2BlobAs(aBmp: TBitmap; const aBlob: TBlob; const aType: string);
procedure Blob2bitmap(const aBlob: TBlob; aBMP: TBitmap);

implementation

uses
	FMX.Surfaces, System.Classes, System.SysUtils;


procedure Bitmap2BlobAs(aBmp: TBitmap; const aBlob: TBlob; const aType: string);
var
  bmp: TBitmapSurface;
  bs: TBytesStream;
begin
  Bmp := TBitmapSurface.create;
  try
    bmp.assign(aBmp);
    bs := TBytesStream.Create;
    try
      TBitmapCodecManager.SaveToStream(bs, bmp, aType);
      aBlob.AsBytes := bs.Bytes;
    finally
      bs.free;
    end;
  finally
    bmp.Free;
  end;
end;

procedure Blob2bitmap(const aBlob: TBlob; aBMP: TBitmap);
var
  MS: TMemoryStream;
begin
  if not Assigned(aBMP) then
    raise Exception.Create('Bitmap not initialised in Blob2Bitmap');
  MS := TMemoryStream.Create;
  try
    aBlob.SaveToStream(MS);
    MS.Position := 0;
    aBMP.LoadFromStream(MS);
  finally
    MS.free;
  end;
end;

end.
