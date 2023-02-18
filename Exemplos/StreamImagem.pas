uses
  SysUtils, Classes, Graphics;

function BitmapToBase64(const Bitmap: TBitmap): string;
var
  Stream: TMemoryStream;
  Input: TStringStream;
  Output: TStringStream;
begin
  Stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Stream);
    Stream.Position := 0;
    Input := TStringStream.Create(Stream.Memory, Stream.Size, TEncoding.ASCII);
    try
      Output := TStringStream.Create('', TEncoding.ASCII);
      try
        Output.CopyFrom(Input, Input.Size);
        Result := Output.DataString;
      finally
        Output.Free;
      end;
    finally
      Input.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function Base64ToBitmap(const Base64: string): TBitmap;
var
  Input: TStringStream;
  Output: TMemoryStream;
begin
  Input := TStringStream.Create(Base64, TEncoding.ASCII);
  try
    Output := TMemoryStream.Create;
    try
      Output.CopyFrom(Input, Input.Size);
      Output.Position := 0;
      Result := TBitmap.Create;
      try
        Result.LoadFromStream(Output);
      except
        Result.Free;
        raise;
      end;
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;


exemplo de uso
var
  Bitmap: TBitmap;
  Base64: string;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile('image.bmp');
    Base64 := BitmapToBase64(Bitmap);
    Bitmap.Free;
    Bitmap := Base64ToBitmap(Base64);
    Bitmap.SaveToFile('image2.bmp');
  finally
    Bitmap.Free;
  end;
end;
