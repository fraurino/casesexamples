{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Soap.EncdDecd;

function Base64FromBitmap(Bitmap: TBitmap): string;
var
  Input: TBytesStream;
  Output: TStringStream;
begin
  Input := TBytesStream.Create;
  try
    Bitmap.SaveToStream(Input);
    Input.Position := 0;
    Output := TStringStream.Create('', TEncoding.ASCII);
    try
      Soap.EncdDecd.EncodeStream(Input, Output);
      Result := Output.DataString;
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

function BitmapFromBase64(const base64: string): TBitmap;
var
  Input: TStringStream;
  Output: TBytesStream;
begin
  Input := TStringStream.Create(base64, TEncoding.ASCII);
  try
    Output := TBytesStream.Create;
    try
      Soap.EncdDecd.DecodeStream(Input, Output);
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

var
  Bitmap: TBitmap;
  s: string;

begin
  Bitmap := TBitmap.Create;
  Bitmap.SetSize(100,100);
  Bitmap.Canvas.Brush.Color := clRed;
  Bitmap.Canvas.FillRect(Rect(20, 20, 80, 80));
  s := Base64FromBitmap(Bitmap);
  Bitmap.Free;
  Bitmap := BitmapFromBase64(s);
  Bitmap.SaveToFile('C:\desktop\temp.bmp');
end.
