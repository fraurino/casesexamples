unit Base64Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function ImageToBase64(const AImage: TGraphic): string;
function Base64ToImage(const ABase64: string): TGraphic;

implementation

uses
  base64; // Lazarus built-in unit for base64 encoding and decoding

function ImageToBase64(const AImage: TGraphic): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    AImage.SaveToStream(Stream);
    Stream.Position := 0;
    Result := EncodeBase64(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
end;

function Base64ToImage(const ABase64: string): TGraphic;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    DecodeBase64(ABase64, Stream);
    Stream.Position := 0;
    if CompareText(Copy(ABase64, 1, 10), 'data:image') = 0 then
      Result := TPicture.Create
    else
      Result := TJPEGImage.Create;
    Result.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.



example

uses
  Base64Utils;

procedure TForm1.Button1Click(Sender: TObject);
var
  Image: TPicture;
  Base64String: string;
begin
  // Load an image from a file
  Image := TPicture.Create;
  Image.LoadFromFile('myimage.jpg');
  try
    // Convert the image to base64
    Base64String := ImageToBase64(Image);

    // Do something with the base64 string...

    // Convert the base64 string back to an image
    Image.Assign(Base64ToImage(Base64String));

    // Do something with the image...
  finally
    Image.Free;
  end;
end;
