uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Clipbrd, // para acessar o Clipboard
  Vcl.Imaging.jpeg, // se quiser suportar JPEG
  dialogs, forms,
  Vcl.Imaging.pngimage, // se quiser suportar PNG
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls;
  
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
 var
  Data: THandle;
  MemStream: TMemoryStream;
  PNG: TPngImage;
  JPEG: TJPEGImage;
begin
  if (Key = Ord('V')) and (ssCtrl in Shift) then
  begin
    if Clipboard.HasFormat(CF_BITMAP) then
    begin
      Image1.Picture.Assign(Clipboard);
      ShowMessage('Imagem colada como Bitmap!');
      Exit;
    end;

    // Tenta colar como PNG
    const CF_PNG = 49406; // Formato registrado para PNG (nem sempre disponível)
    if Clipboard.HasFormat(CF_PNG) then
    begin
      Data := Clipboard.GetAsHandle(CF_PNG);
      if Data <> 0 then
      begin
        MemStream := TMemoryStream.Create;
        try
          MemStream.SetSize(GlobalSize(Data));
          Move(Pointer(GlobalLock(Data))^, MemStream.Memory^, MemStream.Size);
          GlobalUnlock(Data);
          PNG := TPngImage.Create;
          try
            MemStream.Position := 0;
            PNG.LoadFromStream(MemStream);
            Image1.Picture.Assign(PNG);
            ShowMessage('Imagem colada como PNG!');
            Exit;
          finally
            PNG.Free;
          end;
        finally
          MemStream.Free;
        end;
      end;
    end;

    // Tenta colar como JPEG
    if Clipboard.HasFormat(CF_PRIVATEFIRST) then // fallback para formatos alternativos
    begin
      Data := Clipboard.GetAsHandle(CF_PRIVATEFIRST);
      if Data <> 0 then
      begin
        MemStream := TMemoryStream.Create;
        try
          MemStream.SetSize(GlobalSize(Data));
          Move(Pointer(GlobalLock(Data))^, MemStream.Memory^, MemStream.Size);
          GlobalUnlock(Data);
          JPEG := TJPEGImage.Create;
          try
            MemStream.Position := 0;
            try
              JPEG.LoadFromStream(MemStream);
              Image1.Picture.Assign(JPEG);
              ShowMessage('Imagem colada como JPEG!');
              Exit;
            except
              on E: Exception do
                ShowMessage('Erro ao interpretar JPEG: ' + E.Message);
            end;
          finally
            JPEG.Free;
          end;
        finally
          MemStream.Free;
        end;
      end;
    end;

    ShowMessage('Não foi possível colar uma imagem compatível.');
  end;
