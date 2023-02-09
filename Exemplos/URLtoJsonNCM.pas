
  procedure DownloadFileJSON(const AURL, AFileName: string);
  var
    HTTP: TIdHTTP;
    Stream: TMemoryStream;
  begin
    HTTP := TIdHTTP.Create(nil);
    HTTP.HandleRedirects := true;
    HTTP.AllowCookies := True;
    HTTP.RedirectMaximum := 35;
    HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 5.1; rv:2.0b8) Gecko/20100101 Firefox/4.' + '0b8';
    HTTP.HTTPOptions := [hoForceEncodeParams];
    Stream := TMemoryStream.Create;
    try
      HTTP.Get(AURL, Stream);
      Stream.SaveToFile(AFileName);
    finally
      Stream.Free;
      HTTP.Free;
    end;
  end;

  procedure LoadJSONtoURL(const AURL: string);
  var
    HTTP: TIdHTTP;
    JSONObject: TJSONObject;
    ClientDataSet: TClientDataSet;
    Nomenclaturas: TJSONArray;
    I: Integer;
    Inicio: TDateTime;
    Fim: TDateTime;
  begin
  Inicio := Now;
    HTTP := TIdHTTP.Create(nil);
    HTTP.HandleRedirects := true;
    HTTP.AllowCookies := True;
    HTTP.RedirectMaximum := 35;
    HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 5.1; rv:2.0b8) Gecko/20100101 Firefox/4.' + '0b8';
    try
      JSONObject := TJSONObject.ParseJSONValue(HTTP.Get(AURL)) as TJSONObject;
      try
        ClientDataSet := TClientDataSet.Create(nil);
          ClientDataSet.FieldDefs.Add('Codigo', ftString, 50, False);
          ClientDataSet.FieldDefs.Add('Descricao', ftString, 50, False);
          ClientDataSet.FieldDefs.Add('Data_Inicio', ftDate, 0, False);
          ClientDataSet.FieldDefs.Add('Data_Fim', ftDate, 0, False);
          ClientDataSet.FieldDefs.Add('Tipo_Ato', ftString, 50, False);
          ClientDataSet.FieldDefs.Add('Numero_Ato', ftString, 50, False);
          ClientDataSet.FieldDefs.Add('Ano_Ato', ftString, 50, False);
          ClientDataSet.CreateDataSet;

          Nomenclaturas := JSONObject.GetValue('Nomenclaturas') as TJSONArray;
          for I := 0 to Nomenclaturas.Count - 1 do
          begin
            ClientDataSet.Append;


            ClientDataSet.FieldByName('Codigo').AsString        := Nomenclaturas.Items[I].FindValue('Codigo').Value;
            ClientDataSet.FieldByName('Descricao').AsString     := UTF8ToString(Nomenclaturas.Items[I].FindValue('Descricao').Value);
            ClientDataSet.FieldByName('Data_Inicio').AsDateTime := StrToDate(Nomenclaturas.Items[I].FindValue('Data_Inicio').Value);
            ClientDataSet.FieldByName('Data_Fim').AsDateTime    := StrToDate(Nomenclaturas.Items[I].FindValue('Data_Fim').Value);
            ClientDataSet.FieldByName('Tipo_Ato').AsString      := Nomenclaturas.Items[I].FindValue('Tipo_Ato').Value;
            ClientDataSet.FieldByName('Numero_Ato').AsString    := Nomenclaturas.Items[I].FindValue('Numero_Ato').Value;
            ClientDataSet.FieldByName('Ano_Ato').AsString       := Nomenclaturas.Items[I].FindValue('Ano_Ato').Value;
            ClientDataSet.Post;
          end;

          ClientDataSet.First;
          DataSource1.DataSet := ClientDataSet; {configurando o clientdataset ao datasource que está ligado ao dbgrid do seu formulario}

      finally
        JSONObject.Free;
        ClientDataSet.free;
      end;
    finally
      HTTP.Free;
    end;
    Fim := Now;
    ShowMessage(Format('baixados '+InttoStr(ClientDataSet.RecordCount)+ ' registro(s) em %s segundos.',
    [FormatDateTime('ss', Fim - Inicio)]));
  end;


var
Url : string;
begin
   url := 'https://portalunico.siscomex.gov.br/classif/api/publico/nomenclatura/download/json';
   //DownloadFileJSON(url, 'nomenclaturas.json'); {download to File json}
   LoadJSONtoURL(url);
end;
