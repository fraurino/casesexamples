uses
  IniFiles, SysUtils, IdHTTP;

function ReadConfigValueFromURL(const AURL: string): string;
var
  Ini: TIniFile;
  Response: TStringStream;
  Client: TIdHTTP;
begin
  Result := '';
  Response := TStringStream.Create('', TEncoding.UTF8);
  Ini := TIniFile.Create('');
  Client := TIdHTTP.Create;
  try
    Client.Get(AURL, Response);
    Response.Seek(0, soFromBeginning);
    Ini.LoadFromStream(Response);
    Result := Ini.ReadString('firebird', 'database', '');
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
  FreeAndNil(Client);
  FreeAndNil(Response);
  FreeAndNil(Ini);
end;


example
var
  ConfigValue: string;
begin
  ConfigValue := ReadConfigValueFromURL('https://www.site.com.br/file.ini');
  Writeln(ConfigValue);
end;
