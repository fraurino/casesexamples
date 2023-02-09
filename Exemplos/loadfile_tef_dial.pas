const
filetef : string ='C:\Tef_Dial\Resp\intpos_bkp';

{$REGION 'GetValue file TEF_Dial'}

type
  TData = record
    Code: string;
    Value: string;
  end;

type
  TDataArray = array of TData;

type
  TDataFile = class
  private
    FData: TDataArray;
    procedure LoadFromFile(const FileName: string);
    function GetDataValue(const Code: string): string;
  public
    constructor Create(const FileName: string);
    property Data[const Code: string]: string read GetDataValue; default;
  end;

constructor TDataFile.Create(const FileName: string);
begin
  LoadFromFile(FileName);
end;

procedure TDataFile.LoadFromFile(const FileName: string);
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    SetLength(FData, Lines.Count);
    for I := 0 to Lines.Count - 1 do
    begin
      FData[I].Code  := Copy(Lines[I], 1, 7);
      FData[I].Value := Copy(Lines[I], 9, Length(Lines[I]) - 8);
    end;
  finally
    Lines.Free;
  end;
end;

function TDataFile.GetDataValue(const Code: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(FData) - 1 do
  begin
    if FData[I].Code = Code then
    begin

      Result := trim(copy(FData[I].Value, 2 , Length(FData[I].Value) ))  ;
      Break;
    end;
  end;
end;

{$ENDREGION}
{$REGION 'GetLine  File TEF_Dial'}

type
  TDados = class
  private
    Flist: TStringList;
    function GetValue(ACodigo: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: String);
    property Value[ACodigo: String]: String read GetValue; default;
  end;

constructor TDados.Create;
begin
  inherited;
  Flist := TStringList.Create;
end;

destructor TDados.Destroy;
begin
  Flist.Free;
  inherited;
end;

procedure TDados.LoadFromFile(FileName: String);
begin
  Flist.LoadFromFile(FileName);
end;

function TDados.GetValue(ACodigo: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Flist.Count - 1 do
  begin
    if Pos(ACodigo, Flist[I]) = 1 then
    begin
      Result :=  Copy(Flist[I], Length(ACodigo) + 3, Length(Flist[I]));

      //Copy(Flist[I], Length(ACodigo) + 3, Length(Flist[I]));
      Break;
    end;
  end;
end;


{$ENDREGION}



procedure Tfrmtest.Button2Click(Sender: TObject);
  procedure Writeln (value : string );
  begin
    memo1.Lines.Add(value);
  end;
var
  DataFile: TDataFile;
begin
  DataFile := TDataFile.Create(filetef);
  try
    Writeln(DataFile['000-000']);
    Writeln(DataFile['001-000']);
    Writeln(DataFile['002-000']);
    Writeln(DataFile['003-000']);
    Writeln(DataFile['027-000']);
    // e assim por diante...
  finally
    DataFile.Free;
  end;
end;


procedure Tfrmtest.Button4Click(Sender: TObject);
 procedure Writeln (value : string );
  begin
    memo1.Lines.Add(value);
  end;
var
  Dados: TDados;
begin
  Dados := TDados.Create;
  try
    Dados.LoadFromFile(filetef);
    Writeln('740-000: '+ Dados['000-000']);
    Writeln('027-000: '+ Dados['027-000']);
  finally
    Dados.Free;
  end;
end;
