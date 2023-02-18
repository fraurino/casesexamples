unit uTestChatOpenAI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, ZAbstractRODataset,
  ZAbstractDataset, ZDataset, Datasnap.Provider, Datasnap.DBClient, Vcl.Buttons,
  Vcl.Grids, Vcl.DBGrids , Vcl.StdCtrls, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, System.Diagnostics, System.SyncObjs, System.Threading ;

type
  TuTest = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSetProvider1: TDataSetProvider;
    DataSource1: TDataSource;
    ClientDataSet2: TClientDataSet;
    DataSetProvider2: TDataSetProvider;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    procedure p ;

  public
    { Public declarations }
  end;

var
  uTest: TuTest;

implementation

{$R *.dfm}

// -------------------------------------------------------------------------------------------------
//https://github.com/amarildolacerda/exemplos/blob/master/Dia11_Threading_TParallel/wProjExemplo.pas
// -------------------------------------------------------------------------------------------------

{$REGION 'WaitForAllEx'}
// criando WaitForAllEx
Type
  TTaskHelper = class helper for TTask
  private type
    TUnsafeTaskEx = record
    private
      [Unsafe]
      // preciso de um record UNSAFE para nao incrementar o RefCount da Interface
      FTask: TTask;
    public
      property Value: TTask read FTask write FTask;
    end;
  public
    class function WaitForAllEx(AArray: Array of ITask;
    ATimeOut: int64 = INFINITE): boolean;
  end;

class function TTaskHelper.WaitForAllEx(AArray: array of ITask;
ATimeOut: int64 = INFINITE): boolean;
var
  FEvent: TEvent;
  task: TUnsafeTaskEx;
  i: integer;
  taskInter: TArray<TUnsafeTaskEx>;
  completou: boolean;
  Canceled, Exceptions: boolean;
  ProcCompleted: TProc<ITask>;
  LHandle: THandle;
  LStop: TStopwatch;
begin
  LStop := TStopwatch.StartNew;
  ProcCompleted := procedure(ATask: ITask)
    begin
      FEvent.SetEvent;
    end;

  Canceled := false;
  Exceptions := false;
  result := true;
  try
    for i := low(AArray) to High(AArray) do
    begin
      task.Value := TTask(AArray[i]);
      if task.Value = nil then
        raise EArgumentNilException.Create('Wait Nil Task');

      completou := task.Value.IsComplete;
      if not completou then
      begin
        taskInter := taskInter + [task];
      end
      else
      begin
        if task.Value.HasExceptions then
          Exceptions := true
        else if task.Value.IsCanceled then
          Canceled := true;
      end;
    end;

    try
      FEvent := TEvent.Create();
      for task in taskInter do
      begin
        try
          FEvent.ResetEvent;
          if LStop.ElapsedMilliseconds > ATimeOut then
            break;
          LHandle := FEvent.Handle;
          task.Value.AddCompleteEvent(ProcCompleted);
          while not task.Value.IsComplete do
          begin
            if LStop.ElapsedMilliseconds > ATimeOut then
              break;
            if MsgWaitForMultipleObjectsEx(1, LHandle,
              ATimeOut - LStop.ElapsedMilliseconds, QS_ALLINPUT, 0)
              = WAIT_OBJECT_0 + 1 then
              application.ProcessMessages;
          end;
          if task.Value.IsComplete then
          begin
            if task.Value.HasExceptions then
              Exceptions := true
            else if task.Value.IsCanceled then
              Canceled := true;
          end;
        finally
          task.Value.removeCompleteEvent(ProcCompleted);

        end;
      end;
    finally
      FEvent.Free;
    end;
  except
    result := false;
  end;

  if (not Exceptions and not Canceled) then
    Exit;
  if Exceptions or Canceled then
    raise EOperationCancelled.Create
      ('One Or More Tasks HasExceptions/Canceled');

end;

{$ENDREGION}

procedure CopyRecord(DataSetOrigem, DataSetDestino: TClientDataSet );
var
  FieldOrigem, FieldDestino: TField;
begin
  DataSetDestino.Append;
  for FieldOrigem in DataSetOrigem.Fields do
  begin
    FieldDestino := DataSetDestino.FieldByName(FieldOrigem.FieldName);
    FieldDestino.Value := FieldOrigem.Value;
  end;
  DataSetDestino.Post;
end;

procedure CopyAllRecords(DataSetOrigem, DataSetDestino: TClientDataSet);

var
  FieldOrigem: TField;
  FieldDestino: array of TField;
  i: Integer;
begin
  SetLength(FieldDestino, DataSetOrigem.FieldCount);
  DataSetOrigem.DisableControls;
  for i := 0 to DataSetOrigem.FieldCount - 1 do
    FieldDestino[i] := DataSetDestino.FieldByName(DataSetOrigem.Fields[i].FieldName);

  DataSetDestino.DisableControls;
  try
    DataSetOrigem.First;
    while not DataSetOrigem.Eof do
    begin
      DataSetDestino.Append;
      for i := 0 to DataSetOrigem.FieldCount - 1 do
      begin
        FieldOrigem := DataSetOrigem.Fields[i];
        FieldDestino[i].Value := FieldOrigem.Value;
      end;
      DataSetDestino.Post;

      DataSetOrigem.Next;
    end;
  finally
    DataSetDestino.EnableControls;
    DataSetOrigem.EnableControls;

    try
     DataSetDestino.Refresh ;
    except
     //clean cache
     DataSetDestino.MergeChangeLog ;
    end;

  end;
end;

procedure TuTest.FormCreate(Sender: TObject);
var
 i :Integer;
begin
  ClientDataSet1.Close;
  ClientDataSet1.FieldDefs.Clear;
  ClientDataSet1.FieldDefs.add('codigo',ftInteger);
  ClientDataSet1.FieldDefs.add('nome',ftString,50);
  ClientDataSet1.CreateDataSet;

  ClientDataSet2.Close;
  ClientDataSet2.FieldDefs.Clear;
  ClientDataSet2.FieldDefs.add('codigo',ftInteger);
  ClientDataSet2.FieldDefs.add('nome',ftString,50);
  ClientDataSet2.CreateDataSet;
  for i := 0 to 100000 do
  begin
     ClientDataSet1.Append;
     ClientDataSet1.FieldByName('codigo').Value := i;
     ClientDataSet1.FieldByName('nome').Value := IntToStr(i) + ' registros';
     ClientDataSet1.post;
  end;
end;

procedure TuTest.p;
var
Inicio: TDateTime;
  Fim: TDateTime;
begin
  Inicio := Now;
  CopyAllRecords (ClientDataSet1, ClientDataSet2);
  Fim := Now;
  ShowMessage(Format('inseridos '+InttoStr(ClientDataSet2.RecordCount)+ ' registro(s) em %s segundos.',
  [FormatDateTime('ss', Fim - Inicio)]));
end;

procedure TuTest.SpeedButton1Click(Sender: TObject);
begin
  CopyRecord (ClientDataSet1, ClientDataSet2 );
end;

procedure TuTest.SpeedButton2Click(Sender: TObject);
var
  Inicio: TDateTime;
  Fim: TDateTime;
  Tasks: array [0..2] of ITask;
begin

  if CheckBox1.Checked then    //in task
  begin
   if CheckBox2.Checked then  // time in task
     begin
       Tasks[0] := TTask.Create(
        procedure
        begin
          TThread.Queue(nil,
            procedure
            begin
              Inicio := Now;
              CopyAllRecords (ClientDataSet1, ClientDataSet2);
                Fim := Now;
              ShowMessage(Format('inseridos '+InttoStr(ClientDataSet2.RecordCount)+ ' registro(s) em %s segundos.',
              [FormatDateTime('ss', Fim - Inicio)]));
            end);
        end);
       Tasks[0].Start;

        if CheckBox2.Checked then
        TTask.WaitForAllEx(Tasks)
        else
        TTask.WaitForAll(Tasks);
     end
   else
     begin        // not time task  procedure p
        Inicio := Now;
        Tasks[0] := TTask.Create( p );
        Tasks[0].Start;
        Fim := Now;
        ShowMessage(Format('inseridos '+InttoStr(ClientDataSet2.RecordCount)+ ' registro(s) em %s segundos.',
        [FormatDateTime('ss', Fim - Inicio)]));
     end;
  end
  else
  begin   // not task
    Inicio := Now;
    CopyAllRecords (ClientDataSet1, ClientDataSet2);
    Fim := Now;
    ShowMessage(Format('inseridos '+InttoStr(ClientDataSet2.RecordCount)+ ' registro(s) em %s segundos.',
    [FormatDateTime('ss', Fim - Inicio)]));
  end;

end;


end.
