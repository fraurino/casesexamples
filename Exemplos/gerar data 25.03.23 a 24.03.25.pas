var
  currentDate: TDate;
  endDate: TDate;

begin
  currentDate := EncodeDate(2023, 2, 25);
  endDate     := EncodeDate(2025, 2, 25);

  while currentDate <= endDate do
  begin
    currentDate := IncMonth(currentDate, 1);
    currentDate := EncodeDate(System.DateUtils.YearOf(currentDate), System.DateUtils.MonthOf(currentDate), 25);
    Memo1.Lines.Add(FormatDateTime('dd/mm/yyyy', currentdate)) ;
  end;
