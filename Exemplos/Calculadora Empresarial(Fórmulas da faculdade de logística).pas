procedure TForm1.CalcularCicloDoProcessoClick(Sender: TObject);
    var n1,n2,resultado:real; //declaração de variáveis do tipo real(aceita numero inteiro e com vírgula)
    begin
    n1:=StrToFloat(TempoTrabalhadoEmHoras.Text);// converte n1(numero1) para numero flutuante
    n2:=StrToFloat(NumeroDeProdutosBons.Text);//converte n2(numero2)para numero flutuante
    resultado:=n1/n2; //Dividi a variável n1(numero1) e n2(numero2
    ResultadoDoCicloDoProcesso.Text:=FloatToStr(resultado);//mostra o resultado convertendo numero flutuante(Float) para texto(String)
    procedure TForm1.CalcularTaxaDeProducaoClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(TempoTrabalhado.Text);
    n2:=StrToFloat(NumeroDeProdutosBons.Text);
    resultado:=n2/n1;
    ResultadoTaxaDeProducao.Text:=FloatToStr(resultado);
   
    procedure TForm1.CalcularTaxaDeProducaoPorRecursoClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(TaxaDeProducao.Text);
    n2:=StrToFloat(NumeroDeFuncionarios.Text);
    resultado:=n1/n2;
    ResultadoTaxaDeProducaoPorRecurso.Text:=FloatToStr(resultado);
   
    procedure TForm1.CalcularGiroDeEstoqueClick(Sender: TObject);
    var n1,n2,n3,resultado:real;
    begin
    n1:=StrToFloat(NumeroDeProdutosBons.Text);
    n2:=StrToFloat(TotalDeMateriais.Text);
    n3:=StrToFloat(TotalDeProdutos.Text);
    resultado:=n1*12/n2/n3;
    ResultadoDoGiroDoEstoque.Text:=FloatToStr(resultado);
   
    procedure TForm1.CalcularFaltaDeQualidadeClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(NumeroDeProdutosComDefeitos.Text);
    n2:=StrToFloat(TotalProduzido.Text);
    resultado:=n1/n2;
    ResultadoDaFaltaDeQualidade.Text:=FloatToStr(resultado);
   
   
    procedure TForm1.SairClick(Sender: TObject);
    begin
    Form1.Close;
   
    procedure TForm1.CalcularCPVClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(NumeroDeProdutosBons.Text);
    n2:=StrToFloat(ValorUnitario.Text);
    resultado:=n1*n2;
    ResultadoCPV.Text:=FloatToStr(resultado);
   
    procedure TForm1.CalcularCustoDaFaltaDeQualidadeClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(NumeroDeProdutosComDefeitos.Text);
    n2:=StrToFloat(ValorUnitario.Text);
    resultado:=n1*n2;
    ResultadoDoCustoDaFaltaDeQualidade.Text:=FloatToStr(resultado);
    Botão Apagar:
    TempoTrabalhado.Text:='';
    TempoTrabalhadoEmHoras.Text:='';
    NumeroDeProdutosBons.Text:='';
    resultadoDoCicloDoProcesso.Text:='';
    TaxaDeProducao.Text:='';
    ResultadoTaxaDeProducao.Text:='';
    NumeroDeFuncionarios.Text:='';
    ResultadoTaxaDeProducaoPorRecurso.Text:='';
    TotalDeMateriais.Text:='';
    TotalDeProdutos.Text:='';
    ResultadoDoGiroDoEstoque.Text:='';
    NumeroDeProdutosComDefeitos.Text:='';
    TotalProduzido.Text:='';
    ResultadoDaFaltaDeQualidade.Text:='';
    ResultadoCPV.Text:='';
    ResultadoDoCustoDaFaltaDeQualidade.Text:='';
   
    Form1. close; //Fecha o Form
   
    procedure TForm2.CalcularCustoDeCarregarEstoqueClick(Sender: TObject);
    var n1,n2,n3,resultado:real;
    begin
    n1:=StrToFloat(EstoqueDeProducaoEmProcesso.Text);
    n2:=StrToFloat(ValorUnitario.Text);
    n3:=StrToFloat(TaxaDeJuros.Text);
    resultado:=n1*n2*n3;
    ResultadoDoCustoDeCarregarEstoque.Text:=FloatToStr(resultado);
   
   
    end;
   
    procedure TForm2.CalcularCustoFixoClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(SalarioDoColaborador.Text);
    n2:=StrToFloat(DespesasGerais.Text);
    resultado:=n1+n2;
    ResultadoDoCustoFixo.Text:=FloatToStr(resultado);
   
    end;
   
    procedure TForm2.CalcularCustoTotalClick(Sender: TObject);
    var n1,n2,n3,n4,resultado:real;
    begin
    n1:=StrToFloat(CPV.Text);
    n2:=StrToFloat(CustoDaQualidade.Text);
    n3:=StrToFloat(CustoDoEstoque.Text);
    n4:=StrToFloat(CustoFixo.Text);
    resultado:=n1+n2+n3+n4;
    CustoTotal.Text:=FloatToStr(resultado);
   
    end;
   
    procedure TForm2.CalcularCustoUnitarioClick(Sender: TObject);
    var n1,n2,resultado:real;
    begin
    n1:=StrToFloat(CustoTotal.Text);
    n2:=StrToFloat(NumeroDeProdutosBons.Text);
    resultado:=n1/n2;
    ResultadoDoCustoUnitario.Text:=FloatToStr(resultado);
   
   
    end;
   
    procedure TForm2.SairClick(Sender: TObject);
    begin
    Form2.Close;// Fecha o Form2
    end;
   
    procedure TForm2.ApagarClick(Sender: TObject);
    begin
    EstoqueDeProducaoEmProcesso.Text:='';
    ValorUnitario.Text:='';
    TaxaDeJuros.Text:='';
    ResultadoDoCustoDeCarregarEstoque.Text:='';
    SalarioDoColaborador.Text:='';
    DespesasGerais.Text:='';
    ResultadoDoCustoFixo.Text:='';
    CPV.Text:='';
    CustoDaQualidade.Text:='';
    CustodoEstoque.Text:='';
    CustoFixo.Text:='';
    CustoTotal.Text:='';
    NumeroDeProdutosBons.Text:='';
    ResultadoDoCustoUnitario.Text:='';
    end;
