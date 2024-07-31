uses
  Math, ACBrUtil;

function TipoValorArredondamento(Valor: Currency; const Tipo: TTipoArredondamento; QtdeCasaDecimais: Integer = 2): Currency;
begin
  case Tipo of
    taDefault                 : Result := Valor; // Retorna o valor original
    taArredondamento          : Result := math.RoundTo(Valor, -QtdeCasaDecimais); // Arredondamento com precisão
    taArredondamentoSimples   : Result := math.SimpleRoundTo(Valor, -QtdeCasaDecimais); // Arredondamento simples
    taArredondamentoABNT      : Result := ACBrUtil.RoundABNT(Valor, -QtdeCasaDecimais); // Arredondamento ABNT
    taArredondamentoSimplesEX : Result := ACBrUtil.SimpleRoundToEX(Valor, -QtdeCasaDecimais); // Arredondamento simples EX
  else
    raise Exception.CreateFmt('Tipo de arredondamento inesperado: %d', [Ord(Tipo)]);
  end;
end;


// Exemplo de uso
var
  valor: Currency;
  tipoArredondamento: TTipoArredondamento;
begin
  tipoArredondamento := taArredondamentoSimples; // Defina o tipo de arredondamento desejado
  valor := TipoValorArredondamento(valorDesconto, tipoArredondamento);// Chame a função com o tipo de arredondamento
end;

