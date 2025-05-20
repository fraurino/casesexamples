{
💡 Explicação prática:
✍🏽️Se o usuário selecionar UN como método da venda, ele deverá informar:
  - Quantidade do item (ex: 10 bananas)
  - Peso total manualmente/automaticamente via balanca (ex: 1,800 kg)
   - e será calculado os dados....
   
✍🏽️Se o usuário selecionar KG, ele só precisa informar:
Quantidade em quilos (ex: 1,800) e com base no valor do quilo, será calculado automaticamente;

}
procedure CalculaVenda;
var
  Qtde: Double;
  PesoTotal, PrecoPorKg, PrecoUnitario, TotalVenda, FatorUnParaKg: Double;
  UnidadeVenda: string;
begin
  UnidadeVenda := cbUnidade.Text;

  // Valida preço por quilo
  if not TryStrToFloat(edtPrecoKg.Text, PrecoPorKg) then
  begin
    ShowMessage('Informe um preço válido por Kg.');
    Exit;
  end;

  // Valida quantidade
  if not TryStrToFloat(edtQuantidade.Text, Qtde) or (Qtde <= 0) then
  begin
    ShowMessage('Informe uma quantidade válida.');
    Exit;
  end;

  if UnidadeVenda = 'UN' then
  begin
    // Valida peso total informado
    if not TryStrToFloat(edtPesoTotal.Text, PesoTotal) or (PesoTotal <= 0) then
    begin
      ShowMessage('Informe o peso total correspondente.');
      Exit;
    end;

    FatorUnParaKg := PesoTotal / Qtde;
    PrecoUnitario := PrecoPorKg * FatorUnParaKg;
    TotalVenda    := PesoTotal * PrecoPorKg;
  end
  else if UnidadeVenda = 'KG' then
  begin
    PesoTotal := Qtde;
    PrecoUnitario := PrecoPorKg;
    TotalVenda := Qtde * PrecoPorKg;
    FatorUnParaKg := 0;
  end
  else
  begin
    ShowMessage('Selecione uma unidade válida (UN ou KG).');
    Exit;
  end;

  lblPeso.Caption  := Format('Peso: %.3f Kg', [PesoTotal]);
  lblPreco.Caption := Format('Preço Unitário: R$ %.2f', [PrecoUnitario]);
  lblTotal.Caption := Format('Total: R$ %.2f', [TotalVenda]);

  if UnidadeVenda = 'UN' then
    lblMedia.Caption := Format('Média: %.3f Kg por unidade', [FatorUnParaKg])
  else
    lblMedia.Caption := 'Média: - 0,00';
end;
