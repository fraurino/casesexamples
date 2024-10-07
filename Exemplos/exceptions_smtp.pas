
class function CapturaCodigoErroSMTP( const MensagemErro: string): Integer;
var
  CodigoStr: string;
begin
  // Extrai os três primeiros caracteres da mensagem de erro
  CodigoStr := Copy(MensagemErro, 1, 3);
  // Tenta converter os três primeiros caracteres para número
  if TryStrToInt(CodigoStr, Result) then
    Exit
  else
    Result := -1; // Retorna -1 se a conversão falhar (ou seja, código inválido)
end;

class function exceptions_smtp(codigo: Integer; const provedor: string): string;
  function retorno(texto: string): string;
  var
    Builder: TStringBuilder;
    i: Integer;
  begin
    Builder := TStringBuilder.Create;
    try
      for i := 1 to Length(texto) do
        if (texto[i] <> ' ') or ((i > 1) and (texto[i - 1] <> ' ')) then
          Builder.Append(texto[i]);
      Result := Trim(Builder.ToString);
    finally
      Builder.Free;
    end;
  end;

begin
   Result := '';

   {$REGION 'OUTLOOK'}
   if (Pos('outlook', provedor) > 0) then
   begin
      case codigo of
        200: Result := retorno('sucess	e-mail foi entregue corretamente pelo provedor');
        400: Result := retorno('Status undefined	caixa do remetente atingiu limite de envios. Caso tenha algum tipo de aviso sobre reputação do IP, o destinatário pode estar bloqueando envios desse remetente.');
        432: Result := retorno('System not accepting network messages	Falha do destinatário ou bloqueio');
        441: Result := retorno('No answer from the host	Destinatário está com problemas no recebimento que partem de seu servidor.');
        442: Result := retorno('Bad connection - Falha de recebimento por parte do destino ou algum bloqueio por parte do mesmo (testar com e-mail de domínio público como Outlook)');
        447: Result := retorno('Delivery time expired - Falha de comunicação com o servidor do destino. Se resolve, em geral, automaticamente, mas não é resolvido pelo servidor de disparos, e sim pelo de recebimento.');
        471: Result := retorno('Delivery not authorized - Entrega não aceita pelo servidor do destino. Testar com domínio público.');
        501	: Result := retorno('Syntax error in parameter or arguments	O Sender Rewriting Scheme(SRS), de um provedor de e-mail, utiliza verificação de SPF. Isso impede que seja rejeitada a mensagem no servidor do destino.');
        550	: Result := retorno('User unknown	Destinatário não existe no servidor. Pode ser um erro de digitação na hora de inserir a conta.');
        553	: Result := retorno('Erro in DNS	Entrada de DNS SMTP não consta no seu servidor, e a mensagem é rejeitada.');
        554	: Result := retorno('Relay access denied-Ocorre por dois motivos: Dominio recente e com DNS propagando; Entrada SMTP não está constando no servidor;Em ambos os casos, entre em contato com o suporte da RedeHost para que seja verificado.');
        510	: Result := retorno('Address reject	a caixa postal do destino não existe, podendo ser erro de digitação ou conta deletada.');
        511	: Result := retorno('Bad destinantion mailbox or user unknown rejecting	Dominio não existe, ou foi digitado de maneira incorreta');
        512	: Result := retorno('Bad destination system: no such domain-Dominio não existe, ou foi digitado de maneira incorreta');
        513	: Result := retorno('Bad recipient address syntax-Algum caracter está errado no e-mail do destino. Verificar se não existem caracteres especiais (parênteses ou aspas por exemplo).');
        522	: Result := retorno('Mailbox full ou mail quota exceeded-Caixa do destinatário está cheia e não aceita mais mensagens.');
        530	: Result := retorno('Mail server permanently rejected message	Bloqueio por parte do destino ao domínio do remetente. Realizar teste enviando de domínio público.');
        532	: Result := retorno('System not accepting network messages	Bloqueio por parte do destino ao domínio do remetente. Realizar teste enviando de domínio público.');
        534	: Result := retorno('Too long header-Ocorre caso tenha muitos contatos de e-mail (como no CC  ou CO) e caso exista uma troca de mensagens e o cabeçalho do e-mail fique muito grande e com muitos campos.');
        544	: Result := retorno('Internal Domain lookup failed	Problema no domínio do destinatário');
        546	: Result := retorno('Routing Loop detected ou loops back to myself-ocorre quando uma caixa de e-mail é configurada para enviar cópias para uma caixa de e-mail e a mesma também está configurada para responder para o remetente, gerando um loop de e-mails.');
        571	: Result := retorno('Sorry, that domain isn’t in my list of allowed rcpthosts ou we do not relay, account limits apply	Seu gerenciador de e-mails não tem a opção de autenticação habilitada, ou a conta superou o limite de envios por hora/dia');
        577	: Result := retorno('Invalid HELO format	Servidor de destino não reconhece o formato HELO utilizado no e-mail do remente, precisando que o meso consulte o administrador do servidor.');
        5717 : Result := retorno('Sorry, no mailbox here by that name-Endereço de e-mail incorreto ou conta não existente');
      else
        Result := '';
      end;
   end;

  {$ENDREGION}

   {$REGION 'HOTMAIL' }
  if (Pos('hotmail', provedor) > 0) then
  begin
    case codigo of
      101:  Result := retorno('Erro 101: O servidor é incapaz de se conectar. Tente mudar o nome do servidor ou a porta.');
      111:  Result := retorno('Erro 111: Conexão recusada ou incapacidade de abrir um fluxo SMTP.');
      211:  Result := retorno('Erro 211: Mensagem de status do sistema ou resposta de ajuda.');
      214:  Result := retorno('Erro 214: Resposta ao comando HELP.');
      220:  Result := retorno('Erro 220: O servidor está pronto.');
      221:  Result := retorno('Erro 221: O servidor está fechando o canal de transmissão.');
      250:  Result := retorno('Erro 250: Ação de e-mail concluída com sucesso.');
      251:  Result := retorno('Erro 251: O usuário não está local, será encaminhado.');
      252:  Result := retorno('Erro 252: O servidor não pode verificar o usuário, mas tentará entregar.');
      354:  Result := retorno('Erro 354: Resposta ao comando DATA. Pronto para receber corpo da mensagem.');
      421:  Result := retorno('Erro 421: O serviço não está disponível devido a um problema de conexão.');
      422:  Result := retorno('Erro 422: Caixa postal do destinatário excedeu o limite de armazenamento.');
      431:  Result := retorno('Erro 431: Não há espaço suficiente no disco ou "out of memory".');
      432:  Result := retorno('Erro 432: Fila de e-mails de entrada do Exchange Server foi parada.');
      441:  Result := retorno('Erro 441: O servidor do destinatário não está respondendo.');
      442:  Result := retorno('Erro 442: A conexão foi interrompida durante a transmissão.');
      446:  Result := retorno('Erro 446: A contagem máxima de saltos foi excedida (loop interno).');
      447:  Result := retorno('Erro 447: Mensagem de saída expirou por problemas no servidor de entrada.');
      449:  Result := retorno('Erro 449: Erro de roteamento.');
      450:  Result := retorno('Erro 450: A caixa postal do usuário está indisponível.');
      451:  Result := retorno('Erro 451: Ação solicitada abortada por erro local no processamento.');
      452:  Result := retorno('Erro 452: Muitos e-mails enviados ou muitos destinatários.');
      471:  Result := retorno('Erro 471: Erro no servidor de correio devido a um filtro anti-spam local.');
      500:  Result := retorno('Erro 500: Erro de sintaxe, o servidor não reconheceu o comando.');
      501:  Result := retorno('Erro 501: Erro de sintaxe nos parâmetros ou argumentos.');
      502:  Result := retorno('Erro 502: Comando não implementado.');
      503:  Result := retorno('Erro 503: Sequência de comandos incorreta ou autenticação necessária.');
      504:  Result := retorno('Erro 504: Parâmetro de comando não implementado.');
      510, 511: Result := retorno('Erro 510/511: Endereço de e-mail inválido.');
      512:  Result := retorno('Erro 512: Erro de DNS, o servidor do domínio do destinatário não pode ser encontrado.');
      513:  Result := retorno('Erro 513: Tipo de endereço incorreto.');
      523:  Result := retorno('Erro 523: O tamanho total da mensagem excede o limite do servidor do destinatário.');
      530:  Result := retorno('Erro 530: Problema de autenticação ou lista negra.');
      541:  Result := retorno('Erro 541: Mensagem rejeitada pelo destinatário devido a um filtro anti-spam.');
      550:  Result := retorno('Erro 550: O endereço de e-mail do destinatário não existe.');
      551:  Result := retorno('Erro 551: Usuário não local ou endereço inválido, relé negado.');
      552:  Result := retorno('Erro 552: Caixa de correio do destinatário excedeu o limite de armazenamento.');
      553:  Result := retorno('Erro 553: Nome de caixa postal inválido.');
      554:  Result := retorno('Erro 554: A transação falhou. O servidor não tentará novamente.');
    else
      Result := '';
    end;
  end;
  {$ENDREGION}

   {$REGION 'GMAIL'}
  if (Pos('gmail', provedor) > 0) then
  begin
    case codigo of
      421:  Result := retorno('Erro 421: O serviço não está disponível. Tente novamente mais tarde.');
      450:  Result := retorno('Erro 450: O usuário está recebendo muitas mensagens em um curto intervalo.');
      451:  Result := retorno('Erro 451: Erro temporário do servidor.');
      452:  Result := retorno('Erro 452: Limite de armazenamento excedido.');
      454:  Result := retorno('Erro 454: Muitas tentativas de login.');
      500:  Result := retorno('Erro 500: Erro de sintaxe do servidor.');
      501:  Result := retorno('Erro 501: Comando ou argumento inválido.');
      503:  Result := retorno('Erro 503: Sequência de comandos inválida.');
      504:  Result := retorno('Erro 504: Tipo de autenticação não reconhecido.');
      534:  Result := retorno('Erro 534: Senha específica do aplicativo necessária.');
      535:  Result := retorno('Erro 535: Nome de usuário e senha não aceitos.');
      550:  Result := retorno('Erro 550: Caixa de correio inexistente ou domínio não autorizado.');
      552:  Result := retorno('Erro 552: Mensagem bloqueada devido a problema de segurança.');
      554:  Result := retorno('Erro 554: Mensagem rejeitada ou erro no conteúdo.');
    else
      Result := '';
    end;
  end;
    {$ENDREGION}
end;
