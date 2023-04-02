unit ExpressionParser;

interface

uses
    System.SysUtils, Math, Classes  ;
    
    //source example in https://www.portugal-a-programar.pt/forums/topic/48331-expression-parser-em-delphi//
    
    
    {##EXAMPLE##
      var
        f: string;
        r: double;
        v : double;
      begin
        v := 12;
        f := '(v*2)+ 1-2';
        f := StringReplace(f, 'P0', FloatToStr(v), [rfReplaceAll]);
        f := ExpressionParser.ParseF( f ,3,2) ;
        showmessage(f);
      end;
    
    }
    
    (*
    O que faz:

    - Aceita operadores aritméticos +, -, *, x, X, /, :, ^

    - Aceita operadores lógicos <, >, =, !

    - Aceita constantes matemáticas PI, E

    - Aceita funções SEN, COS, TAN, RAIZ, QUADRADO, CUBO, ROUND, TRUNC, ABS, FRAC, PAR, LN, LOG2, LOG10, 
                     ARCSEN, ARCCOS, ARCTAN, COTAN, DEGRAD, RADDEG, MEDIA, HIPOTENUSA, DIV, MOD

    - Aceita funções encadeadas (ex.: CUBO (RAIZ 5))

    - Aceita parêntesis encadeados

    - Trabalha com valores negativos

    - Faz cálculos de sinais (++ = +; +- = -; -+ = -; -- = +) inseridos numa expressão

    - Respeita as prioridades das operações na ordem: Parêntesis --> Funções --> ^ --> *, x, X, /, : --> +, -

    - Guarda o históricos dos cálculos, passo a passo.

    - Permite construir uma string com a ajuda total ou parcial sobre a unit.

    O que não faz:

    - Muita coisa... Falta saber o quê!  😞

    Há funções ainda a adicionar, aceitam-se sugestões. Por agora pus só as mais básicas

    Não fiz grandes testes, pelo que é possível haver algum bug: Se alguém detetar, agradeço o aviso

    Particularidades:

    - Para usar funções encadeadas, a segunda função deve estar dentro de parêntesis

    - Faz algum controlo de erros, nomeadamente, abre ou fecha (no inicio ou no fim da expressão respetivamente) os parêntesis que não tenham sido corretamente inseridos
    *)
    

const
     // Line Break
     BR=#13+#10;
     // Número de constantes previstas na expressão
     MaxConst=2;
     // Nomes das constantes previstas na expressão
     ConstNames:Array [1..MaxConst] of ShortString=('PI','E');
     // Número de funções especiais previstas na expressão
     MaxSFuncs=4;
     // Nomes das funções especiais previstas na expressão
     SFuncNames:Array [1..MaxSFuncs] of ShortString=('MEDIA','HIPOTENUSA','DIV','MOD');
     // Número de funções previstas na expressão
     MaxFuncs=20;
     // Nomes das funções previstas na expressão
     FuncNames:Array [1..MaxFuncs] of ShortString=('SEN','COS','TAN','RAIZ','QUADRADO','CUBO','ROUND','TRUNC','ABS',
                                                   'FRAC','PAR','LN','LOG2','LOG10','ARCSEN','ARCCOS','ARCTAN','COTAN',
                                                   'DEGRAD','RADDEG');
     DecimalSeparator     = ',';

function Help(Level:Integer):String;
procedure HistoryClear;
function HistoryShow:String;
procedure HistoryClose;
procedure HistoryAdd(Expression:ShortString);
function Parse(Expression:ShortString):real;
function ParseF(Expression:ShortString;Precision,Digits:Integer):ShortString;
procedure ParseParentesis(var Expression:ShortString);
procedure ParseConstants(var Expression:ShortString);
procedure ParseSpecialFunctions(var Expression:ShortString);
procedure ParseFunctions(var Expression:ShortString);
procedure ParseOperators(var Expression:ShortString);
function FindLastValueStart(Expression:ShortString;StartPoint:integer):integer;
function FindNextValueEnd(Expression:ShortString;StartPoint:integer):integer;
function GetLastExpressionValue(Expression:ShortString;StartPoint:integer):real;
function GetNextExpressionValue(Expression:ShortString;StartPoint:integer):real;
procedure PrepareParentesis(var Expression:ShortString);
procedure PrepareOpExpression(var Expression:ShortString);
function CharCount(ch:AnsiChar;Text:ShortString):integer;
function wPos(SubStr,Str:ShortString):integer;

var
   History:TStringList;

implementation

function Help(Level:Integer):String;
begin
     Result:=' E X P R E S S I O N   P A R S E R'+BR+
             '-----------------------------------'+BR+BR;

     if Level in [0,1] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Parse'+BR+
                                           'Devolve em formato numérico (Real) o valor da expressão passada por parâmetro!'+BR+
                                           'Uso: <real> := Parse(Expressão:ShortString);        Exemplo: r := Parse(''1+2+3'');'+BR+BR;

     if Level in [0,2] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): ParseF'+BR+
                                           'Devolve em formato alfanumérico (ShortString) o valor da expressão passada por parâmetro!'+BR+
                                           'Uso: <string> := Parse(Expressão:ShortString;Precisão,Decimais:Integer);        Exemplo: s := ParseF(''1+2+3'',3,2);'+BR+
                                           'Precisão: Número mínimo de algarismos reservados para o resultado'+BR+
                                           'Decimais: Número máximo de casas decimais para o resultado'+BR+BR;

     if Level in [0,3] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): History'+BR+
                                           'Grupo de funções para gerir o histórico de operações efectuadas:'+BR+
                                           '- Inicializar o Histórico: Uso: HistoryClear;'+BR+
                                           '- Gerar o Histórico (String): Uso: s:=HistoryShow;'+BR+
                                           '- Terminar o Histórico: Uso: HistoryClose; (Nota: É importante terminar o historico no fim do programa se este tiver sido inicializado!)'+BR+BR;

     if Level in [0,4] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Parêntesis'+BR+
                                           'É suportado o uso de parêntesis curvos, simples () ou encadeados (())'+BR+
                                           'Prioridades: São sempre prioritárias as operações contidas pelo grupo de parêntesis mais interno, até às operações fora de parêntesis.'+BR+BR;

     if Level in [0,5] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Constantes'+BR+
                                           'As constantes são representações alfabéticas de um valor numérico fixo. Podem ser inseridas normalmente nas expressões.'+BR+
                                           'Uso: CONSTANTE   ***   Coloca-se o nome da constante em qualquer lugar da expressão'+BR+
                                           '- PI: 3.14159265'+BR+
                                           '- E: 2.71828182'+BR+BR;

     if Level in [0,6] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Funções Especiais (múltiplos argumentos)'+BR+
                                           'As funções especiais recebem múltiplos argumentos. Podem ser inseridas normalmente nas expressões, mas não é possível encadear funções especiais, embora possam ser usadas funções normais dentro das especiais.'+BR+
                                           'Uso: FUNÇÃO [arg1 arg2 arg3 ... arg_n]   ***   Coloca-se o nome da função, seguido dos argumentos, separados por espaços e contidos entre parêntesis rectos [].'+BR+
                                           '- MEDIA: Calcula a média de todos os argumentos fornecidos.'+BR+
                                           '- HIPOTENUSA: Calcula a hipotenusa de um triângulo rectângulo, cujos catetos são os argumentos 1 e 2.'+BR+
                                           '- DIV: Calcula a divisão inteira dos dois primeiros argumentos.'+BR+
                                           '- MOD: Calcula o resto da divisão inteira dos dois primeiros argumentos.'+BR+BR;

     if Level in [0,7] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Funções (argumento único)'+BR+
                                           'As funções recebem um único argumento. Podem ser inseridas normalmente nas expressões, e podem ser encadeadas desde que usem parêntesis em redor das expressões internas Ex.: ABS (SEN 5) .'+BR+
                                           'Uso: FUNÇÃO Arg  ou  FUNÇÃO (Expressão)   ***   Coloca-se o nome da função, seguido do argumento. O Argumento pode ser uma expressão também, se contido entre parêntesis curvos.'+BR+
                                           '- SEN: Calcula o seno de um ângulo (em radianos).'+BR+
                                           '- COS: Calcula o coseno de um ângulo (em radianos).'+BR+
                                           '- TAN: Calcula a tangente de um ângulo (em radianos).'+BR+
                                           '- ARCSEN: Calcula o arcoseno de um ângulo (em radianos).'+BR+
                                           '- ARCCOS: Calcula o arcocoseno de um ângulo (em radianos).'+BR+
                                           '- ARCTAN: Calcula a arcotangente de um ângulo (em radianos).'+BR+
                                           '- COTAN: Calcula a cotangente de um ângulo (em radianos).'+BR+
                                           '- DEGRAD: Converte graus em radianos.'+BR+
                                           '- RADDEG: Converte radianos em graus.'+BR+
                                           '- RAIZ: Calcula a raíz quadrada.'+BR+
                                           '- QUADRADO: Calcula o quadrado de um valor.'+BR+
                                           '- CUBO: Calcula o cubo de um valor.'+BR+
                                           '- ROUND: Arredonda um valor ao seu inteiro mais próximo.'+BR+
                                           '- TRUNC: Trunca um valor ao número inteiro imediatamente inferior.'+BR+
                                           '- ABS: Devolve a parte absoluta de um valor (sem sinal negativo ou positivo).'+BR+
                                           '- FRAC: Devolve a parte fraccional de um valor.'+BR+
                                           '- PAR: Devolve 1 se um valor for par, ou 0 se o valor for impar.'+BR+
                                           '- LN: Calcula o logaritmo natural de um valor.'+BR+
                                           '- LOG2: Calcula o logaritmo de base 2 de um valor.'+BR+
                                           '- LOG10: Calcula o logaritmo de base 10 de um valor.'+BR+BR;

     if Level in [0,8] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Operadores Lógicos'+BR+
                                           'Os operadores lógicos devolvem 1 ou 0 caso a operação retorne True ou False. .'+BR+
                                           'Uso: arg1 OPERADOR arg2   ***   Coloca-se o operador entre dois argumentos.'+BR+
                                           'Prioridade ente operadores: Máxima'+BR+
                                           '- =: True se os argumentos tiverem o mesmo valor.'+BR+
                                           '- !: True se os argumentos tiverem valor diferente.'+BR+
                                           '- <: True se o primeiro argumento for menor que o segundo.'+BR+
                                           '- >: True se o primeiro argumento for maior que o segundo.'+BR+BR;

     if Level in [0,9] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Operadores Aritméticos'+BR+
                                           'Os operadores aritméticos devolvem o resultado da operação que representam'+BR+
                                           'Uso: arg1 OPERADOR arg2   ***   Coloca-se o operador entre dois argumentos.'+BR+
                                           '- ^: Exponenciação (arg1 elevado a arg2).'+BR+
                                           '- / (ou : Divisão.'+BR+
                                           '- * (ou x ou X): Multiplicação.'+BR+
                                           '- +: Soma.'+BR+
                                           '- -: Subtração.'+BR+BR;

     if Level in [0,10] then Result:=Result+'A J U D A ('+IntToStr(Level)+'): Cálculos de Sinais'+BR+
                                            'Os sinais duplos são calculados de acordo com as leis matemáticas:'+BR+
                                            '- ++ = +'+BR+
                                            '- +- = -'+BR+
                                            '- -+ = -'+BR+
                                            '- -- = +'+BR+BR;
end;

procedure HistoryClear;
begin
     History:=TStringList.Create;
end;

function HistoryShow:String;
var
   i:integer;
begin
     Result:='';
     for i:=0 to History.Count-1 do
         begin
              if i>0 then result:=Result+BR;
              Result:=Result+IntToStr(i+1)+'. '+History[i];
         end;
end;

procedure HistoryClose;
begin
     History.Free;
end;

procedure HistoryAdd(Expression:ShortString);
begin
     if History<>nil then History.Add(Expression);
end;

function Parse(Expression:ShortString):real;
begin
     // Se a expressão for vazia, termina o calculo
     if Expression='' then begin
                                Result:=0;
                                Exit;
                           end;

     // Adiciona a espressão inicial ao histórico
     HistoryAdd(Expression);

     // Prepara todos os grupos de parentesis para serem processados
     PrepareParentesis(Expression);

     // Remove os parentesis existentes na expressão, executando os cálculos indicados pelos operadores dentro dos parentesis
     ParseParentesis(Expression);

     // Converte em valor as constantes existentes na expressão
     ParseConstants(Expression);

     // Executa as funções especiais existentes na expressão
     ParseSpecialFunctions(Expression);

     // Executa as funções existentes na expressão
     ParseFunctions(Expression);

     // Executa os cálculos indicados pelos operadores da expressão
     ParseOperators(Expression);

     try
        Result:=StrToFloat(Expression);
     except
        Result:=0;
     end;
end;

function ParseF(Expression:ShortString;Precision,Digits:Integer):ShortString;
begin
     Result:=FloatToStrF(Parse(Expression),ffGeneral,Precision,Digits);
end;

procedure ParseParentesis(var Expression:ShortString);
var
   Expr:ShortString;
   i,ParStart,ParEnd:integer;
begin
     // Se não houver nenhuma abertura de parentesis, não há nada a fazer e sai
     if pos('(',Expression)=0 then exit;

     // Encontra o grupo de parentesis mais interno (sem parentesis encadeados)
     // Copia a expressão contida no grupo de parentesis encontrado
     // Guarda as posições da expressão antes e depois do grupo de parentesis
     for i:=1 to length(Expression) do
         case Expression[i] of
              '(':begin
                       ParStart:=i-1;
                       Expr:='';
                  end;
              ')':begin
                       ParEnd:=i+1;
                       break;
                  end
         else Expr:=Expr+Expression[i];
         end;

     // Calcula as funções contidas no grupo de parentesis
     ParseFunctions(Expr);

     // Calcula a expressão contida no grupo de parentesis
     ParseOperators(Expr);

     // Junta o valor encontrado ao restante da expressão
     Expression:=Copy(Expression,1,ParStart)+Expr+Copy(Expression,ParEnd,999);

     // Adiciona a espressão ao histórico
     HistoryAdd(Expression);

     // Procura o próximo grupo de parentesis
     ParseParentesis(Expression);
end;

procedure ParseConstants(var Expression:ShortString);
var
   Preffix,Suffix,Value:ShortString;
   ConstStart,NumConst,TempPos:Integer;
begin
     // Verifica se a expressão contém alguma constante prevista, e qual é
     NumConst:=0;
     repeat
           inc(NumConst);

           // Testa o nome da constante
           ConstStart:=wPos(ConstNames[NumConst],UpperCase(Expression));
     until (NumConst=MaxConst) or (ConstStart>0);
     if ConstStart=0 then exit;

     // Memoriza a parte da expressão desde o inicio desta ao ultimo elemento antes da função a processar
     Preffix:=Copy(Expression,1,ConstStart-1);
     // Memoriza a parte da expressão desde o fim do argumento a ser processado até ao fim da expressão
     Suffix:=Copy(Expression,ConstStart+Length(ConstNames[NumConst])+1,999);

     // Calcula o valor da função
     case NumConst of
          1:Value:=FloatToStr(PI);
          2:Value:=FloatToStr(Exp(1.0));
     end;

     // Retorna a expressão depois de processada
     Expression:=Preffix+Value+Suffix;

     // Adiciona a espressão ao histórico
     HistoryAdd(Expression);

     // Processa recursivamente a próxima função
     ParseConstants(Expression);
end;

procedure ParseSpecialFunctions(var Expression:ShortString);
var
   Preffix,Suffix,Param,Value:ShortString;
   SFuncStart,NumSFunc,i,Cont:Integer;
   Temp:Real;
begin
     // Verifica se a expressão contém alguma função especial prevista, e qual é
     NumSFunc:=0;
     repeat
           inc(NumSFunc);

           // Testa o nome da função especial
           SFuncStart:=wPos(SFuncNames[NumSFunc],UpperCase(Expression));
     until (NumSFunc=MaxSFuncs) or (SFuncStart>0);
     if SFuncStart=0 then exit;

     // Memoriza a parte da expressão desde o inicio desta ao ultimo elemento antes da função especial a processar
     Preffix:=Copy(Expression,1,SFuncStart-1);
     // Prepara os parâmetros da função especial (contidos entre [ ])
     Param:=Copy(Expression,Pos('[',Expression)+1,999);
     Param:=Copy(Param,1,Pos(']',Param)-1);
     Param:=StringReplace(Param,'  ',' ',[rfReplaceAll]);
     // Memoriza a parte da expressão desde o fim do argumento a ser processado até ao fim da expressão
     Suffix:=Copy(Expression,Pos(']',Expression)+1,999);

     // Calcula o valor da função especial
     try
        case NumSFunc of
             1:begin // Media
                    Param:='('+StringReplace(Param,' ','+',[rfReplaceAll])+') / ';
                    Param:=Param+IntToStr(CharCount('+',Param)+1);
                    Value:=FloatToStr(Parse(Param));
               end;
             2:begin // Hipotenusa
                    Value:=Param+' ';
                    Param:='RAIZ ( QUADRADO '+Copy(Value,1,pos(' ',Value)-1)+' + ';
                    Delete(Value,1,pos(' ',Value));
                    Param:=Param+'QUADRADO '+Copy(Value,1,pos(' ',Value)-1)+') ';
                    Value:=FloatToStr(Parse(Param));
               end;
             3:begin // DIV
                    Value:=Param+' ';
                    Param:=Copy(Value,1,pos(' ',Value)-1);
                    Delete(Value,1,pos(' ',Value));
                    Delete(Value,pos(' ',Value),999);
                    Value:=IntToStr(Round(StrToFloat(Param)) DIV Round(StrToFloat(Value)));
               end;
             4:begin // MOD
                    Value:=Param+' ';
                    Param:=Copy(Value,1,pos(' ',Value)-1);
                    Delete(Value,1,pos(' ',Value));
                    Delete(Value,pos(' ',Value),999);
                    Value:=IntToStr(Round(StrToFloat(Param)) MOD Round(StrToFloat(Value)));
               end;
        end;
     except
        Value:='0';
     end;

     // Retorna a expressão depois de processada
     Expression:=Preffix+Value+Suffix;

     // Adiciona a espressão ao histórico
     HistoryAdd(Expression);

     // Processa recursivamente a próxima função especial
     ParseSpecialFunctions(Expression);
end;

procedure ParseFunctions(var Expression:ShortString);
var
   Preffix,Suffix,Value:ShortString;
   FuncStart,NumFunc:Integer;
begin
     // Verifica se a expressão contém alguma função prevista, e qual é
     NumFunc:=0;
     repeat
           inc(NumFunc);

           // Testa o nome da função
           FuncStart:=wPos(FuncNames[NumFunc],UpperCase(Expression));
     until (NumFunc=MaxFuncs) or (FuncStart>0);
     if FuncStart=0 then exit;

     // Memoriza a parte da expressão desde o inicio desta ao ultimo elemento antes da função a processar
     Preffix:=Copy(Expression,1,FuncStart-1);
     // Memoriza a parte da expressão desde o fim do argumento a ser processado até ao fim da expressão
     Suffix:=Copy(Expression,FindNextValueEnd(Expression,FuncStart)+1,999);

     // Calcula o valor da função
     try
        case NumFunc of
             1:Value:=FloatToStr(Sin(GetNextExpressionValue(Expression,FuncStart)));
             2:Value:=FloatToStr(Cos(GetNextExpressionValue(Expression,FuncStart)));
             3:Value:=FloatToStr(Tan(GetNextExpressionValue(Expression,FuncStart)));
             4:Value:=FloatToStr(Sqrt(GetNextExpressionValue(Expression,FuncStart)));
             5:Value:=FloatToStr(Sqr(GetNextExpressionValue(Expression,FuncStart)));
             6:Value:=FloatToStr(Power(GetNextExpressionValue(Expression,FuncStart),3));
             7:Value:=IntToStr(Round(GetNextExpressionValue(Expression,FuncStart)));
             8:Value:=IntToStr(Trunc(GetNextExpressionValue(Expression,FuncStart)));
             9:Value:=FloatToStr(Abs(GetNextExpressionValue(Expression,FuncStart)));
            10:Value:=FloatToStr(Frac(GetNextExpressionValue(Expression,FuncStart)));
            11:if Odd(Trunc(GetNextExpressionValue(Expression,FuncStart))) then Value:='1' else Value:='0';
            12:Value:=FloatToStr(Ln(GetNextExpressionValue(Expression,FuncStart)));
            13:Value:=FloatToStr(Log2(GetNextExpressionValue(Expression,FuncStart)));
            14:Value:=FloatToStr(Log10(GetNextExpressionValue(Expression,FuncStart)));
            15:Value:=FloatToStr(ArcSin(GetNextExpressionValue(Expression,FuncStart)));
            16:Value:=FloatToStr(ArcCos(GetNextExpressionValue(Expression,FuncStart)));
            17:Value:=FloatToStr(ArcTan(GetNextExpressionValue(Expression,FuncStart)));
            18:Value:=FloatToStr(CoTan(GetNextExpressionValue(Expression,FuncStart)));
            19:Value:=FloatToStr(DegToRad(GetNextExpressionValue(Expression,FuncStart)));
            20:Value:=FloatToStr(RadToDeg(GetNextExpressionValue(Expression,FuncStart)));
        end;
     except
        Value:='0';
     end;

     // Retorna a expressão depois de processada
     Expression:=Preffix+Value+Suffix;

     // Adiciona a espressão ao histórico
     HistoryAdd(Expression);

     // Processa recursivamente a próxima função
     ParseFunctions(Expression);
end;

procedure ParseOperators(var Expression:ShortString);
var
   Preffix,Suffix,Value:ShortString;
   OpPos:Integer;
   Op:AnsiChar;
begin
     // Remove os espaços da expressão, faz os cálculos dos sinais aritméticos, e mantém o sinal - nos números negativos
     PrepareOpExpression(Expression);

     // Procura operadores de prioridade 0 (Lógicos)
     OpPos:=0;
     repeat
           inc(OpPos);
     until (Expression[OpPos] in ['>','<','=','!']) or (OpPos>length(Expression));
     if OpPos>length(expression)
        then begin
                  // Procura operadores de prioridade 1 (máxima)
                  OpPos:=0;
                  repeat
                        inc(OpPos);
                  until (Expression[OpPos] in ['^']) or (OpPos>length(Expression));
                  if OpPos>length(expression)
                     then begin
                               // Procura operadores de prioridade 2 (média)
                               OpPos:=0;
                               repeat
                                     inc(OpPos);
                               until (Expression[OpPos] in ['*','/','x','X',':']) or (OpPos>length(Expression));
                               if OpPos>length(expression)
                                  then begin
                                            // Procura operadores de prioridade 3 (mínima)
                                            OpPos:=0;
                                            repeat
                                                  inc(OpPos);
                                            until (Expression[OpPos] in ['+']) or (OpPos>length(Expression));
                                            if OpPos>length(expression)
                                               then exit                       // Se não encontrar nenhum operador
                                               else Op:=Expression[OpPos];     // Guarda qual o operador de prioridade 3 encontrado
                                       end
                                  else Op:=Expression[OpPos];                  // Guarda qual o operador de prioridade 2 encontrado
                          end
                     else Op:=Expression[OpPos];                               // Guarda qual o operador de prioridade 1 encontrado
             end
        else Op:=Expression[OpPos];                                            // Guarda qual o operador de prioridade 0 encontrado

     // Memoriza a parte da expressão desde o inicio desta até antes do primeiro elemento do operador
     Preffix:=Copy(Expression,1,FindLastValueStart(Expression,OpPos)-1);

     // Memoriza a parte da expressão desde o fim do argumento a ser processado até ao fim da expressão
     Suffix:=Copy(Expression,FindNextValueEnd(Expression,OpPos)+1,999);

     // Calcula o valor da operação seleccionada
     try
        case Op of
             '=':if GetLastExpressionValue(Expression,OpPos)=GetNextExpressionValue(Expression,OpPos) then Value:='1' else Value:='0';
             '!':if GetLastExpressionValue(Expression,OpPos)<>GetNextExpressionValue(Expression,OpPos) then Value:='1' else Value:='0';
             '<':if GetLastExpressionValue(Expression,OpPos)<GetNextExpressionValue(Expression,OpPos) then Value:='1' else Value:='0';
             '>':if GetLastExpressionValue(Expression,OpPos)>GetNextExpressionValue(Expression,OpPos) then Value:='1' else Value:='0';
             '^':Value:=FloatToStr(Power(GetLastExpressionValue(Expression,OpPos),GetNextExpressionValue(Expression,OpPos)));
             '*','x','X':Value:=FloatToStr(GetLastExpressionValue(Expression,OpPos)*GetNextExpressionValue(Expression,OpPos));
             '/',':':Value:=FloatToStr(GetLastExpressionValue(Expression,OpPos)/GetNextExpressionValue(Expression,OpPos));
             '+':Value:=FloatToStr(GetLastExpressionValue(Expression,OpPos) + GetNextExpressionValue(Expression,OpPos));
             '-':Value:=FloatToStr(GetLastExpressionValue(Expression,OpPos) - GetNextExpressionValue(Expression,OpPos));
        end;
     except
        Value:='0';
     end;

     // Junta o valor encontrado à restante expressão
     Expression:=Preffix+Value+Suffix;

     // Adiciona a espressão ao histórico
     HistoryAdd(Expression);

     // Processa recursivamente a próxima operação
     ParseOperators(Expression);
end;

function FindLastValueStart(Expression:ShortString;StartPoint:integer):integer;
begin
     // Inicializa o resultado (se nada for feito, o resultado será igual ao ponto de partida
     Result:=StartPoint;

     // Encontra a posição do fim do valor
     while (Result>=1) and (Expression[Result-1] in ['0'..'9','-', DecimalSeparator]) do
           dec(Result);
end;

function FindNextValueEnd(Expression:ShortString;StartPoint:integer):integer;
begin
     // Inicializa o resultado (se nada for feito, o resultado será igual ao ponto de partida
     Result:=StartPoint;

     // Passa à frente, caso haja, caracteres que não pertençam ao valor
     while not (Expression[Result] in ['0'..'9','-',DecimalSeparator]) do
           inc(Result);

     // Encontra a posição do inicio do valor
     while Expression[Result+1] in ['0'..'9','-',DecimalSeparator] do
           inc(Result);
end;

function GetLastExpressionValue(Expression:ShortString;StartPoint:integer):real;
var
   Value:ShortString;
   i:integer;
begin
     // Descarta a segunda parte da expressão, desde o operador actual até ao fim
     Expression:=Copy(Expression,1,StartPoint-1);

     // Se o segmento resultante estiver vazio, passa a ser 0
     if Expression='' then Expression:='0';

     // Descarta todos os caracteres da expressão entre o valor e o ponto de partida
     while not (Expression[Length(Expression)] in ['0'..'9','-',DecimalSeparator]) do
           Delete(Expression,Length(Expression),1);

     // Guarda os caracteres correspondentes ao valor
     Value:='';
     i:=Length(Expression);
     while (i>=1) and (Expression[i] in ['0'..'9','-',DecimalSeparator]) do
           begin
                Value:=Expression[i]+Value;
                dec(i);
           end;

     // Converte o valor encontrado num número real
     Result:=StrToFloat(Value);
end;

function GetNextExpressionValue(Expression:ShortString;StartPoint:integer):real;
var
   Value:ShortString;
   i:integer;
begin
     // Descarta a primeira parte da expressão, desde o inicio até ao operador actual
     Expression:=Copy(Expression,StartPoint+1,999);

     // Se o segmento resultante estiver vazio, passa a ser 0
     if Expression='' then Expression:='0';

     // Descarta todos os caracteres da expressão até se inicio o valor
     while not (Expression[1] in ['0'..'9','-',DecimalSeparator]) do
           Delete(Expression,1,1);

     // Guarda os caracteres correspondentes ao valor
     Value:='';
     i:=1;
     while (i<=Length(Expression)) and (Expression[i] in ['0'..'9','-',DecimalSeparator]) do
           begin
                Value:=Value+Expression[i];
                inc(i);
           end;

     // Converte o valor encontrado num número real
     Result:=StrToFloat(Value);
end;

procedure PrepareParentesis(var Expression:ShortString);
var
   i,j,CountOpen,CountClosed:integer;
   pExpr:ShortString;
begin
     // Faz com que todos os parentesis abertos sejam fechados e vice-versa
     CountOpen:=CharCount('(',Expression);
     CountClosed:=CharCount(')',Expression);
     for i:=CountClosed to CountOpen-1 do
         Expression:=Expression+')';
     for i:=CountOpen to CountClosed-1 do
         Expression:='('+Expression;

     // Atribui o sinal * caso não haja sinal antes dos parentesis
     pExpr:='';
     for i:=1 to Length(Expression) do
         case Expression[i] of
              '(':if i>1 then begin
                                   j:=i;
                                   repeat
                                         dec(j);
                                   until Expression[j]<>' ';
                                   if Expression[j] in ['0'..'9',DecimalSeparator]
                                      then pExpr:=pExpr+'*('
                                      else pExpr:=pExpr+'(';
                              end
                         else pExpr:=pExpr+Expression[i];
              ')':if i<length(Expression) then begin
                                                    j:=i;
                                                    repeat
                                                          inc(j);
                                                    until Expression[j]<>' ';
                                                    if Expression[j] in ['0'..'9',DecimalSeparator]
                                                       then pExpr:=pExpr+')*'
                                                       else pExpr:=pExpr+')';
                                               end
                                          else pExpr:=pExpr+Expression[i];
         else pExpr:=pExpr+Expression[i];
         end;
     Expression:=pExpr;
end;

procedure PrepareOpExpression(var Expression:ShortString);
var
   i:integer;
   pExpr:ShortString;
begin
     // Elimina todos os espaços da expressão
     while pos(' ',Expression)>0 do
           Delete(Expression,pos(' ',Expression),1);

     // Inicialização das variáveis do ciclo de preparação de sinais
     i:=1;
     pExpr:='';
     while i<=length(Expression) do
           begin
                case Expression[i] of
                     // Se o caracter encontrado for o sinal +
                     '+':case Expression[i+1] of
                              // Se o sinal encontrado e o próximo forem ++
                              '+':begin
                                       pExpr:=pExpr+'+';          // + com + dá +
                                       Inc(i,2);
                                  end;
                              // Se o sinal encontrado e o próximo forem +-
                              '-':begin
                                       pExpr:=pExpr+'+-';         // mantém ambos, pois o - pertence ao valor
                                       Inc(i,2);
                                  end;
                         else begin
                                   pExpr:=pExpr+'+';              // Se for um + simples, mantém-no
                                   Inc(i,1);
                              end;
                         end;
                     // Se o caracter encontrado for o sinal -
                     '-':case Expression[i+1] of
                              // Se o sinal encontrado e o próximo forem -+
                              '+':begin
                                       pExpr:=pExpr+'+-';         // Inverte os sinais, e o - passa a pertencer ao valor seguinte
                                       Inc(i,2);
                                  end;
                              '-':begin
                                       pExpr:=pExpr+'+';          // - com - dá +
                                       Inc(i,2);
                                  end;
                         else begin
                                   if i>1 then pExpr:=pExpr+'+-'  // Se não for o sinal de um 1º valor negativo, converte-se em +-, onde o - passa a pertencer ao valor e o + é a operação
                                          else pExpr:=pExpr+'-';
                                   Inc(i,1);
                              end;
                         end;
                else begin
                          pExpr:=pExpr+Expression[i];             // Adiciona qualquer outro caracter que não um sinal de + ou -
                          Inc(i,1);
                     end;
                end;
           end;

     // Devolve a expressão preparada
     Expression:=pExpr;
end;

function CharCount(ch:AnsiChar;Text:ShortString):integer;
var
   i:integer;
begin
     Result:=0;
     for i:=1 to length(Text) do
         if Text[i]=ch then Inc(Result);
end;

function wPos(SubStr,Str:ShortString):integer;
begin
     Result:=pos(SubStr,Str);
     if Result>1 then if Upcase(Str[Result-1]) in ['A'..'Z'] then Result:=0;
end;

end.
