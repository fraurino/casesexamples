/*COMO USAR*/
/*select * from SP_CURVA_ABC(:DATAINICIAL, :DATAFINAL)*/

SET TERM ^ ;

create or alter procedure SP_CURVA_ABC (
    DATAINICIAL date,
    DATAFINAL date)
returns (
    CODIGO varchar(20),
    DESCRICAO varchar(100),
    VALOR numeric(15,2),
    FATURAMENTO numeric(15,2),
    PART numeric(15,2),
    ACUM numeric(15,2),
    CLASSE char(1),
    QTDE numeric(15,2))
as
begin
 Select sum(d.valorliquido) from nfceitens d
    inner join nfce v on v.numeronfcce=d.codnfce
    where (Cast(v.dataemissao as date) between :datainicial And :datafinal)

    Into :Faturamento  ;
    ACUM = 0 ;
    For
        Select
            lpad(d.codproduto,14,'0') ,p.produto,
            cast( sum(d.valorliquido) as numeric(15,2) ) Valoritem,
            cast(sum(d.valorliquido) / :Faturamento * 100 as numeric(15,2)) ,
            cast(sum(d.qtde) as numeric(15,2)) Qtde
            from nfceitens d
            inner join nfce v on v.numeronfcce   = d.codnfce
            inner join produtos p on p.controle  = d.codproduto
            where (Cast(v.dataemissao as date) between :datainicial And :datafinal) and v.autorizada = 'SIM' and v.cancelada = 'NÃO'
            group by d.codproduto, p.controle, p.produto
            order by 2 desc -- 2 desc = faturamento
              Into :codigo,:descricao, :valor ,:part, :qtde
        Do
        Begin
            ACUM = ACUM + part ;
            if (ACUM <= 20 ) then
               Classe = 'A' ;
            else if (ACUM <= 50 ) then
               Classe = 'B' ;
            Else
               Classe = 'C' ;
            Suspend ;
        End
end^

SET TERM ; ^

/* Following GRANT statetements are generated automatically */

GRANT SELECT ON NFCEITENS TO PROCEDURE SP_CURVA_ABC;
GRANT SELECT ON NFCE TO PROCEDURE SP_CURVA_ABC;
GRANT SELECT ON PRODUTOS TO PROCEDURE SP_CURVA_ABC;

/* Existing privileges on this procedure */

GRANT EXECUTE ON PROCEDURE SP_CURVA_ABC TO SYSDBA;
