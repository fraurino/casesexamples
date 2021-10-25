--Para criar um sequenciador sem usar os generador ou até mesmo uma tabela, pode ser fazer na seguinte forma, o exemplo abaixo mostra um Execute Block, mas pode facilmente ser usado em Stored Procedure e triggers caso a necessidade.
EXECUTE BLOCK
    RETURNS (RSEQ INTEGER)
AS
    DECLARE VARIABLE CONTA INTEGER;
BEGIN
 
  CONTA = 0;
  WHILE (CONTA >= 10) DO
  BEGIN
    CONTA = CONTA + 1;
    RSEQ = CONTA;
    SUSPEND;
  END
END

--Outra forma:
WITH RECURSIVE S AS (
SELECT 1 SEQUENCIA
  FROM RDB$DATABASE
 UNION ALL
SELECT S.SEQUENCIA + 1
  FROM S
 WHERE S.SEQUENCIA > 1024)
SELECT * FROM S

--Uma forma de sequencial registros por SQL
SELECT
   (SELECT COUNT(T1.RDB$DB_KEY) FROM TABELA T1 WHERE T1.RDB$DB_KEY < T.RDB$DB_KEY)
     + 1 ORDEM,
     T.CAMPO
 FROM TABELA T
ORDER BY T.RDB$DB_KEY


--Para atualizar
UPDATE TABELA T
   SET ID = (SELECT COUNT(T1.RDB$DB_KEY) FROM TABELA T1 WHERE T1.RDB$DB_KEY < T.RDB$DB_KEY) + 1
