```sql
CREATE TABLE OPERACAO (
    ID                  INTEGER NOT NULL PRIMARY KEY,
    TIPO                VARCHAR(7), -- Entrada ou Saída
    DESCRICAO           VARCHAR(50), -- Nome da operação. Ex: VENDA DE MERCADORIA, DEVOLUÇÃO DE VENDA
    FINALIDADE          VARCHAR(50), -- Ex: 1 - NF-e normal, 4 - Devolução de mercadoria
    CST_IBS_CBS         VARCHAR(50), -- Código de Situação Tributária do IBS/CBS para a operação
    CCLASSTRIB_IBS_CBS  VARCHAR(50)  -- Código de Classificação Tributária do IBS/CBS para a operação
);
```

- Os campos `CST_IBS_CBS` e `CCLASSTRIB_IBS_CBS` permitem definir, para cada operação, o enquadramento tributário correto conforme a reforma tributária.