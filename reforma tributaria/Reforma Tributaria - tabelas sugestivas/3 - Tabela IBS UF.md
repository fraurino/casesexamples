```sql
CREATE TABLE IBS_UF (
    ID      INTEGER NOT NULL PRIMARY KEY,
    UF      INTEGER,
    IBS_UF  NUMERIC(15,2)
);
```

Essa estrutura cria a tabela `IBS_UF` para armazenar as alíquotas do IBS por unidade federativa (UF).