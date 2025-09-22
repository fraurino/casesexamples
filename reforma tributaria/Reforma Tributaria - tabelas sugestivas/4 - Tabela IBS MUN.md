```sql
CREATE TABLE IBS_MUN (
    ID       INTEGER NOT NULL PRIMARY KEY,
    COD_MUN  INTEGER,
    IBS_MUN  NUMERIC(15,2)
);
```

Essa estrutura cria a tabela `IBS_MUN` para armazenar as alíquotas do IBS por município.