# Documentação das Alterações no Banco de Dados – Reforma Tributária

Este documento descreve de forma clara e objetiva as alterações realizadas no banco de dados para atender à Reforma Tributária, incluindo a criação de novas tabelas, adição de campos e explicação de conceitos relevantes.

## Novas Tabelas

### 1. `DFE_CST_IBS_CBS`
Tabela destinada ao armazenamento dos novos CST (Código de Situação Tributária) referentes ao IBS (Imposto sobre Bens e Serviços) e CBS (Contribuição sobre Bens e Serviços).

**Conceito:**
- **CST (Código de Situação Tributária):** Identifica a situação tributária de um produto ou serviço em relação ao IBS/CBS, determinando o tratamento fiscal a ser aplicado.

### 2. `DFE_CCLASTRIB_IBS_CBS`
Tabela para armazenar os códigos de classificação tributária (cClassTrib) do IBS/CBS.

**Conceito:**
- **cClassTrib:** Código que classifica a natureza tributária de um produto ou serviço, utilizado para definir regras específicas de tributação.

### 3. `IBS_UF`
Tabela que armazena as alíquotas do IBS por Unidade Federativa (UF).

**Conceito:**
- **Alíquota por UF:** Permite definir diferentes percentuais de IBS conforme o estado de destino da operação.

### 4. `IBS_MUN`
Tabela que armazena as alíquotas do IBS por município.

**Conceito:**
- **Alíquota por Município:** Permite detalhar a tributação do IBS em nível municipal, conforme legislação vigente.

### 5. `ANEXO_REDUCAO`
Tabela que relaciona o número do anexo, o NCM ou NBS e o percentual de redução de alíquota conforme previsto na LC 204.

**Conceito:**
- **NCM/NBS:** Códigos que identificam mercadorias (NCM) ou serviços (NBS).
- **Redução de Alíquota:** Percentual de desconto aplicado à alíquota padrão, conforme legislação específica para determinados produtos/serviços.

### 6. `DFE_NCM_NBS_IBS_CBS`
Tabela que relaciona NCM/NBS com cClassTrib e CST do IBS/CBS. Utilizada para sugerir valores no cadastro de produtos.

**Exemplo de uso:**
- Produto com NCM 00000 poderá utilizar determinados cClassTrib e CST, facilitando o preenchimento correto das informações fiscais.

## Alterações em Tabelas Existentes

### 1. `c000004` (Tabela Empresa)
- **Campo adicionado:** `ALIQUOTA_CBS` (alíquota geral da CBS)

### 2. `OPERACAO`
- **Campos adicionados:**
  - `CST_IBS_CBS`: Define o CST a ser utilizado na operação/movimento
  - `CCLASSTRIB_IBS_CBS`: Define o cClassTrib a ser utilizado na operação/movimento

### 3. Cadastro de Produto
- **Campos presentes:**
  - `CST_IBS_CBS`
  - `CCLASSTRIB_IBS_CBS`

Esses campos permitem definir, para cada produto, o CST e o cClassTrib aplicáveis ao IBS/CBS, garantindo o correto enquadramento tributário.

---

**Observação:**
Essas alterações visam adequar o sistema à nova legislação tributária, proporcionando flexibilidade e precisão no tratamento das regras fiscais do IBS e CBS.