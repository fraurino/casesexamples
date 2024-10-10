/*Código - Receita Estadual - Sefaz RS*/
/*fonte: https://receita.fazenda.rs.gov.br/download/7930*/
CREATE TABLE icms_codigos (
    codigo_ga INT NOT NULL,
    descricao_ga VARCHAR(255),
    codigo_gnre INT NOT NULL,
    descricao_gnre VARCHAR(255),
    detalhamento_receita VARCHAR(255),
    permite_icms_fecp BOOLEAN,
    multiplos_documentos BOOLEAN
);

INSERT INTO icms_codigos (codigo_ga, descricao_ga, codigo_gnre, descricao_gnre, detalhamento_receita, permite_icms_fecp, multiplos_documentos) VALUES
(226, 'ICMS - SERVIÇOS', 100013, 'ICMS Comunicação', 'Não tem', TRUE, FALSE),
(221, 'ICMS - MOD GERAL - COMÉRCIO', 100021, 'ICMS Energia Elétrica', 'ICMS Declarado Energia Elétrica', TRUE, FALSE),
(221, 'ICMS - MOD GERAL - COMÉRCIO', 100021, 'ICMS Energia Elétrica', 'ICMS Declarado Comércio', TRUE, FALSE),
(222, 'ICMS - MOD GERAL - INDÚSTRIA', 100021, 'ICMS Energia Elétrica', 'ICMS Declarado Indústria', TRUE, FALSE),
(223, 'ICMS - DENÚNCIA ESPONTÂNEA DE INFRAÇÃO', 100021, 'ICMS Energia Elétrica', 'ICMS Denúncia Espontânea de Infração', TRUE, FALSE),
(379, 'ICMS - PGTO ANTECIPADO NAS ENTRADAS DE MERCADORIAS ORIUNDAS DE OUTRAS UNIDADES DA FEDERAÇÃO, EFETUADO P/EMPRESAS NO REGIME DE TRIB SIMPLES NACIONAL', 100021, 'ICMS Energia Elétrica', 'ICMS Pagamento Antecipado nas Entradas de Mercadorias Oriundas de Outras Unidades da Federação Efetuado por Empresa Simples Nacional', TRUE, FALSE),
(995, 'ICMS - ESTORNO ETANOL ANIDRO - CONVÊNIO ICMS 110/2007', 100021, 'ICMS Energia Elétrica', 'ICMS Estorno Etanol Anidro', TRUE, FALSE),
(996, 'ICMS - ESTORNO BIODIESEL - CONVÊNIO 110/2007', 100021, 'ICMS Energia Elétrica', 'ICMS Estorno Biodiesel', TRUE, FALSE),
(228, 'ICMS - PAGAMENTO ANTECIPADO DE SERVIÇOS', 100030, 'ICMS Transporte', 'Não tem', FALSE, FALSE),
(224, 'ICMS - SUBSTITUIÇÃO TRIBUTÁRIA INTERESTADUAL', 100048, 'ICMS Substituição Tributária por Apuração', 'ICMS Substituição Tributária Interestadual por Apuração', TRUE, FALSE),
(229, 'ICMS - RESPONSABILIDADE POR SUBSTITUIÇÃO TRIBUTÁRIA DE SERVIÇOS DE TRANSPORTE', 100048, 'ICMS Substituição Tributária por Apuração', 'ICMS Substituição Tributária de Serviços de Transporte', TRUE, FALSE),
(270, 'ICMS - SUBSTITUIÇÃO TRIBUTÁRIA INTERNA', 100048, 'ICMS Substituição Tributária por Apuração', 'ICMS Substituição Tributária Interna', TRUE, FALSE),
(191, 'ICMS - SUBSTITUIÇÃO TRIBUTÁRIA INTERESTADUAL - REPASSE ICMS PROVISIONADO - CONVÊNIO 110/2007', 100048, 'ICMS Substituição Tributária por Apuração', 'Repasse ICMS Provisionado - Conv.110/07', TRUE, FALSE),
(1224, 'COMPLEMENTAÇÃO DE SUBSTITUIÇÃO TRIBUTÁRIA', 100048, 'ICMS Substituição Tributária por Apuração', 'ICMS ST Complemento', TRUE, FALSE),
(280, 'ICMS - IMPORTAÇÃO OU ARREMATAÇÃO DE MERCADORIA ESTRANGEIRA - PAGAMENTO NO DESEMBARAÇO ADUANEIRO', 100056, 'ICMS Importação', 'Não tem', TRUE, TRUE),
(227, 'ICMS - PGTO ANTECIPADO NAS ENTRADAS DE MERCADORIAS ORIUNDAS DE OUTRAS UNIDADES DA FEDERAÇÃO, EFETUADO POR EMPRESA MODALIDADE GERAL', 100080, 'ICMS Recolhimentos Especiais', 'ICMS Pagamento Antecipado nas Entradas de Mercadorias Oriundas de Outras Unidades da Federação Efetuado por Empresa Modalidade Geral', TRUE, FALSE),
(1500, 'ICMS - PAGAMENTO DE AUTORREGULARIZAÇÃO', 100080, 'ICMS Recolhimentos Especiais', 'ICMS Pagamento de Autorregularização', TRUE, FALSE),
(211, 'ICMS - OUTROS PAGAMENTOS ANTECIPADOS', 100080, 'ICMS Recolhimentos Especiais', 'ICMS Outros Pagamentos Antecipados', TRUE, FALSE),
(212, 'ICMS - PAGAMENTO DE PRODUTOR', 100080, 'ICMS Recolhimentos Especiais', 'ICMS Produtor Primário', TRUE, FALSE),
(214, 'ICMS - CARNE E GADO', 100080, 'ICMS Recolhimentos Especiais', 'ICMS Carne e Gado', TRUE, FALSE),
(999, 'ICMS ST - IMP OU MERC UF S/ CONV', 100099, 'ICMS Substituição Tributária por Operação', 'Não tem', TRUE, TRUE),
(1510, 'ICMS CONSUMIDOR FINAL NÃO CONTRIBUINTE OUTRA UF POR OPERAÇÃO', 100102, 'ICMS Consumidor Final Não Contribuinte Outra UF por Operação', 'Não tem', TRUE, TRUE),
(1511, 'ICMS CONSUMIDOR FINAL NÃO CONTRIBUINTE OUTRA UF POR APURAÇÃO', 100110, 'ICMS Consumidor Final Não Contribuinte Outra UF por Apuração', 'Não tem', TRUE, FALSE),
(1512, 'ICMS - AMPARA FUNDO ESTADUAL DE COMBATE À POBREZA POR OPERAÇÃO', 100129, 'ICMS Fundo Estadual de Combate à Pobreza por Operação', 'Não tem', FALSE, TRUE),
(1513, 'ICMS - AMPARA FUNDO ESTADUAL DE COMBATE À POBREZA POR APURAÇÃO', 100137, 'ICMS Fundo Estadual de Combate à Pobreza por Apuração', 'ICMS - Declarado Comércio', FALSE, FALSE),
(1514, 'ICMS - AMPARA SUBSTITUIÇÃO TRIBUTÁRIA FUNDO DE COMBATE À POBREZA POR APURAÇÃO', 100137, 'ICMS Fundo Estadual de Combate à Pobreza por Apuração', 'ICMS - Substituição Tributária Interna', FALSE, FALSE),
(1049, 'CONVÊNIO RESSARCIMENTO SVRS', 600016, 'Taxa Ressarcimento Convênio SEFAZ Virtual RS', FALSE, FALSE),
(1013, 'TAXA CDO - TAXA DE COOPERAÇÃO E DEFESA DA ORIZICULTURA - IRGA - POR OPERAÇÃO', 600016, 'Taxa Cooperação e Defesa da Orizicultura - CDO - IRGA - Por operação', FALSE, FALSE),
(760, 'RECEITA DE HONORÁRIOS ADVOCATICÍOS - FURPGE', 600016, 'Taxa Receita de Honorários Advocatícios - PGE RS', FALSE, FALSE),
(386, 'RECEITAS EVENTUAIS', 600016, 'Taxa Receitas Eventuais', FALSE, FALSE);
