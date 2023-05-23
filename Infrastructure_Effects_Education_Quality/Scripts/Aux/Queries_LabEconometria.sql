-- Projeto Google Cloud: https://console.cloud.google.com/bigquery?project=educacao-bd&ws=!1m0

-- Query que coleta informações do IDEB, do Censo Escolar e dos Indicadores Educacionais do IDEB para cada escola, além de informações do IDEB a nível de município e UF.
SELECT
    municipios.nome_regiao as regiao,
    ideb.sigla_uf as uf,
    ideb.id_municipio as id_municipio,
    municipios.nome as municipio, 
    ideb.id_escola as id_escola, 
    CAST(ideb.ano AS INTEGER) as ano, /* 2009 a 2019 */
    ideb.anos_escolares,
    ideb.ensino,
    censo.rede, /* só municipais */
    IF(censo.tipo_localizacao = "1", "urbano", "rural") as localizacao,
    ideb.indicador_rendimento as rendimento_aprovacao_ideb,
    ideb.nota_saeb_media_padronizada as saeb_ideb,
    ideb.nota_saeb_media_padronizada * ideb.indicador_rendimento as ideb,  /* sem IDEB nulo */
    ideb_municipio.indicador_rendimento as rendimento_aprovacao_ideb_mun,
    ideb_municipio.nota_saeb_media_padronizada as saeb_ideb_mun,
    ideb_municipio.nota_saeb_media_padronizada * ideb_municipio.indicador_rendimento as ideb_mun,
    IF(censo.agua_inexistente = 1, 0, 1) as est_agua,
    IF(censo.energia_inexistente = 1, 0, 1) as est_energia,
    IF(censo.esgoto_inexistente = 1, 0, 1) as est_esgoto,
    COALESCE(censo.agua_rede_publica , 0) as est_agua_rede_publica,
    COALESCE(censo.energia_rede_publica , 0) as est_energia_rede_publica,
    COALESCE(censo.energia_gerador , 0) as est_energia_gerador,    COALESCE(censo.esgoto_rede_publica , 0) as est_esgoto_rede_publica,
    COALESCE(censo.esgoto_fossa , 0) as est_esgoto_fossa,
    IF(censo.banheiro_dentro_predio = 1 OR censo.banheiro_fora_predio = 1, 1, 0) as est_banheiro,
    COALESCE(censo.lixo_servico_coleta , 0) est_coletalixo,
    COALESCE(censo.quadra_esportes , 0) as est_quadraesportes,
    COALESCE(censo.biblioteca , 0) as est_biblioteca,
    COALESCE(censo.sala_diretoria , 0) as est_saladiretor,
    COALESCE(censo.auditorio , 0) as est_auditorio,
    COALESCE(censo.laboratorio_ciencias , 0) as est_lab_ciencias,
    COALESCE(censo.cozinha , 0) as alim_cozinha,
    COALESCE(censo.alimentacao , 0) as alim_alimentacao,
    COALESCE(censo.refeitorio , 0) as alim_refeitorio,
    censo.tipo_atividade_complementar as alim_atividades_complementares,
    COALESCE(censo.area_verde, 0) as alim_area_verde,
    COALESCE(censo.internet , 0) as tec_internet,
    COALESCE(censo.banda_larga , 0) as tec_banda_larga,
    COALESCE(censo.laboratorio_informatica, 0) as tec_lab_informatica,
    IF(censo.quantidade_computador > 0, 1, 0) as tec_computador,
    IF(censo.quantidade_equipamento_impressora > 0, 1, 0) as tec_impressora,
    IF(censo.quantidade_equipamento_tv > 0, 1, 0) as tec_tv,
    censo.quantidade_salas_utilizadas,
    censo.eja,
    indicadores.atu_ef_anos_iniciais as media_alunos_turma_fundamental1,
    indicadores.atu_ef_anos_finais as media_alunos_turma_fundamental2,
    indicadores.atu_ef as media_alunos_turma_fundamental,
    indicadores.dsu_ef_anos_iniciais as docentes_cursosuperior_fundamental1,
    indicadores.dsu_ef_anos_finais as docentes_cursosuperior_fundamental2,
    indicadores.dsu_ef as docentes_cursosuperior_fundamental,
    indicadores.had_ef_anos_iniciais as media_horasaula_diaria_fundamental1,
    indicadores.had_ef_anos_finais as media_horasaula_diaria_fundamental2,
    indicadores.had_ef as media_horasaula_diaria_fundamental,
    indicadores.icg_nivel_complexidade_gestao_escola as complexidade_gestao
FROM `basedosdados.br_inep_ideb.escola` AS ideb
INNER JOIN `basedosdados.br_inep_censo_escolar.escola` AS censo
    ON ideb.ano = censo.ano 
    AND ideb.id_escola = censo.id_escola
INNER JOIN `basedosdados.br_inep_indicadores_educacionais.escola` AS indicadores
    ON censo.ano = indicadores.ano 
    AND censo.id_escola = indicadores.id_escola
LEFT JOIN `basedosdados.br_inep_ideb.municipio` AS ideb_municipio
    ON ideb.id_municipio = ideb_municipio.id_municipio 
    AND ideb.ano = ideb_municipio.ano 
    AND ideb.anos_escolares = ideb_municipio.anos_escolares
    AND ideb.rede = ideb_municipio.rede
LEFT JOIN `basedosdados.br_bd_diretorios_brasil.municipio` as municipios
    ON municipios.id_municipio = ideb.id_municipio
WHERE 
    ideb.ideb IS NOT NULL
    AND ideb.rede = "municipal"
    -- AND sigla_uf IN ("RO", "AC", "AM", "RR", "PA", "AP", "TO")
ORDER BY ano DESC, id_escola DESC, anos_escolares ASC


-- Query que coleta o número de matrículas e o percentual de alunas mulheres em cada escola.
SELECT 
    sigla_uf as uf,
    id_municipio,
    id_escola,
    ano,
    COUNT(DISTINCT id_matricula) as num_matriculas,
    ROUND(COUNT(DISTINCT (CASE WHEN sexo = "2" THEN id_matricula END)) / COUNT(DISTINCT id_matricula), 3) as perc_mulheres
FROM `basedosdados.br_inep_censo_escolar.matricula` 
WHERE 
    ano IN (2009, 2011, 2013, 2015, 2017, 2019)
    AND rede = "3"
GROUP BY uf, id_municipio, id_escola, ano
ORDER BY id_escola, ano

-- Query que coleta informações sobre o número de professores por escola e município
SELECT 
    escolas.*,
    municipios.num_professores_mun
FROM (
    SELECT 
        sigla_uf as uf,
        id_municipio,
        id_escola,
        ano,
        COUNT(DISTINCT id_docente) as num_professores
    FROM `basedosdados.br_inep_censo_escolar.docente` 
    WHERE 
        ano IN (2009, 2011, 2013, 2015, 2017, 2019)
        AND rede = "municipal"
    GROUP BY uf, id_municipio, id_escola, ano
    ORDER BY id_escola, ano
) escolas
LEFT JOIN (
    SELECT 
        id_municipio,
        ano,
        COUNT(DISTINCT id_docente) as num_professores_mun
    FROM `basedosdados.br_inep_censo_escolar.docente` 
    WHERE 
        rede = "municipal"
    GROUP BY id_municipio, ano
) municipios
ON escolas.id_municipio = municipios.id_municipio AND escolas.ano = municipios.ano
ORDER BY id_municipio ASC, ano ASC


-- Query que coleta informações municipais: gastos com educação (SICONFI), PIB, população e números de matrícula
SELECT
    siconfi.sigla_uf as uf,
    siconfi.ano,
    siconfi.id_municipio,
    ROUND(SUM(siconfi.valor), 2) as despesa_educ_mun, /* Seria interessante normalizar os gastos para cada ano; valores em reais correntes */
    ROUND(SUM(siconfi.valor) / SUM(pib.pib), 3) as despesa_educ_perc_pib_mun,
    ROUND(SUM(siconfi.valor) / SUM(a.num_matriculas), 3) as despesa_por_matricula_mun,
    SUM(a.num_matriculas) as num_matriculas_mun, /* apenas rede municipal */
    SUM(pib.pib) as pib_mun,
    ROUND(SUM(pib.va_adespss) / SUM(pib.pib), 3) as percentual_admpub_pib_mun,
    SUM(populacao.populacao) as populacao_mun,
    ROUND(SUM(pib.pib) / SUM(populacao.populacao), 3) as pib_per_capita_mun
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao` AS siconfi
INNER JOIN (
    SELECT 
        id_municipio,
        ano,
        COUNT(DISTINCT id_matricula) as num_matriculas
    FROM `basedosdados.br_inep_censo_escolar.matricula` 
    WHERE 
        rede = "3"
    GROUP BY id_municipio, ano
) a
    ON siconfi.id_municipio = a.id_municipio AND siconfi.ano = a.ano
LEFT JOIN `basedosdados.br_ibge_pib.municipio` AS pib
    ON siconfi.id_municipio = pib.id_municipio 
    AND siconfi.ano = pib.ano
LEFT JOIN `basedosdados.br_ibge_populacao.municipio` AS populacao
    ON siconfi.id_municipio = populacao.id_municipio 
    AND siconfi.ano = populacao.ano
WHERE
  siconfi.ano IN (2009, 2011, 2013, 2015, 2017, 2019)
  AND siconfi.conta_bd = "Educação" /* Educação é a soma de todos os outros subcampos; por haver dados incompletos, optou-se por usar apenas gastos totais com Educação */
  AND siconfi.estagio_bd IN ("Despesas Empenhadas", /* Despesa "reservada" para esse fim */ 
    "Inscrição de Restos a Pagar Não Processados", 
    "Inscrição de Restos a Pagar Processados") 
GROUP BY ano, uf, id_municipio
ORDER BY ano ASC, id_municipio ASC


-- Query que coleta informações municipais da RAIS: número de empregos e salário médio (geral e por etapa educacional) 
SELECT
    rais.sigla_uf as uf,
    rais.id_municipio as id_municipio,
    ano,
    SUM(vinculo_ativo_3112) as num_empregos_formais_mun,
    ROUND(AVG(valor_remuneracao_media), 2) as salario_medio_mun,
    ROUND(AVG(valor_remuneracao_media_sm), 2) as salario_medio_sm_mun,
    SUM(CASE WHEN cbo_2002 LIKE "2312%" THEN vinculo_ativo_3112 END) as rais_num_professores_ef1_mun,
    SUM(CASE WHEN cbo_2002 LIKE "2313%" THEN vinculo_ativo_3112 END) as rais_num_professores_ef2_mun,
    SUM(CASE WHEN cbo_2002 LIKE "2321%" THEN vinculo_ativo_3112 END) as rais_num_professores_em_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2312%" THEN valor_remuneracao_media END), 2) as rais_salario_medio_ef1_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2313%" THEN valor_remuneracao_media END), 2) as rais_salario_medio_ef2_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2321%" THEN valor_remuneracao_media END), 2) as rais_salario_medio_em_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2312%" THEN valor_remuneracao_media_sm END), 2) as rais_salario_medio_ef1_sm_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2313%" THEN valor_remuneracao_media_sm END), 2) as rais_salario_medio_ef2_sm_mun,
    ROUND(AVG(CASE WHEN cbo_2002 LIKE "2321%" THEN valor_remuneracao_media_sm END), 2) as rais_salario_medio_em_sm_mun
FROM `basedosdados.br_me_rais.microdados_vinculos` as rais
WHERE 
    rais.ano IN (2009, 2011, 2013, 2015, 2017, 2019)
GROUP BY uf, id_municipio, ano
ORDER BY ano ASC, id_municipio ASC

-- IDEB por UF
SELECT
    ano,
    sigla_uf as uf,
    anos_escolares,
    indicador_rendimento as rendimento_aprovacao_ideb_uf,
    nota_saeb_media_padronizada as saeb_ideb_uf,
    nota_saeb_media_padronizada *  indicador_rendimento as ideb_uf
FROM `basedosdados.br_inep_ideb.uf`
WHERE 
    ano > 2007
    AND rede NOT IN ("privada", "estadual") /* não tem rede municipal */
    AND ensino = "fundamental"
ORDER BY ano ASC, uf ASC