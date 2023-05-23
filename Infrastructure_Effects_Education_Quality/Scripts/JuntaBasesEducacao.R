# Setup e bibliotecas ----
library(tidyverse)
library(magrittr)
library(dplyr)
library(readxl)

## Comandos Úteis
# Apagar Variáveis
rm(list = ls())
# Limpar Console
cat("\014")

## Settando o diretório
# Optei por esse método porque vai funcionar em qualquer computador :)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)
getwd()

# Lendo as bases ----
# PASTA DE DADOS: https://drive.google.com/drive/folders/1_IVjxQCluX9JYeL3RMlah60Elew0tSjW?usp=sharing
  # Salvar o arquivo resultante desse script ("Dados_Educacao_Final.csv") em uma pasta
  # chamada "Dados" no mesmo diretório do arquivo Markdown para debugar o notebook :)
# Projeto Google Cloud: https://console.cloud.google.com/bigquery?project=educacao-bd&ws=!1m0

## Definindo pasta de dados 
sPasta_Dados <- paste(sDiretorio_Base, "Dados", sep = "/")

## Nomes dos arquivos
sArquivo_MatriculasEscolas <- "Escolas_Censo_Matriculas_Mulheres.csv"
sArquivo_IDEB_Censo_Indicadores <- "IDEB_Escolas.csv"
sArquivo_SalariosRAIS <- "RAIS_Ocupacoes_Salarios.csv"
sArquivo_Despesas_Pop_Matriculas <- "Siconfi_Gastos_PIB_Pop_Matriculas.csv"
sArquivo_Professores <- "Escolas_Censo_Professores.csv"
sArquivo_FUNDEB <- "FUNDEB-por-Municipio.xlsx"
sArquivo_Trad_Municip  <- "Tradutor_Municipios.csv"

## Lendo as bases com read_csv()
dfMatriculasEscolas <- read_csv(paste(sPasta_Dados, sArquivo_MatriculasEscolas, sep = "/"))
dfIDEB <- read_csv(paste(sPasta_Dados, sArquivo_IDEB_Censo_Indicadores, sep = "/"))
dfSalariosRAIS <- read_csv(paste(sPasta_Dados, sArquivo_SalariosRAIS, sep = "/"))
dfDespesas_Pop_Matriculas <- read_csv(paste(sPasta_Dados, sArquivo_Despesas_Pop_Matriculas, sep = "/"))
dfProfessores <-read_csv(paste(sPasta_Dados, sArquivo_Professores, sep = "/"))

# Comentários ----

# No dfIDEB, há apenas dados das escolas que tiveram alguma nota no Ideb entre 2009 e 2019
# Por que 2009? Primeiro ano com informações do censo escolar
# Assim, a base com o número de matrículas do censo possui informações de um número maior de escolas

# Além disso, as colunas de IDEB_Mun e IDEB_UF possuem distinção por anos_escolares,
# Por conta disso e da forma como é calculada o Ideb nessas agregações, há algumas escolas
# que possuem IDEB, mas que não tiveram o IDEB de sua UF/município registrada 
# (levando em conta rede e anos_escolares)

# Quanto ao número de matrículas, não consegui achar essas informações destrinchadas por
# anos_escolares. Assim, se uma escola tem dados de IDEBs de diferentes etapas, ela terá a mesma
# informação de alunos matriculados.
# O número de matrículas também não foi tão fácil de conseguir: peguei os microdados do censo
# por aluno e agrupei/contei por escola. O mesmo foi feito com o número de professores.

# Despesas: empenhadas + resto a pagar

# A RAIS as vezes não tem informações de professores pra alguns municípios, o que
# faz com que muitos tenham dados incompletos de número de professores/remuneração.

# Juntando as bases ----
## IDEB e Matrículas ----
# Vetor de colunas para fazer o join
vColunasJoin <- c("uf", "id_municipio", "id_escola", "ano")

# Juntando
dfFinal <- merge(dfIDEB, dfMatriculasEscolas, by = vColunasJoin)

## Final e Professores
vColunasJoin <- c("uf", "id_municipio", "id_escola", "ano")
dfFinal <- merge(dfFinal, dfProfessores, by = vColunasJoin)

## Final e SalariosRais ----
vColunasJoin <- c("uf", "id_municipio", "ano")
dfFinal <- merge(dfFinal, dfSalariosRAIS, by = vColunasJoin)

## Final e SICONFI ----
vColunasJoin <- c("uf", "id_municipio", "ano")
dfFinal <- merge(dfFinal, dfDespesas_Pop_Matriculas, by = vColunasJoin)

## Deflatores - Início de 2019 ----
# Lendo Excel
dfDeflator <- readxl::read_excel(paste(sPasta_Dados, "Deflatores.xlsx", sep = "/"), sheet = 1)

# Juntando
dfFinal <- merge(dfFinal, dfDeflator[, c(1, 4, 6, 8)], by = c("ano"))


# Retirando Ensino Médio ----
dfFinal <- dfFinal %>% 
  filter(anos_escolares != "todos (1-4)")


# Deflacionando ----
## Despesa do município (Educação Pública TRU)
dfFinal$despesa_educ_mun_def <- dfFinal$despesa_educ_mun * 100 / dfFinal$indice_precos_educ_pub_2019
dfFinal$despesa_por_matricula_mun_def <- dfFinal$despesa_por_matricula_mun * 100 / dfFinal$indice_precos_educ_pub_2019

## PIB e PIB per Capita (Deflator Implícito do PIB)
dfFinal$pib_mun_def <- dfFinal$pib_mun * 100 / dfFinal$indice_precos_deflator_pib_2019
dfFinal$pib_per_capita_mun_def <- dfFinal$pib_per_capita_mun * 100 / dfFinal$indice_precos_deflator_pib_2019

## Despesa do Municipio / PIB
dfFinal$despesa_educ_perc_pib_mun_def <- dfFinal$despesa_educ_mun_def / dfFinal$pib_mun_def

## Despesa do Município por Aluno / PIB per Capita
dfFinal$despesa_matricula_pib_per_capita_mun <- dfFinal$despesa_por_matricula_mun / dfFinal$pib_per_capita_mun
dfFinal$despesa_matricula_pib_per_capita_mun_def <- dfFinal$despesa_por_matricula_mun_def / dfFinal$pib_per_capita_mun_def

# Adicionando outras variáveis relevantes ----
## Nível Municipal ----
## Número de alunos por professor
dfFinal$num_matriculas_por_professor_mun <- dfFinal$num_matriculas_mun / dfFinal$num_professores_mun

## Número de escolas
dfFinal <- dfFinal %>% 
  group_by(id_municipio, ano) %>% 
  mutate(num_escolas_mun = n_distinct(id_escola))

## Número de professores por escola
dfFinal$num_prof_por_escola_mun <- dfFinal$num_professores_mun / dfFinal$num_escolas_mun

## Número de alunos por escola
dfFinal$num_matriculas_por_escola_mun <- dfFinal$num_matriculas_mun / dfFinal$num_escolas_mun

## Número de professores per capita
dfFinal$num_prof_per_capita_mun <- dfFinal$num_professores_mun / dfFinal$populacao_mun

## Razão de Salários: usar relação frente ao salário mínimo :)
dfFinal$rais_razao_salario_ef1_mun <- dfFinal$rais_salario_medio_ef1_mun / dfFinal$salario_medio_mun
dfFinal$rais_razao_salario_ef2_mun <- dfFinal$rais_salario_medio_ef2_mun / dfFinal$salario_medio_mun


## Nível da Escola ----
## Número de alunos por professor
dfFinal$num_matriculas_por_professor <- dfFinal$num_matriculas / dfFinal$num_professores

### Proxy para a despesa da escola ----
# Metodologia: proxy usada pelo Tesouro Nacional
# Link: https://analise-siconfi-ideb.tesouro.gov.br/
dfFinal$proxy_despesa_escola <- dfFinal$despesa_por_matricula_mun * dfFinal$num_matriculas
dfFinal$proxy_despesa_escola_def <- dfFinal$despesa_por_matricula_mun_def * dfFinal$num_matriculas

# Removendo outliers ----
## Colocando como DataFrame
dfFinal <- as.data.frame(dfFinal)

## Análises de quantis ----
### Despesas com Educação > 0 ----
dfFinal %<>% 
  filter(despesa_educ_mun > 0)

### Proxy da Despesa por Escola ----
# Deflacionado sem distinção de anos
dfFinal %>% 
  summarise(quantil = scales::percent(c(.001, .005, .01, .1, .9, .99, .995, .999)),
            proxy_desp_escola_def = quantile(proxy_despesa_escola_def, 
                                             c(.001, .005, .01, .1, .9, .99, .995, .999),
                                             na.rm = T))

## Não-deflacionado 
dfFinal %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.001, .005, .01, .1, .9, .99, .995, .999)),
            proxy_desp_escola_def = quantile(proxy_despesa_escola, 
                                             c(.001, .005, .01, .1, .9, .99, .995, .999),
                                             na.rm = T)) %>% 
  pivot_wider(names_from = quantil, values_from = proxy_desp_escola_def, values_fill = 0)

### Gasto / PIB Municipal ----
# Deflacionado sem distinção de anos
dfFinal %>% 
  summarise(quantil = scales::percent(c(.001, .005, .01, .1, .9, .99, .995, .999)),
            gasto_pib_def = quantile(despesa_educ_perc_pib_mun_def, 
                                     c(.001, .005, .01, .1, .9, .99, .995, .999),
                                     na.rm = T))

## Não-deflacionado 
dfFinal %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.001, .01, .1, .25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(despesa_educ_perc_pib_mun, 
                                 c(.001, .01, .1, .25, .5, .75, .9, .99, .999),
                                 na.rm = T)) %>% 
  pivot_wider(names_from = quantil, values_from = gasto_pib, values_fill = 0)


## Retirando observações ----
dfFinal <- dfFinal %>% 
  filter(proxy_despesa_escola_def < quantile(proxy_despesa_escola_def, .999) &
           proxy_despesa_escola_def > quantile(proxy_despesa_escola_def, .001) &
           despesa_educ_perc_pib_mun < quantile(despesa_educ_perc_pib_mun, .999) &
           despesa_educ_perc_pib_mun > quantile(despesa_educ_perc_pib_mun, .001))


# JP ----------------------------------------------------------------

# Lendo a base do FUNDEB
dfFUNDEB <- read_excel(paste(sPasta_Dados, sArquivo_FUNDEB, sep = "/"))
Trad_Municip <- read_csv(paste(sPasta_Dados, sArquivo_Trad_Municip, sep = "/"))


# transformando as linhas do fundeb em colunas
dfFUNDEB <- dfFUNDEB %>% 
  pivot_longer(
    cols = c(6:20), # as colunas desse intervalo
    names_to = "ano", # ter?o seus nomes armazenados nessa nova coluna
    values_to = "fundeb")# e os seus valores armazenados nessa nova coluna 

#Transformando o dados mensais em anuais
dfFUNDEB <- dfFUNDEB %>% 
  group_by(COD_MUN, ano) %>%
  summarise(ano = ano,
            fundeb = sum(fundeb)) %>% unique()

# Fazendo o merge da base de tradu??o com base do FUNDEB
Trad_Municip$nome <- NULL
Trad_Municip <- Trad_Municip %>%
  rename(COD_MUN = id_municipio_receita)

dfFUNDEB <- merge(dfFUNDEB , Trad_Municip, by = "COD_MUN")
dfFUNDEB$COD_MUN <- NULL


# Fazendo o merge da Base final com a Base do FUNDEB
dfFUNDEB <- dfFUNDEB[order(dfFUNDEB$ano, dfFUNDEB$id_municipio),]
dfFinal <- dfFinal[order(dfFinal$ano, dfFinal$id_municipio),]

dfFinal <- merge(dfFinal,dfFUNDEB, by = c("ano", "id_municipio"))                           

# Exportando para CSV ----
# Ordenando por ano e id_escola decrescente (importante para permitir a inferência correta do read_csv)
dfFinal <- dfFinal %>% 
  arrange(desc(ano), desc(id_escola))

# Exportando
write_csv(dfFinal, file = paste(sPasta_Dados, "Dados_Educacao_Final.csv", sep = "/"))


# Descrevendo a base ----
options(max.print = 2000)
psych::describe(dfFinal)

# Colunas com alguns missings: 
#   IDEB_Mun e relacionados, 
#   quantidade_salas_utilizadas
#   Variáveis dos indicadores educacionais por anos_escolares, especialmente EF2
#   complexidade_gestao
#   RAIS professores
