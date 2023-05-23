# Setup e Bibliotecas ==========================================================
# Dados
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(skimr)

# Controle Sintético
library(tidysynth)
library(scpi)

# Gráficos e Tabelas
library(wesanderson)  # https://github.com/karthik/wesanderson
library(MetBrewer)  # https://github.com/BlakeRMills/MetBrewer/tree/main
library(stargazer)
library(ggplot2)
library(geomtextpath)

# Comandos Úteis
rm(list = ls())
cat("\014")

# Settando o diretório para o do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Baixando funções
funcs <- new.env()
source("1_Funcoes_CS.R", local = funcs, encoding = 'UTF-8')

# Customizando paletas e escalas
paleta_trend <- met.brewer("Signac")[c(14, 6)]
paleta_gap <- met.brewer("Benedictus")[c(13, 11)]
paleta_pesos <- c(met.brewer("Java")[3], met.brewer("Johnson")[4])
paleta_placebos <- c("lightgray", met.brewer("Signac")[14])

breaks_grafico <- 2
escala_trends <- c(1, 6, 1)
escala_gaps <- c(-1.5, 1, 0.25)
escala_trends <- NULL
escala_gaps <- NULL

# Definindo anos iniciais, pré-tratamento e finais
nAnoInicial <- 2005
nAnoInflexao <- 2012
nAnoTratamento <- 2016
nAnoFinalInv <- 2019
nAnoFinal <- 2021

# Dados ========================================================================
# Baixando
dfTodos <- read_excel(paste(sDiretorio, 
                            "Base_Investimento_Deficit_Juros.xlsx", 
                            sep = "/"),
                      sheet = "Dataset_Long")

# Definindo conjunto de doadores + Brasil
vDoadores <- c("BR",
               "ZA", "CL", "CO", "HU", "MX", "MY", "PE", "PL", "RU", "TH", "TR",
               "AR", "CN", "EC", "IN", "UY", "BG")

vDoadores_Juros <- c("BR",
                     "ZA", "CL", "CO", "HU", "MX", "MY", 
                     "PE", "PL", "RU", "TH", "TR")

# Lista de variáveis
cCovariadas <- c("Resultado_Primario_CA_PIB_Pot", "Inflacao_Consumidor",
                 "PIB_Capita_PPP17", "Taxa_Desemprego", "Valor_Add_Industria",
                 "Termos_Troca", "Conta_Corrente_PIB", "Taxa_Cambio100",
                 "Controle_Corrupcao", "Estabilidade_Politica")

cInv <- c("Investimento_Privado_PIB_PPP", 
          "Investimento_Publico_PIB_PPP",
          "Investimento_PIB_PPP", 
          "Investimento_PIB")

cJuros <- c("Taxa_Juros_Politica_CP", "Taxa_Juros_Real_Politica_CP")

cY <- c(cInv, cJuros)

# Restringindo e criando uma coluna de tratado
df <- dfTodos %>% 
   filter(iso2c %in% vDoadores_Juros) %>% 
   filter(Variável %in% c(cCovariadas, cJuros)) %>% 
   filter(Ano >= nAnoInicial) %>% 
   group_by(iso2c, Variável) %>% 
   arrange(Ano) %>% 
   mutate(PD = ifelse(Variável %in% cY, Valor - lag(Valor), Valor),
          Delta = ifelse(Variável %in% cY, Valor / lag(Valor), Valor),
          Valor100 = ifelse(Variável %in% cY, cumprod(c(100, Delta[-1])), Valor)) %>%  # 2016 = 100
   select(-Delta) %>% 
   as.data.frame() %>% 
   mutate(Tratado = ifelse((iso2c == "BR") & (Ano > nAnoTratamento), 1, 0))

## Datasets Largos -------------------------------------------------------------
# Nível
dfNivel <- df %>% 
   select(-PD, -Valor100) %>% 
   pivot_wider(names_from = Variável, values_from = Valor)

# Primeiras diferenças
dfPD <- df %>% 
   select(-Valor, -Valor100) %>% 
   pivot_wider(names_from = Variável, values_from = PD)

# 2003 = 100
dfNorm <- df %>% 
   select(-PD, -Valor) %>% 
   pivot_wider(names_from = Variável, values_from = Valor100)

# # Fazendo 2016 = 100
# ## DataFrame auxiliar
# df2016 <- dfNorm %>%
#     filter(Ano == 2016) %>%
#     select(iso2c, Variável, Valor100) %>%
#     rename(Valor100_2016 = Valor100)
# 
# ## Joining
# dfNorm %<>%
#     left_join(df2016, by = c("iso2c", "Variável")) %>%
#     mutate(Valor100 = ifelse(Variável %in% cY, Valor100 * 100 / Valor100_2016, Valor100)) %>%
#     select(-Valor100_2016) %>% 
#     pivot_wider(names_from = Variável, values_from = Valor100)

# Controles Sintéticos =========================================================
## Normalizado -----------------------------------------------------------------
### 1. Média de Y ####
# Controle Sintético
cs_Juro_Nom_NivelMediaY <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
   generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                      `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                      `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                      `Juro Nominal` = mean(Taxa_Juros_Politica_CP, na.rm = T)) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posNivelMediaY <- funcs$mspe_placebos_pos_trat(cs_Juro_Nom_NivelMediaY,
                                                      nAnoTratamento))
(mspe_pre_brNivelMediaY <- cs_Juro_Nom_NivelMediaY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficosMediaY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_Nom_NivelMediaY, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficosMediaY$pesos
funcs$tabela_media_cs(cs_Juro_Nom_NivelMediaY, stargazer = F)

## Trends e Gaps
lista_graficosMediaY$trends
lista_graficosMediaY$gap

## Inferência
lista_graficosMediaY$placebos
lista_graficosMediaY$razao_mspe
cs_Juro_Nom_NivelMediaY %>% grab_signficance()

### 2. Todos os Valores de Y ####
# Controle Sintético
cs_Juro_NomValoresY <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2006,
                      `Juro Nominal 2006` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2007,
                      `Juro Nominal 2007` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2008,
                      `Juro Nominal 2008` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2009,
                      `Juro Nominal 2009` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2010,
                      `Juro Nominal 2010` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2011,
                      `Juro Nominal 2011` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2012,
                      `Juro Nominal 2012` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2013,
                      `Juro Nominal 2013` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2014,
                      `Juro Nominal 2014` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2015,
                      `Juro Nominal 2015` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2016,
                      `Juro Nominal 2016` = Taxa_Juros_Politica_CP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posValoresY <- funcs$mspe_placebos_pos_trat(cs_Juro_NomValoresY,
                                                         nAnoTratamento))
(mspe_pre_brValoresY <- cs_Juro_NomValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_ValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomValoresY, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_ValoresY$pesos
funcs$tabela_media_cs(cs_Juro_NomValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_ValoresY$trends
lista_graficos_ValoresY$gap

## Inferência
lista_graficos_ValoresY$placebos
lista_graficos_ValoresY$razao_mspe
cs_Juro_NomValoresY %>% grab_signficance()

### 3. Inflexões ####
# Controle Sintético
cs_Juro_NomInflexoes <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   generate_predictor(time_window = nAnoInflexao - 1,
                      `Juro Nominal 2011` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = nAnoTratamento,
                      `Juro Nominal 2016` = Taxa_Juros_Politica_CP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posInflexoes <- funcs$mspe_placebos_pos_trat(cs_Juro_NomInflexoes, 
                                                          nAnoTratamento))
(mspe_pre_brInflexoes <- cs_Juro_NomInflexoes %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_Inflexoes <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomInflexoes, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_Inflexoes$pesos
funcs$tabela_media_cs(cs_Juro_NomInflexoes, stargazer = F)

## Trends e Gaps
lista_graficos_Inflexoes$trends
lista_graficos_Inflexoes$gap

## Inferência
lista_graficos_Inflexoes$placebos
lista_graficos_Inflexoes$razao_mspe
cs_Juro_NomInflexoes %>% grab_signficance()


### 4. Primeiros 75% dos Valores de Y ####
cs_Juro_NomTresQuartosValoresY <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2006,
                      `Juro Nominal 2006` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2007,
                      `Juro Nominal 2007` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2008,
                      `Juro Nominal 2008` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2009,
                      `Juro Nominal 2009` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2010,
                      `Juro Nominal 2010` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2011,
                      `Juro Nominal 2011` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2012,
                      `Juro Nominal 2012` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2013,
                      `Juro Nominal 2013` = Taxa_Juros_Politica_CP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posTresQuartosValoresY <- funcs$mspe_placebos_pos_trat(
   cs_Juro_NomTresQuartosValoresY, nAnoTratamento
))
(mspe_pre_brTresQuartosValoresY <- cs_Juro_NomTresQuartosValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_TresQuartosValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomTresQuartosValoresY, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_TresQuartosValoresY$pesos
funcs$tabela_media_cs(cs_Juro_NomTresQuartosValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_TresQuartosValoresY$trends
lista_graficos_TresQuartosValoresY$gap

## Inferência
lista_graficos_TresQuartosValoresY$placebos
lista_graficos_TresQuartosValoresY$razao_mspe
cs_Juro_NomTresQuartosValoresY %>% grab_signficance()

### 5. Últimos 25% dos Valores de Y ####
cs_Juro_NomUltQuartoValoresY <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2014,
                      `Juro Nominal 2014` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2015,
                      `Juro Nominal 2015` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2016,
                      `Juro Nominal 2016` = Taxa_Juros_Politica_CP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posUltQuartoValoresY <- funcs$mspe_placebos_pos_trat(
   cs_Juro_NomUltQuartoValoresY, nAnoTratamento
))
(mspe_pre_brUltQuartoValoresY <- cs_Juro_NomUltQuartoValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_UltQuartoValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomUltQuartoValoresY, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_UltQuartoValoresY$pesos
funcs$tabela_media_cs(cs_Juro_NomUltQuartoValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_UltQuartoValoresY$trends
lista_graficos_UltQuartoValoresY$gap

## Inferência
lista_graficos_UltQuartoValoresY$placebos
lista_graficos_UltQuartoValoresY$razao_mspe
cs_Juro_NomUltQuartoValoresY %>% grab_signficance()


### 6. Apenas Covariadas Externas ####
cs_Juro_NomControles <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Inflação` = mean(Inflacao_Consumidor, na.rm = T),
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posControles <- funcs$mspe_placebos_pos_trat(cs_Juro_NomControles, 
                                                          nAnoTratamento))
(mspe_pre_brControles <- cs_Juro_NomControles %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_Controles <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomControles, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_Controles$pesos
funcs$tabela_media_cs(cs_Juro_NomControles, stargazer = F)

## Trends e Gaps
lista_graficos_Controles$trends
lista_graficos_Controles$gap

## Inferência
lista_graficos_Controles$placebos
lista_graficos_Controles$razao_mspe
cs_Juro_NomControles %>% grab_signficance()


### 7. Apenas os Valores de Y ####
# Controle Sintético
cs_Juro_NomApenasValoresY <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2006,
                      `Juro Nominal 2006` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2007,
                      `Juro Nominal 2007` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2008,
                      `Juro Nominal 2008` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2009,
                      `Juro Nominal 2009` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2010,
                      `Juro Nominal 2010` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2011,
                      `Juro Nominal 2011` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2012,
                      `Juro Nominal 2012` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2013,
                      `Juro Nominal 2013` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2014,
                      `Juro Nominal 2014` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2015,
                      `Juro Nominal 2015` = Taxa_Juros_Politica_CP) %>% 
   generate_predictor(time_window = 2016,
                      `Juro Nominal 2016` = Taxa_Juros_Politica_CP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_posApenasValoresY <- funcs$mspe_placebos_pos_trat(
   cs_Juro_NomApenasValoresY,nAnoTratamento
))

(mspe_pre_brApenasValoresY <- cs_Juro_NomApenasValoresY %>% 
   grab_signficance() %>% 
   filter(unit_name == "BR") %>% 
   pull(pre_mspe))

# Gráficos
lista_graficos_ApenasValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_Juro_NomApenasValoresY, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)", 
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinal, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10
)

## Pesos e médias
lista_graficos_ApenasValoresY$pesos
funcs$tabela_media_cs(cs_Juro_NomApenasValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_ApenasValoresY$trends
lista_graficos_ApenasValoresY$gap

## Inferência
lista_graficos_ApenasValoresY$placebos
lista_graficos_ApenasValoresY$razao_mspe
cs_Juro_NomApenasValoresY %>% grab_signficance()


### VENCEDOR: APENAS VALORES Y (608.3) ####
## Pesos e médias
lista_graficos_ApenasValoresY$pesos
ggsave("Figuras/Taxa_Juros/Nom_Norm_Pesos.pdf", dpi = 1200)

## Trends e Gaps
lista_graficos_ApenasValoresY$trends
ggsave("Figuras/Taxa_Juros/Nom_Norm_Trends.pdf", dpi = 1200)

lista_graficos_ApenasValoresY$gap
ggsave("Figuras/Taxa_Juros/Nom_Norm_Gap.pdf", dpi = 1200)

## Inferência
lista_graficos_ApenasValoresY$placebos
ggsave("Figuras/Taxa_Juros/Nom_Norm_Placebos.pdf", dpi = 1200)

lista_graficos_ApenasValoresY$razao_mspe
ggsave("Figuras/Taxa_Juros/Nom_Norm_Razao_MSPE.pdf", dpi = 1200)

### Investigando México e Malásia
# México continuou uma grande escalada de juros, enquanto o sintético ficou estável
# Economia crescendo cerca de 1p.p entre 2016-18 e com recessão já em 2019
# Robustez de tirar maior peso pode ser interessante
# Malásia: seu CS é o Brasil (maior peso), o que faz com o sintético descole muito
cs_Juro_NomApenasValoresY %>% 
   grab_synthetic_control(placebo = T) %>% 
   filter(.id == "MX") %>% 
   select(-.placebo) %>% 
   pivot_longer(cols = c(real_y, synth_y)) %>% 
   ggplot(aes(x = time_unit, y = value, colour = name)) +
   geom_line()

## scpi ------------------------------------------------------------------------
### Normal #####################################################################
# Preparando dados
dfNorm_Juro_Nom_scpi <- scdata(
   dfNorm %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (2005:2016),
   period.post = (2017:2021), 
   unit.tr = "BR", unit.co = vDoadores_Juros[-1]
)

# Estimando com pesos estritamente positivos ou zero
cs_Juro_Nom_scpi <- scest(dfNorm_Juro_Nom_scpi, 
                          w.constr = list(name = "simplex", Q = 1))

# Vendo resultados
summary(cs_Juro_Nom_scpi)
scplot(cs_Juro_Nom_scpi)

# Placebos
df_placebos_scpi <- funcs$placebos_scpi(
   dados = dfNorm %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1)
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi <- funcs$razao_mspe_scpi(df_placebos_scpi)
mspe_pos_scpi <- mean(df_razao_mspe_scpi$post_mspe[df_razao_mspe_scpi$type == "donor"])

# Gráficos
lista_graficos_scpi <- funcs$graficos_sc_scpi(
   objeto_sc_scpi = cs_Juro_Nom_scpi,
   df_placebos = df_placebos_scpi,
   df_razao = df_razao_mspe_scpi,
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos
)

lista_graficos_scpi$pesos
ggsave("Figuras/Taxa_Juros/Nom_Norm_Pesos_2021.pdf", dpi = 1200)

lista_graficos_scpi$trends
ggsave("Figuras/Taxa_Juros/Nom_Norm_Trends_2021.pdf", dpi = 1200)

lista_graficos_scpi$gap
ggsave("Figuras/Taxa_Juros/Nom_Norm_Gap_2021.pdf", dpi = 1200)

lista_graficos_scpi$placebos
ggsave("Figuras/Taxa_Juros/Nom_Norm_Placebos_2021.pdf", dpi = 1200)

# México: juros muito altos e crescentes (sintético estável)
# Malásia: principal doador é o Brasil, sintético cai enquanto juros são estáveis
lista_graficos_scpi$razao_mspe
ggsave("Figuras/Taxa_Juros/Nom_Norm_Razao_MSPE_2021.pdf", dpi = 1200)
