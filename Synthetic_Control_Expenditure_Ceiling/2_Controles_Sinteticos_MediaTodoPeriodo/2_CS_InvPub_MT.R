# Setup e Bibliotecas ==========================================================
# MT: Média todo o período
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
   filter(iso2c %in% vDoadores) %>% 
   filter(Variável %in% c(cCovariadas, cY)) %>% 
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
## Nível -----------------------------------------------------------------------
### 1. Média de Y ####
# Controle Sintético
cs_InvPub_NivelMediaY <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                      `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                      `Investimento Público` = mean(Investimento_Publico_PIB_PPP, na.rm = T)) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_NivelMediaY <- funcs$mspe_placebos_pos_trat(cs_InvPub_NivelMediaY,
                                                         nAnoTratamento))
(mspe_pre_br_NivelMediaY <- cs_InvPub_NivelMediaY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficosMediaY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_NivelMediaY, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_NivelMediaY, stargazer = F)

## Trends e Gaps
lista_graficosMediaY$trends
lista_graficosMediaY$gap

## Inferência
lista_graficosMediaY$placebos
lista_graficosMediaY$razao_mspe
cs_InvPub_NivelMediaY %>% grab_signficance()

### 2. Todos os Valores de Y ####
# Controle Sintético
cs_InvPub_Nivel_ValoresY <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2005,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = 2006,
                      `Investimento Público 2006` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2007,
                      `Investimento Público 2007` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2008,
                      `Investimento Público 2008` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2009,
                      `Investimento Público 2009` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2010,
                      `Investimento Público 2010` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2011,
                      `Investimento Público 2011` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2012,
                      `Investimento Público 2012` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2013,
                      `Investimento Público 2013` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2014,
                      `Investimento Público 2014` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2015,
                      `Investimento Público 2015` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2016,
                      `Investimento Público 2016` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_Nivel_ValoresY <- funcs$mspe_placebos_pos_trat(cs_InvPub_Nivel_ValoresY,
                                                         nAnoTratamento))
(mspe_pre_br_Nivel_ValoresY <- cs_InvPub_Nivel_ValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_ValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_ValoresY, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_ValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_ValoresY$trends
lista_graficos_ValoresY$gap

## Inferência
lista_graficos_ValoresY$placebos
lista_graficos_ValoresY$razao_mspe
cs_InvPub_Nivel_ValoresY %>% grab_signficance()

### 3. Inflexões ####
# Controle Sintético
cs_InvPub_Nivel_Inflexoes <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   generate_predictor(time_window = nAnoInicial,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = nAnoInflexao - 1,
                      `Investimento Público 2011` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = nAnoTratamento,
                      `Investimento Público 2016` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_Nivel_Inflexoes <- funcs$mspe_placebos_pos_trat(cs_InvPub_Nivel_Inflexoes, 
                                                          nAnoTratamento))
(mspe_pre_br_Nivel_Inflexoes <- cs_InvPub_Nivel_Inflexoes %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_Inflexoes <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_Inflexoes, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_Inflexoes, stargazer = F)

## Trends e Gaps
lista_graficos_Inflexoes$trends
lista_graficos_Inflexoes$gap

## Inferência
lista_graficos_Inflexoes$placebos
lista_graficos_Inflexoes$razao_mspe
cs_InvPub_Nivel_Inflexoes %>% grab_signficance()


### 4. Primeiros 75% dos Valores de Y ####
cs_InvPub_Nivel_TresQuartosValoresY <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = F) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2005,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = 2006,
                      `Investimento Público 2006` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2007,
                      `Investimento Público 2007` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2008,
                      `Investimento Público 2008` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2009,
                      `Investimento Público 2009` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2010,
                      `Investimento Público 2010` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2011,
                      `Investimento Público 2011` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2012,
                      `Investimento Público 2012` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2013,
                      `Investimento Público 2013` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_Nivel_TresQuartosValoresY <- funcs$mspe_placebos_pos_trat(
   cs_InvPub_Nivel_TresQuartosValoresY, nAnoTratamento
))
(mspe_pre_br_Nivel_TresQuartosValoresY <- cs_InvPub_Nivel_TresQuartosValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_TresQuartosValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_TresQuartosValoresY, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_TresQuartosValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_TresQuartosValoresY$trends
lista_graficos_TresQuartosValoresY$gap

## Inferência
lista_graficos_TresQuartosValoresY$placebos
lista_graficos_TresQuartosValoresY$razao_mspe
cs_InvPub_Nivel_TresQuartosValoresY %>% grab_signficance()

### 5. Últimos 25% dos Valores de Y ####
cs_InvPub_Nivel_UltQuartoValoresY <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
                     `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                     `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                     `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                     `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                     `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                     `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)
  ) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2014,
                      `Investimento Público 2014` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2015,
                      `Investimento Público 2015` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2016,
                      `Investimento Público 2016` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_Nivel_UltQuartoValoresY <- funcs$mspe_placebos_pos_trat(
   cs_InvPub_Nivel_UltQuartoValoresY, nAnoTratamento
))
(mspe_pre_br_Nivel_UltQuartoValoresY <- cs_InvPub_Nivel_UltQuartoValoresY %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_UltQuartoValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_UltQuartoValoresY, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_UltQuartoValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_UltQuartoValoresY$trends
lista_graficos_UltQuartoValoresY$gap

## Inferência
lista_graficos_UltQuartoValoresY$placebos
lista_graficos_UltQuartoValoresY$razao_mspe
cs_InvPub_Nivel_UltQuartoValoresY %>% grab_signficance()


### 6. Apenas Covariadas Externas ####
cs_InvPub_Nivel_Controles <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
  generate_predictor(time_window = nAnoInicial:nAnoTratamento,
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
(mspe_pos_Nivel_Controles <- funcs$mspe_placebos_pos_trat(cs_InvPub_Nivel_Controles, 
                                                          nAnoTratamento))
(mspe_pre_br_Nivel_Controles <- cs_InvPub_Nivel_Controles %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_Controles <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_Controles, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_Controles, stargazer = F)

## Trends e Gaps
lista_graficos_Controles$trends
lista_graficos_Controles$gap

## Inferência
lista_graficos_Controles$placebos
lista_graficos_Controles$razao_mspe
cs_InvPub_Nivel_Controles %>% grab_signficance()


### 7. Apenas os Valores de Y ####
# Controle Sintético
cs_InvPub_Nivel_ApenasValoresY <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: valores pré_tratamento
   generate_predictor(time_window = 2005,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = 2006,
                      `Investimento Público 2006` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2007,
                      `Investimento Público 2007` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2008,
                      `Investimento Público 2008` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2009,
                      `Investimento Público 2009` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2010,
                      `Investimento Público 2010` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2011,
                      `Investimento Público 2011` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2012,
                      `Investimento Público 2012` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2013,
                      `Investimento Público 2013` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2014,
                      `Investimento Público 2014` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2015,
                      `Investimento Público 2015` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2016,
                      `Investimento Público 2016` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_Nivel_ApenasValoresY <- funcs$mspe_placebos_pos_trat(
   cs_InvPub_Nivel_ApenasValoresY,nAnoTratamento
))

(mspe_pre_br_Nivel_ApenasValoresY <- cs_InvPub_Nivel_ApenasValoresY %>% 
   grab_signficance() %>% 
   filter(unit_name == "BR") %>% 
   pull(pre_mspe))

# Gráficos
lista_graficos_ApenasValoresY <- funcs$graficos_cs(
   objeto_cs_tidy = cs_InvPub_Nivel_ApenasValoresY, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
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
funcs$tabela_media_cs(cs_InvPub_Nivel_ApenasValoresY, stargazer = F)

## Trends e Gaps
lista_graficos_ApenasValoresY$trends
lista_graficos_ApenasValoresY$gap

## Inferência
lista_graficos_ApenasValoresY$placebos
lista_graficos_ApenasValoresY$razao_mspe
cs_InvPub_Nivel_ApenasValoresY %>% grab_signficance()

### VENCEDOR: ÚLTIMO QUARTO (6.84) ####
## Pesos e médias
lista_graficos_UltQuartoValoresY$pesos
ggsave("Figuras/Investimento_Publico/Pesos_InvPub.pdf", dpi = 1200)

## Trends e Gaps
lista_graficos_UltQuartoValoresY$trends
ggsave("Figuras/Investimento_Publico/Trends_InvPub.pdf", dpi = 1200)

lista_graficos_UltQuartoValoresY$gap
ggsave("Figuras/Investimento_Publico/Gap_InvPub.pdf", dpi = 1200)

## Inferência
lista_graficos_UltQuartoValoresY$placebos
ggsave("Figuras/Investimento_Publico/Placebos_InvPub.pdf", dpi = 1200)

lista_graficos_UltQuartoValoresY$razao_mspe
ggsave("Figuras/Investimento_Publico/Razao_MSPE_InvPub.pdf", dpi = 1200)


## scpi ------------------------------------------------------------------------
### Normal #####################################################################
# Preparando dados
dfNivel_InvPub_scpi <- scdata(
   dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinalInv)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Investimento_Publico_PIB_PPP", 
   period.pre = (2005:2016),
   period.post = (2017:2019), 
   unit.tr = "BR", unit.co = vDoadores[-1]
)

# Estimando com pesos estritamente positivos ou zero
cs_InvPub_Nivel_scpi <- scest(dfNivel_InvPub_scpi, 
                              w.constr = list(name = "simplex", Q = 1))

# Vendo resultados
summary(cs_InvPub_Nivel_scpi)
scplot(cs_InvPub_Nivel_scpi)

# Placebos
df_placebos_scpi <- funcs$placebos_scpi(
   dados = dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinalInv)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Investimento_Publico_PIB_PPP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinalInv,
   restricao_w = list(name = "simplex", Q = 1)
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi <- funcs$mspe_pre_trat_scpi(cs_InvPub_Nivel_scpi))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi <- funcs$razao_mspe_scpi(df_placebos_scpi)
mspe_pos_scpi <- mean(df_razao_mspe_scpi$post_mspe[df_razao_mspe_scpi$type == "donor"])

# Gráficos
lista_graficos_scpi <- funcs$graficos_sc_scpi(
   objeto_sc_scpi = cs_InvPub_Nivel_scpi,
   df_placebos = df_placebos_scpi,
   df_razao = df_razao_mspe_scpi,
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos
)

lista_graficos_scpi$pesos
lista_graficos_scpi$trends
lista_graficos_scpi$gap

lista_graficos_scpi$placebos
lista_graficos_scpi$razao_mspe

# Com intervalos de confiança
res_scpi <- scpi(data = dfNivel_InvPub_scpi, w.constr = list(name = "simplex", Q = 1))
scplot(res_scpi)

### Demeaned - Ferman & Pinto (2021) ###########################################
# Preparando dados
dfNivel_InvPub_scpi_demeaned <- scdata(
   dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinalInv)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Investimento_Publico_PIB_PPP", 
   period.pre = (2005:2016),
   period.post = (2017:2019), 
   unit.tr = "BR", unit.co = vDoadores[-1],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_InvPub_Nivel_scpi_demeaned <- scest(dfNivel_InvPub_scpi_demeaned, 
                                       w.constr = list(name = "simplex", Q = 1))

# Vendo resultados
summary(cs_InvPub_Nivel_scpi_demeaned)
scplot(cs_InvPub_Nivel_scpi_demeaned)

# Placebos
df_placebos_scpi_demeaned <- funcs$placebos_scpi(
   dados = dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinalInv)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Investimento_Publico_PIB_PPP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinalInv,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi_demeaned <- funcs$mspe_pre_trat_scpi(cs_InvPub_Nivel_scpi_demeaned))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_demeaned <- funcs$razao_mspe_scpi(df_placebos_scpi_demeaned)
mspe_pos_scpi_demeaned <- mean(df_razao_mspe_scpi_demeaned$post_mspe[
   df_razao_mspe_scpi_demeaned$type == "donor"])

# Gráficos
lista_graficos_scpi_demeaned <- funcs$graficos_sc_scpi(
   objeto_sc_scpi = cs_InvPub_Nivel_scpi_demeaned,
   df_placebos = df_placebos_scpi_demeaned,
   df_razao = df_razao_mspe_scpi_demeaned,
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos
)


lista_graficos_scpi_demeaned$pesos
ggsave("Figuras/Investimento_Publico/Pesos_InvPub_demeaned.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$trends
ggsave("Figuras/Investimento_Publico/Trends_InvPub_demeaned.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$gap
ggsave("Figuras/Investimento_Publico/Gap_InvPub_demeaned.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$placebos
ggsave("Figuras/Investimento_Publico/Placebo_InvPub_demeaned.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$razao_mspe
ggsave("Figuras/Investimento_Publico/Razao_MSPE_InvPub_demeaned.pdf", dpi = 1200)
