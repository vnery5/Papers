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
getwd()
sDiretorio <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Baixando funções
funcs <- new.env()
source("1_Funcoes_CS_PT.R", local = funcs, encoding = 'UTF-8')

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
## Dados e variáveis -----------------------------------------------------------
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

# CS Escolhido: Inflexões ======================================================
# Controle Sintético
csInvPub <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
   ## Até 2011
   generate_predictor(time_window = nAnoInicial:(nAnoInflexao - 1),
                      `Taxa de Cambio 100 1` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 1` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 1` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 1` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 1` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 1` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   ## 2012 a 2016
   generate_predictor(time_window = nAnoInflexao:nAnoTratamento,
                      `Taxa de Cambio 100 2` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 2` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 2` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 2` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 2` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 2` = mean(Taxa_Desemprego, na.rm = T)
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
(mspe_pos <- funcs$mspe_placebos_pos_trat(csInvPub, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br <- csInvPub %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPub <- funcs$graficos_cs(
   objeto_cs_tidy = csInvPub, 
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
lista_graficos_InvPub$pesos
funcs$tabela_media_cs(csInvPub, stargazer = F)

## Trends e Gaps
lista_graficos_InvPub$trends
lista_graficos_InvPub$gap

## Inferência
lista_graficos_InvPub$placebos
lista_graficos_InvPub$razao_mspe
csInvPub %>% grab_signficance()

# Placebos Temporais ===========================================================
## 2010 ------------------------------------------------------------------------
# Controle Sintético
csInvPub_2010 <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2010,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias até 2010
   generate_predictor(time_window = nAnoInicial:2010,
                      `Taxa de Cambio 100 1` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 1` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 1` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 1` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 1` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 1` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   generate_predictor(time_window = nAnoInicial,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = 2010,
                      `Investimento Público 2010` = Investimento_Publico_PIB_PPP) %>%
   generate_weights(optimization_window = nAnoInicial:2010) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2010 <- funcs$mspe_placebos_pos_trat(csInvPub_2010, 2010))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2010 <- csInvPub_2010 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPub_2010 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csInvPub_2010, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
   ano_placebo_temporal = 2010,
   ano_crise = 2014,
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinalInv, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10,
   hjust_trends = .5, hjust_gap = .5
)

## Trends e Gaps
lista_graficos_InvPub_2010$trends
ggsave("Figuras/Investimento_Publico/Trends_InvPub_2010.pdf", dpi = 1200)

lista_graficos_InvPub_2010$gap
ggsave("Figuras/Investimento_Publico/Gap_InvPub_2010.pdf", dpi = 1200)


## 2014 ------------------------------------------------------------------------
# Controle Sintético
csInvPub_2014 <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2014,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
   ## Até 2011
   generate_predictor(time_window = nAnoInicial:(nAnoInflexao - 1),
                      `Taxa de Cambio 100 1` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 1` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 1` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 1` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 1` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 1` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   ## 2012 a 2014
   generate_predictor(time_window = nAnoInflexao:2014,
                      `Taxa de Cambio 100 2` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 2` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 2` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 2` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 2` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 2` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   generate_predictor(time_window = nAnoInicial,
                      `Investimento Público 2005` = Investimento_Publico_PIB_PPP) %>%
   generate_predictor(time_window = nAnoInflexao - 1,
                      `Investimento Público 2011` = Investimento_Publico_PIB_PPP) %>% 
   generate_predictor(time_window = 2014,
                      `Investimento Público 2014` = Investimento_Publico_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:2014) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2014 <- funcs$mspe_placebos_pos_trat(csInvPub_2014, 2014))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2014 <- csInvPub_2014 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPub_2014 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csInvPub_2014, 
   nome_variavel_y = "Investimento Público Real PPP (% do PIB)", 
   ano_placebo_temporal = 2014,
   ano_crise = 2014,
   ano_tratamento = nAnoTratamento,
   ano_inicial = nAnoInicial, 
   ano_final = nAnoFinalInv, 
   breaks_grafico = breaks_grafico,
   escala_trends = escala_trends,
   escala_gaps = escala_gaps,
   paleta_trend = paleta_trend, 
   paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos,
   paleta_placebos = paleta_placebos, 
   limite_mspe_placebos = 10,
   hjust_trends = .4, hjust_gap = .4
)

## Trends e Gaps
lista_graficos_InvPub_2014$trends
ggsave("Figuras/Investimento_Publico/Trends_InvPub_2014.pdf", dpi = 1200)

lista_graficos_InvPub_2014$gap
ggsave("Figuras/Investimento_Publico/Gap_InvPub_2014.pdf", dpi = 1200)

# Leave-One-Out ================================================================
# Pegando maior peso
isoMaiorPeso <- csInvPub %>% 
   grab_unit_weights() %>% 
   arrange(desc(weight)) %>% 
   slice(1) %>% 
   pull(1)

# Printando
print(paste0("País com o maior peso: ", isoMaiorPeso))

# Retirando do DataFrame
dfNivel_LOO <- dfNivel %>% 
   filter(iso2c != isoMaiorPeso)

# Reestimando o Controle Sintético
csInvPub_LOO <- dfNivel_LOO %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Publico_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = nAnoTratamento,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias
   ## Até 2011
   generate_predictor(time_window = nAnoInicial:(nAnoInflexao - 1),
                      `Taxa de Cambio 100 1` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 1` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 1` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 1` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 1` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 1` = mean(Taxa_Desemprego, na.rm = T)
   ) %>% 
   ## 2012 a 2016
   generate_predictor(time_window = nAnoInflexao:nAnoTratamento,
                      `Taxa de Cambio 100 2` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP 2` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca 2` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA 2` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção 2` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego 2` = mean(Taxa_Desemprego, na.rm = T)
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
(mspe_pos_LOO <- funcs$mspe_placebos_pos_trat(csInvPub_LOO, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_LOO <- csInvPub_LOO %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPub_LOO <- funcs$graficos_cs(
   objeto_cs_tidy = csInvPub_LOO, 
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
   limite_mspe_placebos = 10,
   hjust_trends = .95, hjust_gap = .9
)

## Pesos
lista_graficos_InvPub_LOO$pesos

## Trends e Gaps
lista_graficos_InvPub_LOO$trends
ggsave("Figuras/Investimento_Publico/Trends_InvPub_LOO.pdf", dpi = 1200)

lista_graficos_InvPub_LOO$gap
ggsave("Figuras/Investimento_Publico/Gap_InvPub_LOO.pdf", dpi = 1200)

## Significância
lista_graficos_InvPub_LOO$razao_mspe

