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
   filter((iso2c %in% vDoadores) & (Variável %in% c(cCovariadas, cY))) %>% 
   filter(Ano >= nAnoInicial) %>% 
   as.data.frame() %>% 
   mutate(Tratado = ifelse((iso2c == "BR") & (Ano > nAnoTratamento), 1, 0))

# Nível
dfNivel <- df %>% 
   pivot_wider(names_from = Variável, values_from = Valor)


# CS Escolhido: Média de Todo Período + Inflexões ==============================
# Controle Sintético
csInvPriv <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Privado_PIB_PPP,
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
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)) %>% 
   # Valores pré-tratamento
   generate_predictor(time_window = 2014,
                      `Investimento Privado 2014` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2015,
                      `Investimento Privado 2015` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2016,
                      `Investimento Privado 2016` = Investimento_Privado_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos <- funcs$mspe_placebos_pos_trat(csInvPriv, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br <- csInvPriv %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPriv <- funcs$graficos_cs(
   objeto_cs_tidy = csInvPriv, 
   nome_variavel_y = "Investimento Privado Real PPP (% do PIB)", 
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
lista_graficos_InvPriv$pesos

## Trends e Gaps
lista_graficos_InvPriv$trends
lista_graficos_InvPriv$gap

## Inferência
lista_graficos_InvPriv$placebos
lista_graficos_InvPriv$razao_mspe
csInvPriv %>% grab_signficance()


# Placebos Temporais ===========================================================
## 2010 ------------------------------------------------------------------------
# Controle Sintético
csInvPriv_2010 <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Privado_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2010,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias até 2010
   generate_predictor(time_window = nAnoInicial:2010,
                      `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)) %>% 
   # Últimos dois valores pré-tratamento
   generate_predictor(time_window = 2009,
                      `Investimento Privado 2009` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2010,
                      `Investimento Privado 2010` = Investimento_Privado_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:2010) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2010 <- funcs$mspe_placebos_pos_trat(csInvPriv_2010, 2010))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2010 <- csInvPriv_2010 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPriv_2010 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csInvPriv_2010, 
   nome_variavel_y = "Investimento Privado Real PPP (% do PIB)", 
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
   hjust_gap = 0.925, hjust_trends = 0.925
)

## Trends e Gaps
lista_graficos_InvPriv_2010$trends
ggsave("Figuras/Investimento_Privado/Trends_InvPriv_2010.pdf", dpi = 1200)

lista_graficos_InvPriv_2010$gap
ggsave("Figuras/Investimento_Privado/Gap_InvPriv_2010.pdf", dpi = 1200)

## Significância
lista_graficos_InvPriv_2010$razao_mspe

## Pesos
lista_graficos_InvPriv_2010$pesos
csInvPriv_2010 %>% 
   grab_unit_weights() %>% 
   arrange(desc(weight))

## 2014 ------------------------------------------------------------------------
# Controle Sintético
csInvPriv_2014 <- dfNivel %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Privado_PIB_PPP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2014,
                     generate_placebos = TRUE) %>% 
   # Gerando preditores: médias até 2014
   generate_predictor(time_window = nAnoInicial:2014,
                      `Taxa de Cambio 100` = mean(Taxa_Cambio100, na.rm = T),
                      `PIB per Capita PPP` = mean(PIB_Capita_PPP17, na.rm = T),
                      `Termos de Troca` = mean(Termos_Troca, na.rm = T),
                      `Resultado Primário CA` = mean(Resultado_Primario_CA_PIB_Pot, na.rm = T),
                      `Controle Corrupção` = mean(Controle_Corrupcao, na.rm = T),
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)) %>% 
   # Valores de Y
   generate_predictor(time_window = 2012,
                      `Investimento Privado 2012` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2013,
                      `Investimento Privado 2013` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2014,
                      `Investimento Privado 2014` = Investimento_Privado_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:2014) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2014 <- funcs$mspe_placebos_pos_trat(csInvPriv_2014, 2014))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2014 <- csInvPriv_2014 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPriv_2014 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csInvPriv_2014, 
   nome_variavel_y = "Investimento Privado Real PPP (% do PIB)", 
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
   hjust_gap = 0.925, hjust_trends = 0.15
)

## Trends e Gaps
lista_graficos_InvPriv_2014$trends
ggsave("Figuras/Investimento_Privado/Trends_InvPriv_2014.pdf", dpi = 1200)

lista_graficos_InvPriv_2014$gap
ggsave("Figuras/Investimento_Privado/Gap_InvPriv_2014.pdf", dpi = 1200)

lista_graficos_InvPriv_2014$razao_mspe
csInvPriv_2014 %>% grab_signficance()


# Leave-One-Out ================================================================
# Pegando maior peso
isoMaiorPeso <- csInvPriv %>% 
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
csInvPriv_LOO <- dfNivel_LOO %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinalInv)) %>% 
   synthetic_control(outcome = Investimento_Privado_PIB_PPP,
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
                      `Taxa Desemprego` = mean(Taxa_Desemprego, na.rm = T)) %>% 
   # Valores pré-tratamento
   generate_predictor(time_window = 2014,
                      `Investimento Privado 2014` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2015,
                      `Investimento Privado 2015` = Investimento_Privado_PIB_PPP) %>% 
   generate_predictor(time_window = 2016,
                      `Investimento Privado 2016` = Investimento_Privado_PIB_PPP) %>% 
   generate_weights(optimization_window = nAnoInicial:nAnoTratamento) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_LOO <- funcs$mspe_placebos_pos_trat(csInvPriv_LOO, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_LOO <- csInvPriv_LOO %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_InvPriv_LOO <- funcs$graficos_cs(
   objeto_cs_tidy = csInvPriv_LOO, 
   nome_variavel_y = "Investimento Privado Real PPP (% do PIB)", 
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
   hjust_gap = 0.925, hjust_trends = 0.925
)

## Pesos
lista_graficos_InvPriv_LOO$pesos

## Trends e Gaps
lista_graficos_InvPriv_LOO$trends
ggsave("Figuras/Investimento_Privado/Trends_InvPriv_LOO.pdf", dpi = 1200)

lista_graficos_InvPriv_LOO$gap
ggsave("Figuras/Investimento_Privado/Gap_InvPriv_LOO.pdf", dpi = 1200)

## Significância
lista_graficos_InvPriv_LOO$razao_mspe
csInvPriv_LOO %>% grab_signficance()
