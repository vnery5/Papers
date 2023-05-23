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
   filter(iso2c %in% vDoadores_Juros) %>% 
   filter(Variável %in% c(cCovariadas, cY)) %>% 
   filter(Ano >= nAnoInicial) %>% 
   group_by(iso2c, Variável) %>% 
   arrange(Ano) %>% 
   mutate(PD = ifelse(Variável %in% cY, Valor - lag(Valor), Valor),
          Delta = ifelse(Variável %in% cY, Valor / lag(Valor), Valor),
          Valor100 = ifelse(Variável %in% cY, cumprod(c(100, Delta[-1])), Valor)) %>%
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

# CS Escolhido: Apenas Y =======================================================
# Controle Sintético
csJurosNom <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinal)) %>% 
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
(mspe_pos <- funcs$mspe_placebos_pos_trat(csJurosNom, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br <- csJurosNom %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_JurosNom <- funcs$graficos_cs(
   objeto_cs_tidy = csJurosNom, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual)", 
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
lista_graficos_JurosNom$pesos
funcs$tabela_media_cs(csJurosNom, stargazer = F)

## Trends e Gaps
lista_graficos_JurosNom$trends
lista_graficos_JurosNom$gap

## Inferência
lista_graficos_JurosNom$placebos
lista_graficos_JurosNom$razao_mspe
csJurosNom %>% grab_signficance()

# Placebos Temporais ===========================================================
## 2010 ------------------------------------------------------------------------
# Controle Sintético
csJurosNom_2010 <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinal)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2010,
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
   generate_weights(optimization_window = nAnoInicial:2010) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2010 <- funcs$mspe_placebos_pos_trat(csJurosNom_2010, 2010))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2010 <- csJurosNom_2010 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_JurosNom_2010 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csJurosNom_2010, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual)", 
   ano_placebo_temporal = 2010,
   ano_crise = 2014,
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
   hjust_gap = .2
)

## Trends e Gaps
lista_graficos_JurosNom_2010$trends
ggsave("Figuras/Taxa_Juros/Trends_JurosNom_2010.pdf", dpi = 1200)

lista_graficos_JurosNom_2010$gap
ggsave("Figuras/Taxa_Juros/Gap_JurosNom_2010.pdf", dpi = 1200)


## 2014 ------------------------------------------------------------------------
# Controle Sintético
csJurosNom_2014 <- dfNorm %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinal)) %>% 
   synthetic_control(outcome = Taxa_Juros_Politica_CP,
                     unit = iso2c,
                     time = Ano,
                     i_unit = "BR",
                     i_time = 2014,
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
   generate_weights(optimization_window = nAnoInicial:2014) %>% 
   generate_control()

# Vendo MSPE dos placebos pós-tratamento
(mspe_pos_2014 <- funcs$mspe_placebos_pos_trat(csJurosNom_2014, 2014))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_2014 <- csJurosNom_2014 %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_JurosNom_2014 <- funcs$graficos_cs_temporal(
   objeto_cs_tidy = csJurosNom_2014, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual)", 
   ano_placebo_temporal = 2014,
   ano_crise = 2014,
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
   hjust_trends = .9, hjust_gap = .2
)

## Trends e Gaps
lista_graficos_JurosNom_2014$trends
ggsave("Figuras/Taxa_Juros/Trends_JurosNom_2014.pdf", dpi = 1200)

lista_graficos_JurosNom_2014$gap
ggsave("Figuras/Taxa_Juros/Gap_JurosNom_2014.pdf", dpi = 1200)

# Leave-One-Out ================================================================
# Pegando maior peso
isoMaiorPeso <- csJurosNom %>% 
   grab_unit_weights() %>% 
   arrange(desc(weight)) %>% 
   slice(1) %>% 
   pull(1)

# Printando
print(paste0("País com o maior peso: ", isoMaiorPeso))

# Retirando do DataFrame
dfNorm_LOO <- dfNorm %>% 
   filter(iso2c != isoMaiorPeso)

# Reestimando o Controle Sintético
csJurosNom_LOO <- dfNorm_LOO %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinal)) %>% 
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
(mspe_pos_LOO <- funcs$mspe_placebos_pos_trat(csJurosNom_LOO, nAnoTratamento))

# MSPE do Brasil pré-tratamento
(mspe_pre_br_LOO <- csJurosNom_LOO %>% 
      grab_signficance() %>% 
      filter(unit_name == "BR") %>% 
      pull(pre_mspe))

# Gráficos
lista_graficos_JurosNom_LOO <- funcs$graficos_cs(
   objeto_cs_tidy = csJurosNom_LOO, 
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual)", 
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
   hjust_trends = .1, hjust_gap = .1
)

## Pesos
lista_graficos_JurosNom_LOO$pesos

## Trends e Gaps
lista_graficos_JurosNom_LOO$trends
ggsave("Figuras/Taxa_Juros/Trends_JurosNom_LOO.pdf", dpi = 1200)

lista_graficos_JurosNom_LOO$gap
ggsave("Figuras/Taxa_Juros/Gap_JurosNom_LOO.pdf", dpi = 1200)
