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

# Nível
dfNivel <- df %>% 
   select(-PD, -Valor100) %>% 
   pivot_wider(names_from = Variável, values_from = Valor) %>% 
   filter(Ano %in% c(nAnoInicial:nAnoFinal))

# CS Escolhido: Apenas Y (demeaned) ============================================
# Preparando dados para o CS
dfNivel_Juro_Nom_scpi <- scdata(
   dfNivel, 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (nAnoInicial:nAnoTratamento),
   period.post = ((nAnoTratamento + 1):nAnoFinal), 
   unit.tr = "BR", unit.co = vDoadores_Juros[-1],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi <- scest(dfNivel_Juro_Nom_scpi, 
                          w.constr = list(name = "simplex", Q = 1))

# Placebos
df_placebos_scpi <- funcs$placebos_scpi(
   dados = dfNivel,
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi <- funcs$razao_mspe_scpi(df_placebos_scpi)

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

lista_graficos_scpi$trends
lista_graficos_scpi$gap

lista_graficos_scpi$placebos
lista_graficos_scpi$razao_mspe

# Placebos Temporais ===========================================================
## 2010 ------------------------------------------------------------------------
# Preparando dados para o CS
dfNivel_Juro_Nom_scpi_2010 <- scdata(
   dfNivel, 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (nAnoInicial:2010),
   period.post = ((2010 + 1):nAnoFinal), 
   unit.tr = "BR", unit.co = vDoadores_Juros[-1],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi_2010 <- scest(dfNivel_Juro_Nom_scpi_2010, 
                               w.constr = list(name = "simplex", Q = 1))

# Placebos
df_placebos_scpi_2010 <- funcs$placebos_scpi(
   dados = dfNivel,
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = 2010, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi_2010 <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi_2010))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_2010 <- funcs$razao_mspe_scpi(df_placebos_scpi_2010)

# Gráficos
lista_graficos_scpi_2010 <- funcs$graficos_sc_scpi_temporal(
   objeto_sc_scpi = cs_Juro_Nom_scpi_2010,
   df_placebos = df_placebos_scpi_2010,
   df_razao = df_razao_mspe_scpi_2010,
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_placebo = 2010,
   ano_inicial = nAnoInicial, ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos,
   hjust_trends = .05, hjust_gap = .1
)

lista_graficos_scpi_2010$pesos

lista_graficos_scpi_2010$trends
ggsave("Figuras/Taxa_Juros/Trends_Nivel_JurosNom_2010.pdf", dpi = 1200)

lista_graficos_scpi_2010$gap
ggsave("Figuras/Taxa_Juros/Gap_Nivel_JurosNom_2010.pdf", dpi = 1200)


## 2014 ------------------------------------------------------------------------
# Preparando dados para o CS
dfNivel_Juro_Nom_scpi_2014 <- scdata(
   dfNivel, 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (nAnoInicial:2014),
   period.post = ((2014 + 1):nAnoFinal), 
   unit.tr = "BR", unit.co = vDoadores_Juros[-1],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi_2014 <- scest(dfNivel_Juro_Nom_scpi_2014, 
                               w.constr = list(name = "simplex", Q = 1))

# Placebos
df_placebos_scpi_2014 <- funcs$placebos_scpi(
   dados = dfNivel,
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = 2014, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi_2014 <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi_2014))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_2014 <- funcs$razao_mspe_scpi(df_placebos_scpi_2014)

# Gráficos
lista_graficos_scpi_2014 <- funcs$graficos_sc_scpi_temporal(
   objeto_sc_scpi = cs_Juro_Nom_scpi_2014,
   df_placebos = df_placebos_scpi_2014,
   df_razao = df_razao_mspe_scpi_2014,
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_placebo = 2014,
   ano_inicial = nAnoInicial, ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos,
   hjust_trends = .05, hjust_gap = .1
)

lista_graficos_scpi_2014$pesos

lista_graficos_scpi_2014$trends
ggsave("Figuras/Taxa_Juros/Trends_Nivel_JurosNom_2014.pdf", dpi = 1200)

lista_graficos_scpi_2014$gap
ggsave("Figuras/Taxa_Juros/Gap_Nivel_JurosNom_2014.pdf", dpi = 1200)


# Leave-One-Out ================================================================
# Pegando maior peso
## Dados
dfPesos <- as.data.frame(cs_Juro_Nom_scpi$est.results$w)
dfPesos$name <- substring(rownames(dfPesos), 4)
dfPesos$weight <- dfPesos[, 1]

## Maior
isoMaiorPeso <- dfPesos %>% 
   arrange(desc(weight)) %>% 
   slice(1) %>% 
   pull(2)

# Printando
print(paste0("País com o maior peso: ", isoMaiorPeso))

# Retirando do DataFrame
dfNivel_LOO <- dfNivel %>% 
   filter(iso2c != isoMaiorPeso)

# Reestimando o CS
## Preparando dados para o CS
dfNivel_Juro_Nom_scpi_LOO <- scdata(
   dfNivel_LOO, 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (nAnoInicial:nAnoTratamento),
   period.post = ((nAnoTratamento + 1):nAnoFinal), 
   unit.tr = "BR", unit.co = unique(dfNivel_LOO$iso2c)[-1],
   constant = TRUE, cointegrated.data = TRUE
)

## Estimando
cs_Juro_Nom_scpi_LOO <- scest(dfNivel_Juro_Nom_scpi_LOO, 
                              w.constr = list(name = "simplex", Q = 1))

# Placebos
df_placebos_scpi_LOO <- funcs$placebos_scpi(
   dados = dfNivel_LOO,
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi_LOO <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi_LOO))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_LOO <- funcs$razao_mspe_scpi(df_placebos_scpi_LOO)

# Gráficos
lista_graficos_scpi_LOO <- funcs$graficos_sc_scpi(
   objeto_sc_scpi = cs_Juro_Nom_scpi_LOO,
   df_placebos = df_placebos_scpi_LOO,
   df_razao = df_razao_mspe_scpi_LOO,
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos,
   hjust_trends = .05, hjust_gap = .1
)

lista_graficos_scpi_LOO$pesos

lista_graficos_scpi_LOO$trends
ggsave("Figuras/Taxa_Juros/Trends_Nivel_JurosNom_LOO.pdf", dpi = 1200)

lista_graficos_scpi_LOO$gap
ggsave("Figuras/Taxa_Juros/Gap_Nivel_JurosNom_LOO.pdf", dpi = 1200)
