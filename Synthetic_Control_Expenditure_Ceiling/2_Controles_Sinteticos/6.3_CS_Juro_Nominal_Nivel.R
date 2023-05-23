# Setup e Bibliotecas ==========================================================
# Dados
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(skimr)
library(glue)

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


# Controles Sintéticos =========================================================
## scpi ------------------------------------------------------------------------
### Demeaned - Ferman & Pinto (2021) ###########################################
# Preparando dados
dfNivel_Juro_Nom_scpi_demeaned <- scdata(
   dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (2005:2016),
   period.post = (2017:2021), 
   unit.tr = "BR", unit.co = vDoadores_Juros[-1],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi_demeaned <- scest(dfNivel_Juro_Nom_scpi_demeaned, 
                                   w.constr = list(name = "simplex", Q = 1))

# Vendo resultados
summary(cs_Juro_Nom_scpi_demeaned)
scplot(cs_Juro_Nom_scpi_demeaned)

# Placebos
df_placebos_scpi_demeaned <- funcs$placebos_scpi(
   dados = dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "BR",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

## MSPE do Brasil pré-tratamento
(mspe_pre_br_scpi_demeaned <- funcs$mspe_pre_trat_scpi(cs_Juro_Nom_scpi_demeaned))

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_demeaned <- funcs$razao_mspe_scpi(df_placebos_scpi_demeaned)
mspe_pos_scpi_demeaned <- mean(df_razao_mspe_scpi_demeaned$post_mspe[
   df_razao_mspe_scpi_demeaned$type == "donor"])

# Gráficos
lista_graficos_scpi_demeaned <- funcs$graficos_sc_scpi(
   objeto_sc_scpi = cs_Juro_Nom_scpi_demeaned,
   df_placebos = df_placebos_scpi_demeaned,
   df_razao = df_razao_mspe_scpi_demeaned,
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend, paleta_gap = paleta_gap, 
   paleta_pesos = paleta_pesos, paleta_placebos = paleta_placebos,
   hjust_trends = .2, hjust_gap = .9
)

lista_graficos_scpi_demeaned$pesos
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Pesos_demeaned_2021.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$trends
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Trends_demeaned_2021.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$gap
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Gap_demeaned_2021.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$placebos
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Placebos_demeaned_2021.pdf", dpi = 1200)

lista_graficos_scpi_demeaned$razao_mspe
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Razao_MSPE_demeaned_2021.pdf", dpi = 1200)


#### México ####
# Preparando dados
dfNivel_Juro_Nom_scpi_demeaned_MX <- scdata(
   dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (2005:2016),
   period.post = (2017:2021), 
   unit.tr = "MX", unit.co = vDoadores_Juros[-6],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi_demeaned_MX <- scest(dfNivel_Juro_Nom_scpi_demeaned_MX, 
                                      w.constr = list(name = "simplex", Q = 1))


# Placebos
df_placebos_scpi_demeaned_MX <- funcs$placebos_scpi(
   dados = dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "MX",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_demeaned_MX <- funcs$razao_mspe_scpi(df_placebos_scpi_demeaned_MX)

# Gráficos
### Customizando paletas
paleta_trend_MX <- met.brewer("Homer2")[c(1, 3)]
paleta_gap_MX <- met.brewer("Hokusai1")[c(2, 3)]

## Função
lista_graficos_scpi_demeaned_MX <- funcs$graficos_sc_scpi_outros_paises(
   objeto_sc_scpi = cs_Juro_Nom_scpi_demeaned_MX,
   df_placebos = df_placebos_scpi_demeaned_MX,
   nome_pais = "México",
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend_MX, 
   paleta_gap = paleta_gap_MX, 
   paleta_pesos = paleta_pesos
)

lista_graficos_scpi_demeaned_MX$pesos
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Pesos_demeaned_MX.pdf", dpi = 1200)

lista_graficos_scpi_demeaned_MX$trends
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Trends_demeaned_MX.pdf", dpi = 1200)

lista_graficos_scpi_demeaned_MX$gap
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Gap_demeaned_MX.pdf", dpi = 1200)

#### Malásia ####
# Preparando dados
dfNivel_Juro_Nom_scpi_demeaned_MY <- scdata(
   dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)), 
   id.var = "iso2c", time.var = "Ano",
   outcome.var = "Taxa_Juros_Politica_CP", 
   period.pre = (2005:2016),
   period.post = (2017:2021), 
   unit.tr = "MY", unit.co = vDoadores_Juros[-7],
   constant = TRUE, cointegrated.data = TRUE
)

# Estimando
cs_Juro_Nom_scpi_demeaned_MY <- scest(dfNivel_Juro_Nom_scpi_demeaned_MY, 
                                      w.constr = list(name = "simplex", Q = 1))


# Placebos
df_placebos_scpi_demeaned_MY <- funcs$placebos_scpi(
   dados = dfNivel %>% filter(Ano %in% c(nAnoInicial:nAnoFinal)),
   string_coluna_unidade = "iso2c",
   string_coluna_tempo = "Ano",
   string_variavel_interesse = "Taxa_Juros_Politica_CP",
   ano_tratamento = nAnoTratamento, unidade_tratada = "MY",
   ano_inicial = nAnoInicial, ano_final = nAnoFinal,
   restricao_w = list(name = "simplex", Q = 1),
   constant = TRUE, cointegrated = TRUE
)

# Razão MSPE Pós-Tratamento
df_razao_mspe_scpi_demeaned_MY <- funcs$razao_mspe_scpi(df_placebos_scpi_demeaned_MY)

# Gráficos
### Customizando paletas
paleta_trend_MY <- met.brewer("Ingres")[c(4, 5)]
paleta_gap_MY <- met.brewer("Homer1")[c(4, 5)]

## Função
lista_graficos_scpi_demeaned_MY <- funcs$graficos_sc_scpi_outros_paises(
   objeto_sc_scpi = cs_Juro_Nom_scpi_demeaned_MY,
   df_placebos = df_placebos_scpi_demeaned_MY,
   nome_pais = "Malásia",
   nome_variavel_y = "Taxa de Juros Nominal (Média Anual, %)",
   ano_tratamento = nAnoTratamento, ano_inicial = nAnoInicial,
   ano_final = nAnoFinal, breaks_grafico = 2,
   paleta_trend = paleta_trend_MY, 
   paleta_gap = paleta_gap_MY, 
   paleta_pesos = paleta_pesos
)

lista_graficos_scpi_demeaned_MY$pesos
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Pesos_demeaned_MY.pdf", dpi = 1200)

lista_graficos_scpi_demeaned_MY$trends
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Trends_demeaned_MY.pdf", dpi = 1200)

lista_graficos_scpi_demeaned_MY$gap
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Gap_demeaned_MY.pdf", dpi = 1200)

#### Placebos com MX e MY ####
## Calculando MSPEs pré-tratamento
df_placebos_mx_my <- df_placebos_scpi_demeaned %>% 
   pivot_longer(cols = c(real_y, synth_y, gap)) %>% 
   group_by(unit) %>% 
   mutate(mspe = sum(sq_pred_error * pre_trat) / sum(pre_trat)) %>% 
   ungroup() %>% 
   mutate(mspe_br = mean(ifelse(unit == "BR", mspe, NA), na.rm = T))

grafPlacebos_mx_my <- df_placebos_mx_my %>% 
   filter((name == "gap") & (mspe < 10 * mspe_br)) %>% 
   ggplot(aes(x = time_unit, y = value, group = unit)) +
   geom_line(size = .75, colour = paleta_placebos[1]) + 
   geom_line(
      data = df_placebos_mx_my %>% filter((name == "gap") & (unit == "BR")),
      size = 1.5, colour = paleta_placebos[2]
   ) + 
   geom_line(
      data = df_placebos_mx_my %>% filter((name == "gap") & (unit == "MX")),
      size = 1.5, colour = met.brewer("Homer2")[1]
   ) + 
   geom_line(
      data = df_placebos_mx_my %>% filter((name == "gap") & (unit == "MY")),
      size = 1.5, colour = met.brewer("Ingres")[4]
   ) + 
   geom_hline(yintercept = 0, linetype = 'dotted', 
              size = .5, colour = 'black', show.legend = F) + 
   geom_textvline(xintercept = nAnoTratamento, linetype = 'dotted', 
                  colour = 'black', size = 3,
                  label = "Aprovação do Teto", hjust = .88, vjust = .55) +  
   labs(x = 'Ano', y = "Real - Sintético") +
   scale_x_continuous(breaks = seq(nAnoInicial, nAnoFinal, by = breaks_grafico)) +
   scale_y_continuous(breaks = seq(-5, 9, by = 2)) +
   theme_bw() 
grafPlacebos_mx_my
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Placebos_demeaned_MX_MY_BR.pdf", dpi = 1200)

# Razão MSPE
## Dados
df_razao_mspe_scpi_demeaned_mx_my <- df_razao_mspe_scpi_demeaned
df_razao_mspe_scpi_demeaned_mx_my$type_mx_my = c("MX", "MY", "BR", rep("donor", 9))

## Valor do México
mspe_mx <- round(df_razao_mspe_scpi_demeaned_mx_my[1, 4], 0)

grafRazaoMSPE_mx_my <- df_razao_mspe_scpi_demeaned_mx_my %>% 
   ggplot(aes(x = reorder(unit, mspe_ratio), y = mspe_ratio, fill = type_mx_my)) +
   geom_bar(stat = "identity", colour = "black") + 
   # annotate(geom = 'text', x = 12, y = 9.5, label = glue("Razão MSPE: {mspe_mx}"), size = 3) + 
   scale_fill_manual(name = "", 
                     values = c(paleta_placebos[c(2, 1)],
                                met.brewer("Homer2")[1],
                                met.brewer("Ingres")[4]),
                     guide = "none") +
   labs(x = 'Razão MSPE', y = '') + 
   theme_bw() +
   coord_flip()
   # coord_flip(ylim = c(0, 10))
grafRazaoMSPE_mx_my
ggsave("Figuras/Taxa_Juros/Nom_Nivel_Razao_MSPE_demeaned_MX_MY_BR.pdf", dpi = 1200)
