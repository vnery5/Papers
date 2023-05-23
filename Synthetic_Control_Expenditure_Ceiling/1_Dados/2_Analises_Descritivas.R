# Setup e bibliotecas ----
## Dados
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(openxlsx)

## Gráficos
library(wesanderson)  # https://github.com/karthik/wesanderson
library(MetBrewer)  # https://github.com/BlakeRMills/MetBrewer/tree/main

# Comandos Úteis
rm(list = ls())
cat("\014")

# Settando o diretório para o do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Outros Diretórios
sDiretorioFiguras <- paste(sDiretorio_Base, "Figuras", sep = "/")
sDiretorioIBRE <- paste(sDiretorio_Base, "IBRE", sep = "/")
sDiretorioDados <- paste(sDiretorio_Base, "Tesouro_OutrasFontes", sep = "/")
sDiretorioFMI <- paste(sDiretorio_Base, "FMI", sep = "/")
sDiretorioFinal <- paste(sDiretorio_Base, "Base_Final", sep = "/")

# Definindo paletas
Palette <- wes_palette("Darjeeling1")
Palette2 <- wes_palette("BottleRocket2")


## Tesouro =====================================================================
# Lendo
dfTesouro <- readxl::read_excel(paste(sDiretorioDados, 
                                       "Despesas_GovCentral.xlsx", 
                                       sep = "/"), sheet = "Saida")

# Formato Longo
dfTesouro %<>%
    pivot_longer(cols = !c(Ano),
                 names_to = "Indicador", values_to = "Valor")

### Gráficos ----
# Engessamento
dfTesouro %>% 
    filter((Indicador == "Grau_Engessamento_MM3") & 
               (Ano > 2002) & (Ano <= 2021)) %>% 
    ggplot(aes(x = Ano, y = Valor)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = 'Ano', y = 'Despesas Obrigatórias (MM3A - % do Total)') +
    scale_x_continuous(breaks = seq(2003, 2021, 2)) +
    scale_colour_hue(labels = "") +
    ylim(70, 93) + 
    theme_bw()
ggsave(paste(sDiretorioFiguras, "Grau_Engessamento.pdf", sep = "/"), dpi = 300)

# Receita e Despesa do Governo Central
dfTesouro %>% 
    filter((Indicador %in% c("ReceitaLiq_GovCentral_PIB", 
                             "Despesa_GovCentral_PIB")) & 
               (Ano > 2002) & (Ano <= 2019)) %>% 
    ggplot(aes(x = Ano, y = Valor, colour = Indicador)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2014, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) +
    annotate("text", x = 2015.8, y = 21, label = "Déficits Primários") +
    scale_colour_manual(name = "", values = Palette, 
                        labels = c("Despesa Primária", "Receita Líquida")) +
    labs(x = 'Ano', y = 'Receita e Despesa do Gov. Central (% do PIB)') +
    scale_x_continuous(breaks = seq(2003, 2021, 2)) +
    ylim(15, 21) + 
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, 
             "ReceitaLiq_Despesa_GovCentral_PIB_19.pdf", sep = "/"), dpi = 300)


## Regras FMI ==================================================================
# Lendo
dfRegras <- readxl::read_excel(paste(sDiretorioFMI, 
                                     "IMF Fiscal Rules Dataset.xlsx", 
                                     sep = "/"), sheet = "Saida")

# Formato Longo
dfRegras %<>%
    pivot_longer(cols = !c(Ano, Pais, Categoria),
                 names_to = "Indicador", values_to = "Valor")

# Contando
dfRegrasContagem <- dfRegras %>% 
    group_by(Ano, Categoria, Indicador) %>% 
    summarise(Contagem = sum(Valor))

### Gráficos ----
# Número de Regras no Mundo por Renda
dfRegrasContagem %>% 
    filter((Indicador == "Numero_Regras") & 
               (Ano > 1989) & (Ano < 2021)) %>% 
    ggplot(aes(x = Ano, y = Contagem, fill = Categoria)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(name = "", values = Palette, 
                     labels = c("Alta Renda", "Baixa Renda", "Emergente")) +
    labs(x = 'Ano', y = 'Número de Regras Fiscais no Mundo') +
    scale_x_continuous(breaks = seq(1990, 2020, 5)) +
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "NumeroRegras_Renda.pdf", sep = "/"), dpi = 300)

# Número de Regras no Mundo por Tipo
dfRegrasContagem %>% 
    filter((Indicador %in% c("Regra_Despesa", "Regra_Divida", "Regra_Receita", "Regra_Resultado")) & 
               (Ano > 1989) & (Ano <= 2021)) %>% 
    ggplot(aes(x = Ano, y = Contagem, fill = Indicador)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(name = "", values = Palette, 
                        labels = c("Despesa", "Dívida", "Receita", "Resultado")) +
    labs(x = 'Ano', y = 'Número de Regras Fiscais no Mundo') +
    scale_x_continuous(breaks = seq(1990, 2021, 5)) +
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "NumeroRegras_Tipo.pdf", sep = "/"), dpi = 300)

# Número de Regras no Mundo por Tipo - 100%
dfRegrasContagem %>% 
    filter((Indicador %in% c("Regra_Despesa", "Regra_Divida", "Regra_Receita", "Regra_Resultado")) & 
               (Ano > 1993) & (Ano <= 2021)) %>% 
    ggplot(aes(x = Ano, y = Contagem, fill = Indicador)) + 
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "", values = Palette2, 
                      labels = c("Despesa", "Dívida", "Receita", "Resultado")) +
    labs(x = 'Ano', y = 'Composição das Regras Fiscais no Mundo') +
    scale_x_continuous(breaks = seq(1993, 2021, 3)) +
    scale_y_continuous(labels=function(x) format(paste0(100*x, '%'), big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) + 
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "NumeroRegras_Tipo100.pdf", sep = "/"), dpi = 300)


## Crescimento do PIB ==========================================================
# Lendo
dfPIB <- readxl::read_excel(paste(sDiretorioDados, 
                                  "PIB_Real_Trimestral_Sazonal.xlsx", 
                                  sep = "/"), sheet = "Saida")

# Formato Longo
dfPIB %<>%
    pivot_longer(cols = !c(Ano, Trimestre, Ano_Trimestre),
                 names_to = "Indicador", values_to = "Valor") %>% 
    filter((Ano > 2009) & (Ano <= 2021))

### Gráficos ----
# Crescimento do PIB
dfPIB %>% 
    filter(Indicador %in% c("MédiaPIB_2010_14", "MédiaPIB_2015_16", "MédiaPIB_2017_19")) %>% 
    ggplot(aes(x = Ano_Trimestre, y = Valor, colour = Indicador, group = Indicador)) +
    # Crescimento Acumulado em 4 Trimestres
    geom_line(data = dfPIB[dfPIB$Indicador == "Crescimento_Acumulado_PIB_4Trim",],
              size = 1, colour = 'light gray', show.legend = F) +
    # Médias
    geom_line(size = 1.25, linetype = 'dashed') + 
    
    # Linhas e anotações
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'red', show.legend = F) +
    geom_vline(xintercept = "2015 T1", linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) +
    geom_vline(xintercept = "2017 T1", linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) +
    geom_vline(xintercept = "2020 T2", linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) +
    
    annotate("text", x = "2016 T1", y = 7.3, label = "Recessão\n2015-16") +
    annotate("text", x = "2021 T1", y = 7.5, label = "Pandemia") +
    
    # Formatação
    scale_colour_manual(name = "", values = Palette, 
                        labels = c("Média 2010-14", "Média 2015-16", "Média 2017-19")) +
    labs(x = 'Ano', y = 'Crescimento do PIB (%, Acumulado em 4 Trimestres)') +
    scale_x_discrete(breaks = c("2010 T1", "2011 T1", "2012 T1", "2013 T1",
                                "2014 T1", "2015 T1", "2016 T1", "2017 T1",
                                "2018 T1", "2019 T1", "2020 T1", "2021 T1")) + 
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "CrescimentoPIB_201021.pdf", sep = "/"), dpi = 300)


## Primeiras Diferenças ========================================================
# Lendo a base
dfBase <- readxl::read_excel(paste(sDiretorioFinal, 
                                   "Base_Investimento_Deficit_Juros.xlsx", 
                                   sep = "/"), sheet = "Dataset_Long")

# Filtrando e selecionando apenas as colunas úteis
# Além disso, calculando as primeiras diferenças
dfBaseBR <- dfBase %>% 
    filter(Variável %in% c("Investimento_Privado_PIB_PPP", "Investimento_Publico_PIB_PPP",
                           "Investimento_PIB_PPP", "Investimento_PIB",
                           "Resultado_Primario_CA_PIB_Pot", "Resultado_Primario_PIB", 
                           "Taxa_Juros_Politica_CP", "Taxa_Juros_Real_Politica_CP")) %>% 
    filter(iso2c == "BR") %>% 
    select(Ano, Variável, Valor) %>% 
    # Primeiras Diferenças
    group_by(Variável) %>% 
    mutate(Diff = Valor - lag(Valor))

### Juros ----
dfBaseBR %>% 
    filter((Variável %in% c("Taxa_Juros_Politica_CP",
                            "Taxa_Juros_Real_Politica_CP")) & (Ano >= 2010))  %>% 
    ggplot(aes(x = Ano, y = Diff, colour = Variável)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'red', show.legend = F) +
    labs(x = 'Ano', y = 'Primeira Diferença das Taxas de Juros') +
    scale_colour_manual(name = "", values = Palette, labels = c("Nominal", "Real")) +
    scale_x_continuous(breaks = seq(2010, 2021, 1)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(-4.5, 2.5, by = 1), limits = c(-4.5, 2.65)) +
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "Juros_BR_PD.pdf", sep = "/"), dpi = 300)

### Déficit - Esforço Fiscal ----
dfBaseBR %>% 
    filter((Variável %in% c("Resultado_Primario_CA_PIB_Pot",
                            "Resultado_Primario_PIB")) & (Ano >= 2010))  %>% 
    ggplot(aes(x = Ano, y = Diff, colour = Variável)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'red', show.legend = F) +
    labs(x = 'Ano', y = 'Esforço Fiscal Primário') +
    scale_colour_manual(name = "", values = Palette[c(2, 3)], 
                        labels = c("Ciclicamente Ajustado - % do PIB Potencial", 
                                   "Corrente - % do PIB")) +
    scale_x_continuous(breaks = seq(2010, 2021, 1)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(-8, 10, by = 2), limits = c(-8.5, 10)) +
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "Esforço_Fiscal_BR.pdf", sep = "/"), dpi = 300)

### Taxas de Investimento ----
dfBaseBR %>% 
    filter((Variável %in% c("Investimento_Privado_PIB_PPP", 
                            "Investimento_Publico_PIB_PPP",
                            "Investimento_PIB_PPP")) & (Ano >= 2010) & (Ano <= 2019))  %>% 
    ggplot(aes(x = Ano, y = Diff, colour = Variável)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'red', show.legend = F) +
    labs(x = 'Ano', y = 'Primeiras Diferença das Taxas de Investimento') +
    scale_colour_manual(name = "", values = Palette,
                        labels = c("Total Real", "Privado Real", "Público Real")) +
    scale_x_continuous(breaks = seq(2010, 2021, 1)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(-2.5, 2, by = .5), limits = c(-2.5, 2)) +
    theme_bw() + 
    theme(
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0),
        legend.text = element_text(size = 12)
    )
ggsave(paste(sDiretorioFiguras, "Investimento_BR_PD.pdf", sep = "/"), dpi = 300)


## STN =========================================================================
# Lendo dados sobre investimento público
dfSTN_Investimento <- readxl::read_excel(paste(sDiretorioDados, 
                                               "Dados_STN_GovernoGeral.xlsx", 
                                               sep = "/"), 
                                         sheet = "Resumo caixa anual",
                                         range = "A254:M259"
)

# Pivottando
dfSTN_Investimento %<>% 
   pivot_longer(cols = !c(Ente), names_to = "Ano", values_to = "Investimento")

# Filtrando
dfSTN_Investimento %<>% 
   filter(Ente != "Investimento bruto (aquisição líquida de ativos não financeiros)") %>%
   filter(Ente != "FGTS e PIS-PASEP")

# Somando
dfSTN_Investimento %<>% 
   group_by(Ano) %>% 
   mutate(Investimento_Total = sum(Investimento)) %>% 
   ungroup()

# Plottando
dfSTN_Investimento %>% 
   ggplot(aes(x = Ano, y = Investimento, fill = Ente, group = Ente)) +
   geom_bar(width = .5, stat = "identity") + 
   geom_text(aes(label = round(Investimento, 2), fontface = "bold"),
             position = position_stack(vjust = 0.5)) + 
   
   # Total
   geom_line(aes(x = as.numeric(Ano) - 2009, y = Investimento_Total, colour = 'a'),
             size = 1) + 
   geom_point(aes(x = as.numeric(Ano) - 2009, y = Investimento_Total),
              size = 2, colour = 'black', show.legend = F) +
   
   # Formatações
   geom_vline(xintercept = 7.5, linetype = 'longdash', 
              size = .3, colour = 'black', show.legend = F) + 
   labs(x = 'Ano', y = 'Aquisição Líquida de Ativos Não-Financeiros (% do PIB)') +
   scale_fill_manual(
      name = "",
      values = Palette[c(-1, -3)],
      labels = c("Federal", "Estadual", "Municipal")
   ) +
   scale_colour_manual(name = "", values = c("black"), labels = c("Total")) +
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   theme_bw() + 
   theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      plot.caption = element_text(hjust = 0),
      legend.text = element_text(size = 12)
   )
ggsave(paste(sDiretorioFiguras, "Investimento_BR_Entes.pdf", sep = "/"), dpi = 1200)

## Observatório InvPub =========================================================
### Taxas ####
# Lendo
# Lendo dados sobre investimento público
dfIBRE_Investimento <- readxl::read_excel(paste(sDiretorioIBRE, 
                                                "investimentos_publicos_1947_2021.xlsx", 
                                                sep = "/"), 
                                          sheet = "Em % do PIB",
                                          range = "A1:BX8"
)

# Pivottando
dfIBRE_Investimento %<>% 
   rename(Ente = Ano) %>% 
   pivot_longer(cols = !c(Ente), names_to = "Ano", values_to = "Investimento")

# Filtrando e adicionando grupo de estatais
dfIBRE_Investimento %<>% 
   filter(Ano >= 2010) %>% 
   filter(!Ente %in% c("FBCF", "GG", "SP")) %>% 
   mutate(Ente_Fed = ifelse(Ente == "EPU", 0 , 1))

# Somando investimento total
dfIBRE_Investimento %<>% 
   group_by(Ano) %>% 
   mutate(Investimento_Total_SP = sum(Investimento),
          Investimento_Total_Pub = sum(Investimento * Ente_Fed)) %>% 
   ungroup()

# Plottando
dfIBRE_Investimento %>% 
   ggplot(aes(x = Ano, y = Investimento, fill = Ente, group = Ente)) +
   # Por Ente
   geom_bar(width = .5, stat = "identity") + 
   geom_text(aes(label = round(Investimento, 2), fontface = "bold"),
             position = position_stack(vjust = 0.5)) + 
   
   # Sem Estatais
   geom_line(size = 1,
             aes(x = as.numeric(Ano) - 2009, y = Investimento_Total_Pub, 
                 colour = 'Total')) + 
   geom_point(aes(x = as.numeric(Ano) - 2009, y = Investimento_Total_Pub),
              size = 2, colour = 'black', show.legend = F) + 
   
   # Total Setor Público
   geom_line(size = 1,
             aes(x = as.numeric(Ano) - 2009, y = Investimento_Total_SP, 
                 colour = 'Total com Estatais')) + 
   geom_point(aes(x = as.numeric(Ano) - 2009, y = Investimento_Total_SP),
              size = 2, colour = 'darkgray', show.legend = F) + 
   
   # Formatação
   geom_vline(xintercept = 7.5, linetype = 'longdash', 
              size = .3, colour = 'black', show.legend = F) + 
   labs(x = 'Ano', y = 'Investimento do Setor Público Consolidado (% do PIB)') +
   scale_fill_manual(
      name = "",
      values = Palette[c(3, 2, 4, 5)],
      labels = c("Estatais", "Federal", "Estadual", "Municipal")
   ) +
   scale_colour_manual(
      name = "",
      values = c("black", "darkgray")
   ) +
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   theme_bw() + 
   theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      plot.caption = element_text(hjust = 0),
      legend.text = element_text(size = 12)
   )
# ggsave(paste(sDiretorioFiguras, "Investimento_BR_SetPub.pdf", sep = "/"), dpi = 1200)
ggsave(paste(sDiretorioFiguras, "Investimento_BR_SetPub_Totais.pdf", sep = "/"), dpi = 1200)


### Investimento Líquido ####
# Lendo
# Lendo dados sobre investimento público
dfIBRE_InvLiq <- readxl::read_excel(paste(sDiretorioIBRE, 
                                          "investimentos_publicos_1947_2021.xlsx", 
                                          sep = "/"), 
                                    sheet = "Em % do PIB",
                                    range = "A18:BX22"
)

# Pivottando
dfIBRE_InvLiq %<>% 
   rename(Ente = Ano) %>% 
   pivot_longer(cols = !c(Ente), names_to = "Ano", values_to = "Investimento")

# Filtrando
dfIBRE_InvLiq %<>% 
   filter(Ano >= 2010)

# Plottando
dfIBRE_InvLiq %>% 
   ggplot(aes(x = Ano, y = Investimento, 
              colour = reorder(Ente, Investimento), group = Ente)) +
   geom_line(size = 1) + 
   geom_point(size = 2) + 
   geom_vline(xintercept = 7.5, linetype = 'longdash', 
              size = .3, colour = 'black', show.legend = F) + 
   geom_hline(yintercept = 0, linetype = 'longdash', 
              size = .3, colour = 'red', show.legend = F) + 
   labs(x = 'Ano', y = 'Investimento Líquido de Depreciação (% do PIB)') +
   scale_colour_manual(
      name = "",
      values = c(Palette[c(-1, -3)], "black"),
      labels = c("Federal", "Estadual", "Municipal", "Total")
   ) +
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   theme_bw() + 
   theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      plot.caption = element_text(hjust = 0),
      legend.text = element_text(size = 12)
   )
ggsave(paste(sDiretorioFiguras, "Investimento_BR_Liquido.pdf", sep = "/"), dpi = 1200)


## IBGE PRelInv ================================================================
# Lendo dados 
dfIBGE <- readxl::read_excel(paste(sDiretorioDados, 
                                   "Deflator_PIB_FBKF.xlsx", 
                                   sep = "/"), 
                             sheet = "Analise",
                             range = "H1:BA28"
)

# Taxas de Investimento a preços de cada ano
dfIBGE_Taxas <- dfIBGE %>% 
   select(ano, taxa_inv_corrente, taxa_inv_icsd_ppp, taxa_inv_icsd_nom, 
          starts_with("taxa_inv_p")) %>% 
   pivot_longer(cols = !c(ano)) %>% 
   filter((ano >= 2001) & (ano < 2022))

# Preços Relativos
dfIBGE_PRel <- dfIBGE %>% 
   select(ano, starts_with("prel")) %>% 
   pivot_longer(cols = !c(ano)) %>% 
   filter((ano >= 2001) & (ano < 2022))

# Plottando preços relativos
dfIBGE_PRel %>% 
   filter(name != "prel_inv_2017_100_icsd") %>% 
   ggplot(aes(x = ano, y = value, colour = name, group = name)) +
   geom_line(size = 1) +
   geom_point(size = 2) + 
   geom_textvline(xintercept = 2016.5, linetype = 'dotted', 
                  colour = 'black', size = 3,
                  label = "Aprovação do Teto", hjust = .85, vjust = .5) +  
   geom_hline(yintercept = 100, linetype = 'longdash', 
              size = .3, colour = 'black', show.legend = F) + 
   geom_label(aes(label = round(value, 0)), fill = 'black', colour = 'white') + 
   labs(x = 'Ano', y = 'Preço Relativo do Investimento (2017 = 100)') +
   scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
   scale_y_continuous(breaks = seq(98, 118, by = 4)) +
   scale_colour_manual(name = "", values = c("black"), labels = c("Preço Relativo")) +
   theme_bw() + 
   theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      plot.caption = element_text(hjust = 0),
      legend.text = element_text(size = 12)
   )
ggsave(paste(sDiretorioFiguras, "PRel_Inv.pdf", sep = "/"), dpi = 1200)


# Taxas de Investimento a diferentes preços 
## Tabela auxiliar
dfIBGE_Taxas_MinMax <- dfIBGE %>% 
   select(ano, starts_with("taxa_inv_p")) %>% 
   pivot_longer(cols = !c(ano)) %>% 
   filter((ano > 2000) & (ano < 2022)) %>% 
   group_by(ano) %>% 
   mutate(min = min(value), max = max(value)) %>% 
   ungroup() %>% 
   select(ano, min, max) %>% 
   unique()

## Juntando
dfIBGE_Taxas %<>% 
   left_join(dfIBGE_Taxas_MinMax, by = "ano") %>% 
   filter(name %in% c("taxa_inv_corrente", "taxa_inv_icsd_ppp"))

# Plottando 
dfIBGE_Taxas %>% 
   ggplot(aes(x = ano, y = value, colour = name, fill = name, group = name)) +
   geom_ribbon(aes(ymin = min, ymax = max), fill= 'gray', colour = 'darkgray',
               linetype = 'dashed', show.legend = F, alpha = .3, size = .3) + 
   geom_line(size = 1) +
   geom_point(size = 2) + 
   geom_textvline(xintercept = 2016.5, linetype = 'dotted', 
                  colour = 'black', size = 3,
                  label = "Aprovação do Teto", hjust = .85, vjust = .5) +  
   labs(x = 'Ano', y = 'Taxa de Investimento Total (% do PIB)') +
   scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
   scale_colour_manual(name = "", 
                       values = Palette, 
                       labels = c("Corrente (IBGE)", "Real PPP (ICSD)")) +
   scale_fill_manual(name = "", 
                     values = Palette, 
                     labels = c("Corrente (IBGE)", "Real PPP (ICSD)")) +
   scale_y_continuous(labels=function(x) format(100*x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) + 
   theme_bw() + 
   theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      plot.caption = element_text(hjust = 0),
      legend.text = element_text(size = 12)
   )
ggsave(paste(sDiretorioFiguras, "Taxas_PRel.pdf", sep = "/"), dpi = 1200)
