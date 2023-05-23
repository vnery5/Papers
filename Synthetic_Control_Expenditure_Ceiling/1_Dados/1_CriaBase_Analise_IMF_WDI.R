# Setup e bibliotecas ----
# Comandos Úteis
rm(list = ls())
cat("\014")

# Salvar base para o Excel?
bSaveDados <- FALSE

# Pacotes
## Dados
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(openxlsx)

## Gráficos
library(wesanderson)  # https://github.com/karthik/wesanderson
library(MetBrewer)  # https://github.com/BlakeRMills/MetBrewer/tree/main

# Settando o diretório para o do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Outros Diretórios
sDiretorioOutput <- paste(sDiretorio_Base, "Base_Final", sep = "/")
sDiretorioFiguras <- paste(sDiretorio_Base, "Figuras", sep = "/")
sDiretorioFMI <- paste(sDiretorio_Base, "FMI", sep = "/")
sDiretorioBM <- paste(sDiretorio_Base, "Banco_Mundial", sep = "/")
sDiretorioIBRE <- paste(sDiretorio_Base, "IBRE", sep = "/")


# Lendo as bases ----
## FMI ----
### Juros ----
dfFMI_Juros <- readxl::read_excel(paste(sDiretorioFMI, 
                                        "Juros_FMI_Ano.xlsx", 
                                        sep = "/"), sheet = "Sheet 1")

## Filtrando (entre 2003 e 2021)
dfFMI_Juros %<>%
    filter((Taxa_Juros_Politica_CP >= 0) & (Ano > 2002) & (Ano <= 2021)) %>% 
    group_by(iso2c) %>% 
    mutate(num_aparicoes = n()) %>%  # contando quantas vezes cada país aparece
    filter(num_aparicoes == 19) %>%  # apenas países que aparecem em todos os anos
    arrange(iso2c, Ano)  # ordem bonitinha


### Investimento Público e Privado ----
dfFMI_Inv <- readxl::read_excel(paste(sDiretorioFMI, 
                                      "IMFInvestmentandCapitalStockDataset2021.xlsx", 
                                      sep = "/"), sheet = "Dataset")

## Filtrando (entre 2003 e 2021)
dfFMI_Inv %<>%
    filter((i_GDP_rppp > 0) & (year > 2002) & (year <= 2021)) %>%  
    group_by(isocode) %>% 
    mutate(num_aparicoes = n()) %>%  # contando quantas vezes cada país aparece
    filter(num_aparicoes == 17) %>%  # apenas países que aparecem em todos os anos
    select(isocode, iso2c, ifscode, country, year, income, 
           ipriv_GDP_rppp, igov_GDP_rppp, i_GDP_rppp) %>%  # colunas relevantes
    arrange(isocode, year)  # ordem bonitinha

## Renomeando
names(dfFMI_Inv) <- c("iso3c", "iso2c", "Code", "Nome", "Ano",
                      "Categoria_Pais_FMI",
                      "Investimento_Privado_PIB_PPP",
                      "Investimento_Publico_PIB_PPP",
                      "Investimento_PIB_PPP")

## Contando
length(unique(dfFMI_Inv$iso3c))

### Resultado Primário Ciclicamente Ajustado ----
dfFMI_Res <- readxl::read_excel(paste(sDiretorioFMI, 
                                      "Painel_FMI_Fiscal_Ciclo.xlsx", 
                                      sep = "/"), sheet = "Dataset")

## Renomeando (CA = ciclicamente ajustado)
names(dfFMI_Res) <- c("Nome", "Code", "iso2c", "iso3c", "Ano",
                      "Resultado_Nominal_CA_PIB_Pot", "Resultado_Primario_CA_PIB_Pot",
                      "Despesas_PIB", "Divida_Bruta_PIB", "Divida_Liquida_PIB",
                      "Resultado_Nominal_PIB", "Resultado_Primario_PIB", "Receitas_PIB")

## Selecionando colunas relevantes e filtrando
dfFMI_Res %<>%
    filter(!is.na(Resultado_Nominal_CA_PIB_Pot) & (Ano > 2002) & (Ano <= 2021)) %>% 
    group_by(iso2c) %>% 
    mutate(num_aparicoes = n()) %>%  # contando quantas vezes cada país aparece
    filter(num_aparicoes == 19) %>%  # apenas países que aparecem em todos os anos
    select(iso2c, iso3c, Code, Nome, Ano, 
           Resultado_Nominal_CA_PIB_Pot, Resultado_Primario_CA_PIB_Pot, 
           Resultado_Nominal_PIB, Resultado_Primario_PIB)

## Contando
length(unique(dfFMI_Res$iso2c))

### Governo Geral e Federal para o Brasil ----
dfFMI_Brasil <- readxl::read_excel(paste(sDiretorioFMI, "Deficit_BR_Ano.xlsx", sep = "/"))


## Banco Mundial ----
### WDI ----
dfWDI <- readxl::read_excel(paste(sDiretorioBM, "Dados_WDI_Ano.xlsx", sep = "/"))

### Governance Indicators ----
# Lendo Controle da Corrupçõa e Estabilidade Política
dfWGI <- readxl::read_excel(paste(sDiretorioBM, "wgidataset.xlsx", sep = "/"), 
                            sheet = "Saida", na = "#N/A")

# Pivotando pra formato longo e depois colocando os indicadores em duas colunas
dfWGI %<>% 
    pivot_longer(cols = !c(iso3c, Country, Indicador),
                 names_to = "Ano", values_to = "Valor") %>% 
    pivot_wider(names_from = Indicador, values_from = Valor)

# Juntando ----
## Fazendo os merges
## Aqui, surge um problema de NAs que temos que resolver para 2020 e 2021
## (dados de investimento PPP não disponíveis para esses anos, apesar de essa
## base ter o maior número de países)
dfFinal <- merge(dfFMI_Inv, dfFMI_Res, by = c('iso3c', 'iso2c', 'Ano'), 
                 all.x = T, all.y = T)

## Colocando as categorias do país e os códigos para 2020 e 2021 + 
## retirando linhas extras desnecessárias: grupos de países e países problemáticos
dfFinal %<>% 
    filter(str_length(iso2c) <= 2) %>% 
    filter((iso2c != "NA") & (iso2c != "XK")) %>% 
    group_by(iso2c) %>% 
    mutate(Categoria_Pais_FMI = ifelse(Ano >= 2020, Categoria_Pais_FMI[1], Categoria_Pais_FMI))

# Continuando
dfFinal <- merge(dfFinal, dfFMI_Juros, by = c('iso2c', 'Ano'), all.x = T)
dfFinal <- merge(dfFinal, dfWDI, by = c('iso2c', 'Ano'), all.x = T)
dfFinal <- merge(dfFinal, dfWGI, by = c('iso3c', 'Ano'), all.x = T)
names(dfFinal)

# Modificações ----
## Taxa Real de Juros ----
dfFinal$Taxa_Juros_Real_Politica_CP <- 100 * ((1 + dfFinal$Taxa_Juros_Politica_CP/100) / 
    (1 + dfFinal$Inflacao_Consumidor/100) - 1)

## Especificações CS ----
# Colunas auxiliares
dfFinal$Pre_Tratamento <- ifelse(dfFinal$Ano < 2017, 1, 0)
dfFinal$Pos_Tratamento <- ifelse(dfFinal$Ano >= 2017, 1, 0)
dfFinal$Brasil <- ifelse(dfFinal$iso2c == "BR", 1, 0)
dfFinal$Brasil_Tratado <- ifelse((dfFinal$Brasil == 1) & (dfFinal$Pos_Tratamento == 1), 1 , 0)

dfFinal$Metade <- ifelse(dfFinal$Ano < 2011, 1, NA)
dfFinal$TresQuartos <- ifelse(dfFinal$Ano < 2015, 1, NA)
dfFinal$QuartoFinal <- ifelse(dfFinal$Ano >= 2015, 1, NA)

# Especificação 1: média dos períodos pré-tratamento
dfFinal %<>% 
    group_by(iso2c, Pre_Tratamento) %>% 
    mutate(Media_Taxa_Juros_Politica_CP = mean(Taxa_Juros_Politica_CP),
           Media_Taxa_Juros_Real_Politica_CP = mean(Taxa_Juros_Real_Politica_CP),
           Media_Investimento_Privado_PIB_PPP = mean(Investimento_Privado_PIB_PPP),
           Media_Investimento_Publico_PIB_PPP = mean(Investimento_Publico_PIB_PPP),
           Media_Investimento_PIB_PPP = mean(Investimento_PIB_PPP),
           Media_Resultado_Primario_CA_PIB_Pot = mean(Resultado_Primario_CA_PIB_Pot),
           Media_Resultado_Primario_PIB = mean(Resultado_Primario_PIB))

# Especificação 2: próprio valor (não precisa fazer nada)

# Especificação 3: metade dos períodos pré-tratamento
dfFinal %<>% 
    mutate(Metade_Taxa_Juros_Politica_CP = Metade*Taxa_Juros_Politica_CP,
           Metade_Taxa_Juros_Real_Politica_CP = Metade*Taxa_Juros_Real_Politica_CP,
           Metade_Investimento_Privado_PIB_PPP = Metade*Investimento_Privado_PIB_PPP,
           Metade_Investimento_Publico_PIB_PPP = Metade*Investimento_Publico_PIB_PPP,
           Metade_Investimento_PIB_PPP = Metade*Investimento_PIB_PPP,
           Metade_Resultado_Primario_CA_PIB_Pot = Metade*Resultado_Primario_CA_PIB_Pot,
           Metade_Resultado_Primario_PIB = Metade*Resultado_Primario_PIB)

# Especificação 4: tres_quartos dos períodos pré-tratamento
dfFinal %<>% 
    mutate(TresQuartos_Taxa_Juros_Politica_CP = TresQuartos*Taxa_Juros_Politica_CP,
           TresQuartos_Taxa_Juros_Real_Politica_CP = TresQuartos*Taxa_Juros_Real_Politica_CP,
           TresQuartos_Investimento_Privado_PIB_PPP = TresQuartos*Investimento_Privado_PIB_PPP,
           TresQuartos_Investimento_Publico_PIB_PPP = TresQuartos*Investimento_Publico_PIB_PPP,
           TresQuartos_Investimento_PIB_PPP = TresQuartos*Investimento_PIB_PPP,
           TresQuartos_Resultado_Primario_CA_PIB_Pot = TresQuartos*Resultado_Primario_CA_PIB_Pot,
           TresQuartos_Resultado_Primario_PIB = TresQuartos*Resultado_Primario_PIB)

# Especificação 5: último quarto dos períodos pré-tratamento
dfFinal %<>% 
    mutate(QuartoFinal_Taxa_Juros_Politica_CP = QuartoFinal*Taxa_Juros_Politica_CP,
           QuartoFinal_Taxa_Juros_Real_Politica_CP = QuartoFinal*Taxa_Juros_Real_Politica_CP,
           QuartoFinal_Investimento_Privado_PIB_PPP = QuartoFinal*Investimento_Privado_PIB_PPP,
           QuartoFinal_Investimento_Publico_PIB_PPP = QuartoFinal*Investimento_Publico_PIB_PPP,
           QuartoFinal_Investimento_PIB_PPP = QuartoFinal*Investimento_PIB_PPP,
           QuartoFinal_Resultado_Primario_CA_PIB_Pot = QuartoFinal*Resultado_Primario_CA_PIB_Pot,
           QuartoFinal_Resultado_Primario_PIB = QuartoFinal*Resultado_Primario_PIB)

## Seleção ----
dfFinal %<>%
    select(iso2c, iso3c, Categoria_Pais_FMI, Ano, Pre_Tratamento,
           Investimento_Privado_PIB_PPP, Investimento_Publico_PIB_PPP, 
           Investimento_PIB_PPP, Investimento_PIB,
           Resultado_Primario_CA_PIB_Pot, Resultado_Primario_PIB, 
           Taxa_Juros_Politica_CP, Taxa_Juros_Real_Politica_CP,
           
           Poupanca_Dom_PIB, Consumo_Total_PIB, Consumo_Governo_PIB, 
           Consumo_Privado_PIB, Inflacao_Consumidor, Inflacao_Consumidor_Acum, 
           PIB_Capita_PPP17, Crescimento_PIB,
           Taxa_Cambio100, Taxa_Desemprego, Valor_Add_Industria, 
           Termos_Troca, Conta_Corrente_PIB,
           Controle_Corrupcao, Estabilidade_Politica,
           Resultado_Nominal_CA_PIB_Pot, Resultado_Nominal_PIB,
           
           Media_Taxa_Juros_Politica_CP, Media_Taxa_Juros_Real_Politica_CP,
           Media_Investimento_Privado_PIB_PPP, Media_Investimento_Publico_PIB_PPP, 
           Media_Investimento_PIB_PPP,
           Media_Resultado_Primario_CA_PIB_Pot, Media_Resultado_Primario_PIB,
           
           Metade_Taxa_Juros_Politica_CP, Metade_Taxa_Juros_Real_Politica_CP,
           Metade_Investimento_Privado_PIB_PPP, Metade_Investimento_Publico_PIB_PPP, 
           Metade_Investimento_PIB_PPP,
           Metade_Resultado_Primario_CA_PIB_Pot, Metade_Resultado_Primario_PIB,
           
           TresQuartos_Taxa_Juros_Politica_CP, TresQuartos_Taxa_Juros_Real_Politica_CP,
           TresQuartos_Investimento_Privado_PIB_PPP, TresQuartos_Investimento_Publico_PIB_PPP, 
           TresQuartos_Investimento_PIB_PPP,
           TresQuartos_Resultado_Primario_CA_PIB_Pot, TresQuartos_Resultado_Primario_PIB,
           
           QuartoFinal_Taxa_Juros_Politica_CP, QuartoFinal_Taxa_Juros_Real_Politica_CP,
           QuartoFinal_Investimento_Privado_PIB_PPP, QuartoFinal_Investimento_Publico_PIB_PPP, 
           QuartoFinal_Investimento_PIB_PPP,
           QuartoFinal_Resultado_Primario_CA_PIB_Pot, QuartoFinal_Resultado_Primario_PIB)

## Formato Longo ----
dfFinalLongo <- dfFinal %>% 
    pivot_longer(cols = !c(iso2c, iso3c, Categoria_Pais_FMI, Ano, Pre_Tratamento),
                 names_to = "Variável",
                 values_to = "Valor")

### Apenas Brasil
dfFinalLongoBR <- dfFinalLongo %>% 
    filter(iso2c == "BR")

## Salvando no Excel ----
if (bSaveDados) {
    # Duas abas na mesma planilha - normal e long
    ## Criando o objeto
    wbWorkbook <- createWorkbook()
    
    # Adicionando abas
    addWorksheet(wbWorkbook, sheetName = "Dataset")
    addWorksheet(wbWorkbook, sheetName = "Dataset_Long")
    
    # Adicionando os DataFrame às respectivas abas
    writeData(wbWorkbook, "Dataset", dfFinal)
    writeData(wbWorkbook, "Dataset_Long", dfFinalLongo)
    
    # Salvando
    saveWorkbook(wbWorkbook,
                 paste(sDiretorioOutput, "Base_Investimento_Deficit_Juros.xlsx", sep = "/"),
                 overwrite = TRUE)
}

# Análise Descritiva ----
## Definindo paleta
Palette <- wes_palette("Darjeeling1")

## Trocando NAs por 0
dfFMI_Brasil[is.na(dfFMI_Brasil)] <- 0

## Formato longo
dfFMI_BrasilLongo <- dfFMI_Brasil %>% 
    pivot_longer(cols = !c(iso2c, Year),
                 names_to = "Variável",
                 values_to = "Valor")

# Calculando médias dos emergentes
dfFinalLongo_Emerg <- dfFinalLongo %>% 
   filter(!iso2c %in% c("BR", "AR")) %>% 
   filter(Categoria_Pais_FMI == "Emerging Market Economies") %>% 
   group_by(Ano, Variável) %>% 
   mutate(Media = mean(Valor, na.rm = T)) %>% 
   as.data.frame()

## Juros ----
cListaVariaveisJuros <- c("Taxa_Juros_Politica_CP", "Taxa_Juros_Real_Politica_CP")
listLabelsJuros <- c(`Taxa_Juros_Politica_CP` = "Taxa Nominal", 
                     `Taxa_Juros_Real_Politica_CP` = "Taxa Real")

### Brasil ----
dfFinalLongoBR %>% 
    filter((Variável %in% cListaVariaveisJuros) & (Ano >= 2010))  %>% 
    ggplot(aes(x = Ano, y = Valor, colour = Variável)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'red', show.legend = F) + 
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Taxa de Juros de Política Monetária (%)') +
    scale_colour_manual(name = "", values = Palette, labels = c("Nominal", "Real")) +
    scale_x_continuous(breaks = seq(2010, 2021, 1)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(-3.5, 15, by = 2), limits = c(-3.5, 15)) +
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
ggsave(paste(sDiretorioFiguras, "Juros_BR.pdf", sep = "/"), dpi = 300)

### Emergentes ----
dfFinalLongo_Emerg %>% 
    filter(Variável %in% cListaVariaveisJuros)  %>% 
    ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
    geom_line(size = .5, colour = "lightgray") + 
    # Brasil
    geom_point(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisJuros, ], 
               size = 2.5) + 
    geom_line(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisJuros, ],
              lwd = 1.25) +
    # Média Emergentes
   geom_line(aes(y = Media, colour = Categoria_Pais_FMI), 
             lwd = 1.25, linetype = "dashed") + 
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Taxa de Juros de Política Monetária - Média Anual (%)') +
    scale_x_continuous(breaks = seq(2003, 2021, 4)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
   scale_colour_manual(
      name = "",
      values = c(met.brewer("Signac")[14], "black"),
      labels = c("Brasil", "Média dos Emergentes (sem Argentina)")
   ) +
   theme_bw() +
   theme(
      legend.position = 'top',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(size = 12)
   ) + 
    facet_wrap(vars(Variável), labeller = as_labeller(listLabelsJuros))
ggsave(paste(sDiretorioFiguras, "Juros_Emergentes_v2.pdf", sep = "/"), dpi = 600)


## Investimento ----
cListaVariaveisInv <- c("Investimento_Privado_PIB_PPP", "Investimento_Publico_PIB_PPP",
                        "Investimento_PIB_PPP")
listLabelsInv <- c(
    `Investimento_Privado_PIB_PPP` = "Inv. Privado Real PPP", 
    `Investimento_Publico_PIB_PPP` = "Inv. Público Real PPP",
    `Investimento_PIB_PPP` = "Inv. Total Real PPP")

### Brasil ----
dfFinalLongoBR %>% 
    filter(Variável %in% c(cListaVariaveisInv, "Investimento_PIB"))  %>% 
    ggplot(aes(x = Ano, y = Valor, colour = Variável)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Taxa de Investimento (% do PIB)') +
    scale_colour_manual(
        name = "",
        values = Palette,
        labels = c("Total Nominal", "Total Real PPC", 
                   "Privado Real PPC", "Público Real PPC")
    ) +
    scale_x_continuous(breaks = seq(2003, 2021, 2)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(0, 21, 3)) +
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
ggsave(paste(sDiretorioFiguras, "Investimento_BR.pdf", sep = "/"), dpi = 600)

### Emergentes ----
dfFinalLongo %>% 
    filter((Variável %in% cListaVariaveisInv))  %>% 
    filter(Categoria_Pais_FMI == "Emerging Market Economies") %>%  # apenas emergentes
    ggplot(aes(x = Ano, y = Valor, group = iso2c)) +
    geom_line(size = .5, colour = "lightgray") + 
    # Brasil
    geom_point(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisInv, ], 
               size = 2.5, colour = "black") + 
    geom_line(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisInv, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Variável como % do PIB') +
    scale_x_continuous(breaks = seq(2003, 2019, 4), limits = c(2003, 2019)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'none') + 
    facet_wrap(vars(Variável), labeller = as_labeller(listLabelsInv))
ggsave(paste(sDiretorioFiguras, "Investimento_Emergentes.pdf", sep = "/"), dpi = 600)


## Déficit FMI ----
cListaVariaveisDef <- c("Resultado_Nominal_CA_PIB_Pot", "Resultado_Primario_CA_PIB_Pot",
                        "Resultado_Nominal_PIB", "Resultado_Primario_PIB")
listLabelsDef <- c(
    `Resultado_Nominal_CA_PIB_Pot` = "Res. Nominal CA", 
    `Resultado_Primario_CA_PIB_Pot` = "Res. Primário CA",
    `Resultado_Nominal_PIB` = "Res. Nominal",
    `Resultado_Primario_PIB` = "Res. Primário")

### Brasil ----
# Nominal e Primário
dfFinalLongoBR %>% 
    filter(Variável %in% cListaVariaveisDef)  %>% 
    ggplot(aes(x = Ano, y = Valor, colour = Variável)) +
    geom_line(size = 1) + 
    geom_vline(xintercept = 2016, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Resultado - % do PIB (Potencial)') +
    scale_colour_manual(
        name = "",
        values = Palette,
        labels = c("Nominal CA", "Nominal", "Primário CA", "Primário")
    ) +
    scale_x_continuous(breaks = seq(2003, 2021, 2)) + 
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
ggsave(paste(sDiretorioFiguras, "Deficit_BR.pdf", sep = "/"), dpi = 300)

# Apenas Primário
dfFinalLongoBR %>% 
    filter((Variável %in% cListaVariaveisDef[c(2, 4)]) & (Ano >= 2010))  %>% 
    ggplot(aes(x = Ano, y = Valor, colour = Variável)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_vline(xintercept = 2016.8, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Resultado Primário') +
    scale_colour_manual(
        name = "",
        values = Palette[c(2, 3)],
        labels = c("Ciclicamente Ajustado - % do PIB Potencial", 
                   "Corrente - % do PIB")
    ) +
    scale_x_continuous(breaks = seq(2010, 2021, 1)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                       breaks = seq(-8.5, 3, 1.5)) +
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
ggsave(paste(sDiretorioFiguras, "Deficit_Primario_BR.pdf", sep = "/"), dpi = 300)


### Emergentes ----
# Nominal e Primário
dfFinalLongo %>% 
    filter(Variável %in% cListaVariaveisDef)  %>% 
    filter(Categoria_Pais_FMI == "Emerging Market Economies") %>%  # apenas emergentes
    ggplot(aes(x = Ano, y = Valor, group = iso2c)) +
    geom_line(size = .5, colour = "lightgray") + 
    # Brasil
    geom_point(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisDef, ], 
               size = 2.5, colour = "black") + 
    geom_line(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisDef, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Resultado - % do PIB (Potencial)') +
    scale_x_continuous(breaks = seq(2003, 2021, 4)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'none') + 
    facet_wrap(vars(Variável), nrow = 2, labeller = as_labeller(listLabelsDef))
ggsave(paste(sDiretorioFiguras, "Deficit_Emergentes.pdf", sep = "/"), dpi = 600)

# Apenas Primário
dfFinalLongo %>% 
    filter(Variável %in% cListaVariaveisDef[c(2, 4)])  %>% 
    filter(Categoria_Pais_FMI == "Emerging Market Economies") %>%  # apenas emergentes
    ggplot(aes(x = Ano, y = Valor, group = iso2c)) +
    geom_line(size = .5, colour = "lightgray") + 
    # Brasil
    geom_point(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisDef[c(2, 4)], ], 
               size = 2.5, colour = "black") + 
    geom_line(data = dfFinalLongoBR[dfFinalLongoBR$Variável %in% cListaVariaveisDef[c(2, 4)], ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    geom_hline(yintercept = 0, linetype = 'longdash', 
               size = .3, colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Resultado - % do PIB (Potencial)') +
    scale_x_continuous(breaks = seq(2003, 2021, 4)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'none') + 
    facet_wrap(vars(Variável), ncol = 2, labeller = as_labeller(listLabelsDef[c(2, 4)]))
ggsave(paste(sDiretorioFiguras, "Deficit_Primario_Emergentes.pdf", sep = "/"), dpi = 600)

