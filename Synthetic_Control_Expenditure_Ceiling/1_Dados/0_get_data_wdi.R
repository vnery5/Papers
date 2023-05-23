# Setup e bibliotecas ----
library(tidyverse)
library(dplyr)
library(magrittr)
library(WDI)
library(openxlsx)
library(lubridate)
library(zoo)

## Comandos Úteis
# Apagar Variáveis
rm(list = ls())
# Apagar plots
dev.off()
# Limpar Console
cat("\014")

## Settando o diretório para o do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)
sDiretorioOutput <- paste(sDiretorio_Base, "Banco_Mundial", sep = "/")
sDiretorioFiguras <- paste(sDiretorio_Base, "Figuras", sep = "/")

# Países ----
cListaPaisesEmergLatAM <- c("AR", "BR", "CL", "CN", "CO", "EG", "HU", "IN", "ID", "IR", 
                            "MY", "MX", "PH", "PL", "RU", "SA", "ZA", "TH", "TR", "AE",
                            "AG", "AW", "BS", "BB", "BZ", "BO", "CR", "DM", "DO", "EC",
                            "SV", "GD", "GT", "GY", "HT", "HN", "JM", "NI", "PA", "PY",
                            "PE", "KN", "LC", "VC", "SR", "TT", "UY", "VE")

# Poupanca, Impostos e Consumo do Governo Geral ----
## Usando o pacote do Banco Mundial
dfWDI <- WDI(
    # country = cListaPaisesEmergLatAM,
    indicator = c("NY.GNS.ICTR.ZS",  # Poupança
                  "NE.GDI.TOTL.ZS",  # Investimento como % do PIB
                  "NE.CON.TOTL.ZS",  # Consumo total
                  "NE.CON.GOVT.ZS",  # Consumo governamental
                  "NE.CON.PRVT.ZS",  # Coonsumo Famílias e ISFLSF
                  "NY.GDP.DEFL.KD.ZG",  # Deflator do PIB
                  "FP.CPI.TOTL.ZG",  # Inflação do Consumidor
                  "NY.GDP.MKTP.KD.ZG",  # Crescimento do PIB
                  "NY.GDP.PCAP.PP.KD",  # PIB per Capita PPP 2017
                  "PA.NUS.FCRF",  # Câmbio
                  "SL.UEM.TOTL.ZS",  # Desemprego: Estimativa da International Labour Organization (OIT)
                  "NV.IND.MANF.ZS",  # Valor adicionado da indústria
                  "TT.PRI.MRCH.XD.WD",  # Net Barter Terms of Trade
                  "BN.CAB.XOKA.GD.ZS"  # Conta Corrente como % do PIB
                  ),
    start = 2003,
    end = 2021
    )

## Renomeando
names(dfWDI) <- c("iso2c", 'Name', 'Ano',
                  'Poupanca_Dom_PIB', 
                  'Investimento_PIB',
                  'Consumo_Total_PIB',
                  'Consumo_Governo_PIB',
                  'Consumo_Privado_PIB',  # famílias e ISFLSF
                  'Inflacao_Deflator',
                  'Inflacao_Consumidor',
                  'Crescimento_PIB',
                  'PIB_Capita_PPP17',
                  'Taxa_Cambio',
                  'Taxa_Desemprego',
                  'Valor_Add_Industria',
                  'Termos_Troca',
                  'Conta_Corrente_PIB'
                  )

## Filtrando países sem dados
dfWDI <- dfWDI %>%
    filter(((!is.na(Investimento_PIB)) | 
               (!is.na(PIB_Capita_PPP17))) & 
               (!is.na(Taxa_Cambio))
           )

## Normalizando ----
# Inflacao Acumulada (2003 = 100)
dfWDI %<>%
    group_by(Name) %>% 
    mutate(Inflacao_Consumidor_Acum = cumprod(c(100, 1 + Inflacao_Consumidor[-1] / 100)))

# Câmbio (2003 = 100)
dfWDI %<>%
    group_by(Name) %>% 
    ## Coluna auxiliar com a variação percentual (not really, falta o -1) de cada ano
    mutate(Taxa_Cambio_Delta = Taxa_Cambio / lag(Taxa_Cambio)) %>% 
    ## Criando o índice encadeado ([-1] (removendo a primeira linha de cada país) para que o cumprimento de cada vetor bata)
    mutate(Taxa_Cambio100 = cumprod(c(100, Taxa_Cambio_Delta[-1]))) %>% 
    select(-Taxa_Cambio_Delta)  # removendo a coluna auxiliar


# Salvando ----
write.xlsx(dfWDI,
           paste(sDiretorioOutput, "Dados_WDI_Ano.xlsx", sep = "/"))
