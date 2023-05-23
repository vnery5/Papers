# Setup e bibliotecas ----
library(tidyverse)
library(magrittr)
library(dplyr)
library(imfr)
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
sDiretorioOutput <- paste(sDiretorio_Base, "FMI", sep = "/")

# Países ----
cListaPaisesEmergLatAM <- c("AR", "BR", "CL", "CN", "CO", "EG", "HU", "IN", "ID", "IR", 
                            "MY", "MX", "PH", "PL", "RU", "SA", "ZA", "TH", "TR", "AE",
                            "AG", "AW", "BS", "BB", "BZ", "BO", "CR", "DM", "DO", "EC",
                            "SV", "GD", "GT", "GY", "HT", "HN", "JM", "NI", "PA", "PY",
                            "PE", "KN", "LC", "VC", "SR", "TT", "UY", "VE")

cListaPaises<- c("AR", "BR", "CL", "CN", "CO", "EG", "HU", "IN", "ID", "IR", 
                 "MY", "MX", "PH", "PL", "RU", "SA", "ZA", "TH", "TR", "AE",
                 "AG", "AW", "BS", "BB", "BZ", "BO", "CR", "DM", "DO", "EC",
                 "SV", "GD", "GT", "GY", "HT", "HN", "JM", "NI", "PA", "PY",
                 "PE", "KN", "LC", "VC", "SR", "TT", "UY", "VE",
                 "AE", "AT", "AU", "BE", "BG", "CH", "CZ", "DE", "DK", "PT",
                 "EE", "ES", "GR", "IE", "IL", "IQ", "IT", "LT", "LV", "MA",
                 "US", "CA", "FI", "SE", "SI")

## Pegando correspondencias de códigos
dfISO <- readxl::read_excel("ISO_Codes.xlsx")

# Taxas de Juros ----
## Usando o pacote do FMI
## Ver pasta do FMI para a planilha com os códigos (Codigos_FMI_IFS.xlsx)
dfFMI_Juros <- imf_data(
    database_id = "IFS",
    indicator = c("FITB_PA", "FPOLM_PA", "PCPI_IX"),
    # indicator = c("FILR_PA", "FIDR_PA", "FITB_PA", "FPOLM_PA"),
    # country = cListaPaisesEmergLatAM,
    start = 2003,
    end = 2021,
    freq = "M"
)

## Renomeando
names(dfFMI_Juros) <- c("iso2c", 'year_month', 
                        'Taxa_Tesouro', 
                        'Taxa_Juros_Politica_CP', 
                        'Inflacao_Consumidor_Mensal')

## Separando ano e mes
dfFMI_Juros <- separate(data = dfFMI_Juros, col = year_month, into = c("Ano", "month"))

## Nomeando os países
dfFMI_Juros <- merge(dfFMI_Juros, dfISO, 
                     by.x = "iso2c", by.y = "Code", sort = FALSE)

## Tirando NAs
dfFMI_Juros <- dfFMI_Juros %>% 
    filter((!is.na(Taxa_Tesouro)) | (!is.na(Taxa_Juros_Politica_CP))) 

## Média do ano ----
### Inflação ----
dfFMI_Juros %<>%
    group_by(iso2c) %>% 
    mutate(Inflacao_Consumidor_12Meses = 100*Inflacao_Consumidor_Mensal / 
               lag(Inflacao_Consumidor_Mensal, n = 12) - 1)


### Juros ----
dfFMI_JurosAno <- dfFMI_Juros %>% 
    group_by(Name, iso2c, Ano) %>% 
    summarise(Taxa_Tesouro = mean(Taxa_Tesouro),
              Taxa_Juros_Politica_CP = mean(Taxa_Juros_Politica_CP),
              Inflacao_Consumidor_MediaAno = mean(Inflacao_Consumidor_12Meses))

# Fiscal ----
## Ver https://www.imf.org/external/pubs/ft/gfs/manual/pdf/ch4.pdf
## Dados do FMI em moeda nacional nominal
dfFMI_Deficit <- imf_data(
    database_id = "IFS",
    indicator = c("GG_GXCBG_G01_XDC", "GG_GXCBN_G01_XDC",
                  "GG_GXOB_G01_XDC", "GG_GXCNLA_G01_XDC",
                  "GG_GEI_G01_XDC"
    ),
    country = cListaPaises,
    start = 2010,
    end = 2021,
    freq = "A"
)

## Dados do governo central (sim, tem que ser separado)
dfFMI_Deficit_BR <- imf_data(
    database_id = "IFS",
    indicator = c("GG_GXCBG_G01_XDC", 
                  "GG_GXCBN_G01_XDC",
                  "GG_GXOB_G01_XDC", "GG_GXCNLA_G01_XDC",
                  "GG_GEI_G01_XDC",
                  
                  "NGDP_XDC",
                  
                  "BCG_GXCBG_G01_XDC", 
                  "BCG_GXCBN_G01_XDC",
                  "BCG_GXOB_G01_XDC",
                  "BCG_GEI_G01_XDC"
    ),
    country = 'BR',
    start = 2006,  # primeiro ano com dados
    end = 2021,
    freq = "A"
)

## Renomeando
names(dfFMI_Deficit) <- c("iso2c", 'Year', 
                          'Resultado_Operacional_Bruto', 
                          'Resultado_Nominal',  # operacional líquido
                          'Despesas_Juros',
                          'Net_Lending', 'Adj_Net_Lending')

names(dfFMI_Deficit_BR) <- c("iso2c", 'Year', 
                          'Resultado_Operacional_Bruto', 
                          'Resultado_Nominal', 
                          'Net_Lending', 'Adj_Net_Lending',
                          'Despesas_Juros',
                          
                          'PIB_Nominal',
                          
                          'Resultado_Operacional_Bruto_BCG', 
                          'Resultado_Nominal_BCG', 
                          'Net_Lending_BCG',
                          'Despesas_Juros_BCG')

## Criando variáveis
# Resultado primário: resultado operacional líquido - juros
## Geral
dfFMI_Deficit$Resultado_Primario <- dfFMI_Deficit$Resultado_Nominal + 
    dfFMI_Deficit$Despesas_Juros

## Brasil
### Resultado Primário como % do PIB a preços correntes
dfFMI_Deficit_BR$Despesas_Juros_PIB <- dfFMI_Deficit_BR$Despesas_Juros / dfFMI_Deficit_BR$PIB_Nominal * 100

dfFMI_Deficit_BR$Despesas_Juros_BCG_PIB <- dfFMI_Deficit_BR$Despesas_Juros_BCG / dfFMI_Deficit_BR$PIB_Nominal * 100

### Resultado Nominal como % do PIB a preços correntes
dfFMI_Deficit_BR$Resultado_Nominal_PIB <- dfFMI_Deficit_BR$Resultado_Nominal / dfFMI_Deficit_BR$PIB_Nominal * 100

dfFMI_Deficit_BR$Resultado_Nominal_BCG_PIB <- dfFMI_Deficit_BR$Resultado_Nominal_BCG / dfFMI_Deficit_BR$PIB_Nominal * 100

### Net Lending/Borrowing
dfFMI_Deficit_BR$Net_Lending_PIB <- (dfFMI_Deficit_BR$Net_Lending + 
    dfFMI_Deficit_BR$Adj_Net_Lending) / dfFMI_Deficit_BR$PIB_Nominal * 100

dfFMI_Deficit_BR$Net_Lending_BCG_PIB <- dfFMI_Deficit_BR$Net_Lending_BCG / dfFMI_Deficit_BR$PIB_Nominal * 100

## Tirando NAs
dfFMI_Deficit <- dfFMI_Deficit %>% 
    filter(!is.na(Year)) %>% 
    filter(!is.na(Resultado_Primario))

# Salvando ----
write.xlsx(dfFMI_JurosAno,
           paste(sDiretorioOutput, "Juros_FMI_Ano.xlsx", sep = "/"))

write.xlsx(dfFMI_Deficit,
           paste(sDiretorioOutput, "Deficit_FMI_Ano.xlsx", sep = "/"))

write.xlsx(dfFMI_Deficit_BR,
           paste(sDiretorioOutput, "Deficit_BR_Ano.xlsx", sep = "/"))
