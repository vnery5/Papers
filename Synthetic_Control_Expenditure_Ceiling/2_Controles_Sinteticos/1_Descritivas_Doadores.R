# Setup e Bibliotecas ==========================================================
# Dados
library(tidyverse)
library(magrittr)
library(lubridate)
library(zoo)
library(openxlsx)
library(readxl)

# Controle Sintético
library(augsynth)
library(scpi)

# Gráficos
library(wesanderson)  # https://github.com/karthik/wesanderson
library(MetBrewer)  # https://github.com/BlakeRMills/MetBrewer/tree/main
library(plotly)

# Comandos Úteis
rm(list = ls())
cat("\014")

# Settando o diretório para o do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Definindo paletas
Palette <- wes_palette("Darjeeling1")
Palette2 <- wes_palette("BottleRocket2")

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

# Restringindo
df <- dfTodos %>% 
    filter(iso2c %in% vDoadores) %>% 
    group_by(iso2c, Variável) %>% 
    arrange(Ano) %>% 
    mutate(PD = Valor - lag(Valor),
           Delta = Valor / lag(Valor),
           Valor100 = cumprod(c(100, Delta[-1]))) %>%  # 2016 = 100
    select(-Delta) %>% 
    as.data.frame()

# Fazendo 2016 = 100
## DataFrame auxiliar
# df2016 <- df %>% 
#     filter(Ano == 2016) %>% 
#     select(iso2c, Variável, Valor100) %>% 
#     rename(Valor100_2016 = Valor100)
# 
# ## Joining
# df %<>% 
#     left_join(df2016, by = c("iso2c", "Variável")) %>% 
#     mutate(Valor100 = Valor100 * 100 / Valor100_2016) %>% 
#     select(-Valor100_2016)

# Brasil 
dfBR <- df %>% 
    filter(iso2c == "BR")

# Médias ex-Brasil
dfexBr <- df %>% 
   filter(iso2c != "BR") %>% 
   group_by(Ano, Variável) %>% 
   mutate(Media = mean(Valor, na.rm = T)) %>% 
   as.data.frame()

# Gráficos =====================================================================
## Investimento ----------------------------------------------------------------
# Definindo variáveis para os facets
cListaVariaveisInv <- c("Investimento_Privado_PIB_PPP", 
                        "Investimento_Publico_PIB_PPP",
                        "Investimento_PIB_PPP", 
                        "Investimento_PIB")

listLabelsInv <- c(
    `Investimento_Privado_PIB_PPP` = "Investimento Privado Real PPC", 
    `Investimento_Publico_PIB_PPP` = "Investimento Público Real PPC",
    `Investimento_PIB_PPP` = "Investimento Total Real PPC",
    `Investimento_PIB` = "Investimento Total Preços Correntes"
)

### Nível ####
plotInv_Nivel <- df %>% 
    filter(Variável %in% cListaVariaveisInv)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Variável como % do PIB') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsInv), scales = "free_y")

ggplotly(plotInv_Nivel)
ggsave("Figuras/Descritivas/Descritivas_Investimento_Nivel.pdf", dpi = 1200)

#### Preto e Cinza ####
df %>% 
   filter(Variável %in% cListaVariaveisInv)  %>% 
   filter(iso2c != "BR") %>% 
   ggplot(aes(x = Ano, y = Valor, group = iso2c)) +
   geom_line(size = .5, colour = "gray", show.legend = F) + 
   # Brasil
   geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ],
             lwd = 1.25, colour = "black", show.legend = F) +
   geom_point(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ], 
              size = 2.5, colour = "black") +
   
   geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
              colour = 'black', show.legend = F) + 
   labs(x = 'Ano', y = 'Investimento como % do PIB') +
   scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   theme_bw() +
   facet_wrap(vars(Variável), 
              labeller = as_labeller(listLabelsInv), scales = "free_y")
ggsave("Figuras/Descritivas/Descritivas_Investimento_Nivel_Preto.pdf", dpi = 1200)

#### Verde + Média ####
dfexBr %>% 
   filter(Variável %in% cListaVariaveisInv)  %>% 
   ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
   geom_line(size = .5, colour = "gray") + 
   # Brasil
   geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ],
             lwd = 1.25) +
   geom_point(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ], 
              size = 2.5) +
   # Média dos Emergentes
   geom_line(aes(y = Media, colour = Categoria_Pais_FMI), 
             lwd = 1.25, linetype = "dashed") + 
   geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
              colour = 'black', show.legend = F) + 
   labs(x = 'Ano', y = 'Investimento como % do PIB') +
   scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   scale_colour_manual(
      name = "",
      values = c(met.brewer("Signac")[14], "black"),
      labels = c("Brasil", "Média dos Doadores")
   ) +
   theme_bw() +
   theme(
      legend.position = 'top',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(size = 12)
   ) + 
   facet_wrap(vars(Variável), 
              labeller = as_labeller(listLabelsInv), scales = "free_y")
ggsave("Figuras/Descritivas/Descritivas_Investimento_Nivel_BR_Media.pdf", dpi = 1200)

# Só público e privado
dfexBr %>% 
   filter(Variável %in% cListaVariaveisInv[c(1:2)])  %>% 
   ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
   geom_line(size = .5, colour = "gray") + 
   # Brasil
   geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv[c(1:2)], ],
             lwd = 1.25) +
   geom_point(data = dfBR[dfBR$Variável %in% cListaVariaveisInv[c(1:2)], ], 
              size = 2.5) +
   # Média
   geom_line(aes(y = Media, colour = Categoria_Pais_FMI), 
             lwd = 1.25, linetype = "dashed") + 
   geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
              colour = 'black', show.legend = F) + 
   labs(x = 'Ano', y = 'Investimento como % do PIB') +
   scale_x_continuous(breaks = seq(2003, 2019, 3), limits = c(2003, 2019)) + 
   scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE)) +
   scale_colour_manual(
      name = "",
      values = c(met.brewer("Signac")[14], "black"),
      labels = c("Brasil", "Média dos Doadores")
   ) +
   theme_bw() +
   theme(
      legend.position = 'top',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(size = 12)
   ) + 
   facet_wrap(vars(Variável), 
              labeller = as_labeller(listLabelsInv[c(1:2)]), scales = "free_y")
ggsave("Figuras/Descritivas/Descritivas_Investimento_Nivel_BR_Media_PubPriv.pdf", dpi = 1200)


### Primeiras Diferenças ####
plotInv_PD <- df %>% 
    filter(Variável %in% cListaVariaveisInv)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = PD, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Variação como % do PIB') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsInv), scales = "free_y")

ggplotly(plotInv_PD)
ggsave("Figuras/Descritivas/Descritivas_Investimento_PD.pdf", dpi = 1200)

### Normalizado (2003 = 100) ####
plotInv_Norm <- df %>% 
    filter(Variável %in% cListaVariaveisInv)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = Valor100, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisInv, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Variável como % do PIB (2003 = 100)') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsInv), scales = "free_y")

ggplotly(plotInv_Norm)
ggsave("Figuras/Descritivas/Descritivas_Investimento_Norm.pdf", dpi = 1200)


## Juros -----------------------------------------------------------------------
# Definindo variáveis para os facets
cListaVariaveisJuros <- c("Taxa_Juros_Politica_CP", "Taxa_Juros_Real_Politica_CP")

listLabelsJuros <- c(`Taxa_Juros_Politica_CP` = "Taxa Nominal", 
                     `Taxa_Juros_Real_Politica_CP` = "Taxa Real")

### Nível ####
plotJuros_Nivel <- df %>% 
    filter(Variável %in% cListaVariaveisJuros)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisJuros, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = '% ao Ano') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsJuros), scales = "free_y")

ggplotly(plotJuros_Nivel)
ggsave("Figuras/Descritivas/Descritivas_Juros_Nivel.pdf", dpi = 1200)

### Primeiras Diferenças ####
plotJuros_PD <- df %>% 
    filter(Variável %in% cListaVariaveisJuros)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = PD, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisJuros, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = 'Variação % ao Ano') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsJuros), scales = "free_y")

ggplotly(plotJuros_PD)
ggsave("Figuras/Descritivas/Descritivas_Juros_PD.pdf", dpi = 1200)

### Normalizado (2003 = 100) ####
plotJuros_Norm <- df %>% 
    filter(Variável %in% cListaVariaveisJuros)  %>% 
    filter(iso2c != "BR") %>% 
    ggplot(aes(x = Ano, y = Valor100, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cListaVariaveisJuros, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = '% ao Ano (2003 = 100)') +
    scale_x_continuous(breaks = seq(2003, 2021, 4), limits = c(2003, 2021)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), 
               labeller = as_labeller(listLabelsJuros), scales = "free_y")

ggplotly(plotJuros_Norm)
ggsave("Figuras/Descritivas/Descritivas_Juros_Norm.pdf", dpi = 1200)

## Covariadas ------------------------------------------------------------------
cCovariadas <- c("Resultado_Primario_CA_PIB_Pot", "Inflacao_Consumidor",
                 "PIB_Capita_PPP17", "Taxa_Desemprego", "Valor_Add_Industria",
                 "Termos_Troca", "Conta_Corrente_PIB", "Taxa_Cambio100",
                 "Controle_Corrupcao", "Estabilidade_Politica")

plotCov_Nivel <- df %>% 
    filter(Variável %in% cCovariadas)  %>% 
    filter(iso2c != "BR") %>% 
    filter(iso2c != "AR") %>% 
    ggplot(aes(x = Ano, y = Valor, group = iso2c, colour = iso2c)) +
    geom_line(size = .5) + 
    # Brasil
    geom_line(data = dfBR[dfBR$Variável %in% cCovariadas, ],
              lwd = 1.25, colour = "black") +
    geom_vline(xintercept = 2016, linetype = 'longdash', size = .3, 
               colour = 'black', show.legend = F) + 
    labs(x = 'Ano', y = '% ao Ano') +
    scale_x_continuous(breaks = seq(2003, 2019, 4), limits = c(2003, 2019)) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE)) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) + 
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
    facet_wrap(vars(Variável), scales = "free_y", ncol = 3)

ggplotly(plotCov_Nivel)
ggsave("Figuras/Descritivas/Descritivas_Covariadas.pdf", dpi = 1200)
