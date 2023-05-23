# Bibliotecas ----
# Dados
library(tidyverse)
library(readxl)
library(janitor)
library(psych)
library(magrittr)

# Gráficos e Tabelas
library(ggplot2)
library(stargazer)
library(kableExtra)
library(patchwork)
library(viridis)

# Mapas
library(raster) 
library(rgdal) # Mapas/polígonos
library(maptools) # Auxilia nas ferramentas gráficas do ggplot
library(gpclib)

# Dados ----
# Mais informações: JuntaBasesEducacao.R
# Diretório
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Definindo pasta de dados 
sPasta_Dados <- paste(sDiretorio_Base, "Dados", sep = "/")

# Nome do arquivo
sArquivo_Dados <- "Dados_Educacao_Final.csv"

# Lendo a base
df <- read_csv(paste(sPasta_Dados, sArquivo_Dados, sep = "/"))

## Ativar permição para o funcionamento do maptools e broom em mapas
gpclibPermit()

## Puxando dados do site do IBGE
td <- tempdir()
tf <- tempfile()
url <- "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2020/Brasil/BR/BR_UF_2020.zip"
download.file(url , destfile = tf)

## Observar os arquivos dentro da pasta zipada
unzip(tf, list=TRUE)

## Separando cada arquivo na pasta zip
# Para esse grupo de arquivo em específico é necessário que puxe todos os arquivos
# para que a função shapefile() funcione, mas na maioria dos casos isso não é
# necessariamente verdade

## Unzipando os arquivos do diretório temporário 
unzip(tf, files=unzip(tf, list=TRUE)$Name[1], exdir=td, overwrite=TRUE)
unzip(tf, files=unzip(tf, list=TRUE)$Name[2], exdir=td, overwrite=TRUE)
unzip(tf, files=unzip(tf, list=TRUE)$Name[3], exdir=td, overwrite=TRUE)
unzip(tf, files=unzip(tf, list=TRUE)$Name[4], exdir=td, overwrite=TRUE)
unzip(tf, files=unzip(tf, list=TRUE)$Name[5], exdir=td, overwrite=TRUE)

## Importando os dados
# MAPA <- shapefile(paste0(td,"\\",unzip(tf, list=TRUE)$Name[4]))
MAPA <- shapefile(paste0(td,"/",unzip(tf, list=TRUE)$Name[4]))

## Unlinkando os arquivos temporários do R e removendo coisas inuteis
unlink(tf)
unlink(td)
rm(list = c("td", "tf", "url", "fpath"))

## Mudando formato do arquivo para tidy, para poder usar o ggplot 
mapa_tidy <- broom::tidy(MAPA, region = "SIGLA_UF")

## Média do IDEB por ano e UF
df.educ.ideb <- df %>%
    group_by(uf, ano) %>%
    summarise(mean(ideb, na.rm = T))

names(df.educ.ideb)[3] <- paste("ideb")


## Unindo as informações de interesse com a base dos gráficos
mapa_tidy %>%
    right_join(. , df.educ.ideb, by=c("id"="uf")) %>%
    filter(ano != 2009) %>%
    ggplot() +
    geom_polygon(aes(fill = ideb, x = long, y = lat, group = group), 
                 size=0, alpha=0.9) +
    geom_polygon(aes(x = long, y = lat, group = group), 
                 color="black", fill = NA) +
    scale_fill_viridis(trans = "log", name="IDEB",
                       guide = guide_legend(keyheight = unit(3, units = "mm"),
                                            keywidth=unit(12, units = "mm"),
                                            label.position = "bottom",
                                            title.position = 'top', nrow=1)) + 
    theme_void() +
    theme(legend.position = c(0.85, 0.09)) +
    coord_map() +
    facet_wrap(vars(ano))

## Salvando
ggsave("Mapa_IDEB.png", dpi=600)