# Setup e bibliotecas ----
# Dados
library(tidyverse)
library(purrr)
library(magrittr)
library(stringr)

# Gráficos
library(ggplot2)
library(cowplot)
library(ggstatsplot)
library(viridis)

## Comandos Úteis
# Apagar Variáveis
rm(list = ls())
# Apagar plots
dev.off()
# Limpar Console
cat("\014")

## Settando o diretório
# Optei por esse método porque vai funcionar em qualquer computador :)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sDiretorio_Base <- dirname(rstudioapi::getActiveDocumentContext()$path)
getwd()

# Lendo as bases ----
# Definindo pasta de dados 
sPasta_Dados <- paste(sDiretorio_Base, "Dados", sep = "/")

# Nome do arquivo
sArquivo_Dados <- "Dados_Educacao_Final.csv"

# Lendo a base
df <- read_csv(paste(sPasta_Dados, sArquivo_Dados, sep = "/"))

# Mudando nomes de rede e anos_escolares
df$anos_escolares <- ifelse(df$anos_escolares == "finais (6-9)",
                            "Fundamental II",
                            "Fundamental I"
                            )

df$rede <- str_to_title(df$rede)


# Correlações ----
## IDEB e Infraestrutura ----
### Todos os anos ----
# Correlação positiva, mas pequena
jmv::corrMatrix(df, vars = vars(ideb, est_agua, est_energia))

# Correlação maior, principalmente em água da rede pública
jmv::corrMatrix(df, vars = vars(ideb, est_agua_rede_publica, est_energia_rede_publica))

# Correlação muito grande!!
jmv::corrMatrix(df, vars = vars(ideb, tec_internet, tec_banda_larga))

### EF1 ----
# Dataset
dfEF1 <- df %>% 
  filter(anos_escolares == "Fundamental I")

# Correlação positiva, mas pequena (e maior que nos dois dados juntos)
jmv::corrMatrix(dfEF1, vars = vars(ideb, est_agua, est_energia))

# Correlação maior, principalmente em água da rede pública (maior que nos dois dados juntos)
jmv::corrMatrix(dfEF1, vars = vars(ideb, est_agua_rede_publica, est_energia_rede_publica))

# Correlação muito grande!! (maior que nos dois dados juntos)
jmv::corrMatrix(dfEF1, vars = vars(ideb, tec_internet, tec_banda_larga))

### EF2 ----
# Dataset
dfEF2 <- df %>% 
  filter(anos_escolares == "Fundamental II")

# Correlação positiva, mas pequena (e menor que nos dois dados juntos)
jmv::corrMatrix(dfEF2, vars = vars(ideb, est_agua, est_energia))

# Correlação maior, principalmente em água da rede pública (menor que nos dois dados juntos)
jmv::corrMatrix(dfEF2, vars = vars(ideb, est_agua_rede_publica, est_energia_rede_publica))

# Correlação muito grande!! (menor que nos dois dados juntos)
jmv::corrMatrix(dfEF2, vars = vars(ideb, tec_internet, tec_banda_larga))

# Médias de Infraestrutura por Ano ----
# Melhor usar _pub por conta da maior porcentagem de escolas sem
dfMedias <- df %>% 
  group_by(ano, anos_escolares) %>% 
  summarise(agua = mean(est_agua),
            agua_pub = mean(est_agua_rede_publica),
            energia = mean(est_energia),
            energia_pub = mean(est_energia_rede_publica),
            internet = mean(tec_internet),
            banda_larga = mean(tec_banda_larga))
dfMedias

# Gráficos ----
## IDEB ----
# Fundamental I > Fundamentlal II, mas movimentos similares ao longo do tempo
df %>% 
  group_by(ano, rede, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, colour = anos_escolares, group = anos_escolares)) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  theme_bw() + 
  guides(colour = guide_legend("Etapa"))

## Rendimento ----
# Fundamental I > II, mas taxa do EFII cresce mais rápido (sinal de manipulação?)
df %>% 
  group_by(ano, rede, anos_escolares) %>% 
  summarise(ideb = mean(rendimento_aprovacao_ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, colour = anos_escolares, group = anos_escolares)) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "Índice de Rendimento (%)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  theme_bw() + 
  guides(colour = guide_legend("Etapa"))

## SAEB ----
# Fundamental I > II, mas EFI cresce mais rápido
df %>% 
  group_by(ano, rede, anos_escolares) %>% 
  summarise(ideb = mean(saeb_ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, colour = anos_escolares, group = anos_escolares)) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "Índice de Aproveitamento (Saeb)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  theme_bw() + 
  guides(colour = guide_legend("Etapa"))

# Evolução Temporal ----
## Agua ----
# Diferença maior no EF1 e aumentando em ambos os casos
df %>% 
  group_by(ano, anos_escolares, est_agua) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_agua), 
             group = factor(est_agua))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3.5, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Água")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

## Agua Pública ----
# Diferença de desempenho consistente e similar à água, mas é maior no EF1
df %>% 
  group_by(ano, est_agua_rede_publica, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_agua_rede_publica), 
             group = factor(est_agua_rede_publica))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3.5, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Água Pública")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

## Energia - MELHOR + PAINEL ####
# Diferença MUITO grande, crescente e maior no EF1!!
df %>% 
  group_by(ano, est_energia, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_energia), 
             group = factor(est_energia))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Energia")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')


## Energia Rede Pública - MELHOR + PAINEL----
# Diferença MUITO grande, crescente e maior no EF1!!
df %>% 
  group_by(ano, est_energia_rede_publica, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_energia_rede_publica), 
             group = factor(est_energia_rede_publica))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Energia Pública")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')


## Tecnologia ----
### Internet ----
## Maior no EF1 e crescente no EF2
## Ideia: educação digital e painel?
df %>% 
  group_by(ano, tec_internet, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(tec_internet), 
             group = factor(tec_internet))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Internet")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

### Banda-Larga ----
# Meh
df %>% 
  group_by(ano, tec_banda_larga, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(tec_banda_larga), 
             group = factor(tec_banda_larga))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Banda Larga")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

## Esgoto ----
# Diferença maior no EF1 e consistente
df %>% 
  group_by(ano, est_esgoto, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_esgoto), 
             group = factor(est_esgoto))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Esgoto")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')


## Banheiro ----
# muito estranho... fiz merda?
df %>% 
  group_by(ano, est_banheiro, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_banheiro), 
             group = factor(est_banheiro))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Banheiro")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

## Regiões ----
# Sul ≈ Sudeste > CO > NE (passa norte recentemente) > N
df %>% 
  group_by(ano, regiao, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(regiao), 
             group = factor(regiao))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 6, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  theme_bw() + 
  guides(colour = guide_legend("Região")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

## Quadra, Lab., Biblio ----
# Estranho!
df %>% 
  group_by(ano, est_quadraesportes, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_quadraesportes), 
             group = factor(est_quadraesportes))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Quadra")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

# Diferença consistente e igual nas duas etapas
df %>% 
  group_by(ano, tec_lab_informatica, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(tec_lab_informatica), 
             group = factor(tec_lab_informatica))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Lab. Info.")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')

# Diferença consistente ao longo do tempo!
df %>% 
  group_by(ano, est_biblioteca, anos_escolares) %>% 
  summarise(ideb = mean(ideb)) %>% 
  ggplot(aes(x = factor(ano), y = ideb, 
             colour = factor(est_biblioteca), 
             group = factor(est_biblioteca))) +
  geom_point(size = 2.5) + 
  geom_line(lwd = 1.25) +
  labs(x = "Ano", y = "IDEB") +
  scale_y_continuous(breaks = seq(3, 5.5, by = .5),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() + 
  guides(colour = guide_legend("Biblioteca")) +
  facet_grid(cols = vars(anos_escolares), switch = 'y')


# Despesas ----
## X: Gasto / PIB_Mun: Negativa ----
### Outliers: Boxplots/Quantis ----
# Boxplot
df %>% 
  filter(despesa_educ_perc_pib_mun_def < 1) %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = ano, y = 100 * despesa_educ_perc_pib_mun_def, fill = ano)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "Despesa com Educação (R$ 2019) / PIB Municipal (R$ 2019) (%)") +
  theme_bw() +
  theme(legend.position = 'none')

# Quantis
## Deflacionado
df %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(despesa_educ_perc_pib_mun_def, c(.25, .5, .75, .9, .99, .999))) %>% 
  pivot_wider(names_from = quantil, values_from = gasto_pib, values_fill = 0)
  
## Não-deflacionado
df %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(despesa_educ_perc_pib_mun, c(.25, .5, .75, .9, .99, .999))) %>% 
  pivot_wider(names_from = quantil, values_from = gasto_pib, values_fill = 0)

### Energia Pública ----
# Valores deflacionados: relação negativa
df %>% 
  filter(despesa_educ_perc_pib_mun_def < .4 & despesa_educ_mun > 0) %>% 
  ggplot(aes(x = 100 * despesa_educ_perc_pib_mun_def,
             y = ideb,
             colour = factor(est_energia_rede_publica))) +
  geom_point(size = 2, alpha = .5) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  labs(color = str_wrap("Energia Pública", 8),
       x = "Despesa com Educação (R$ 2019) / PIB Municipal (R$ 2019) (%)",
       y = "IDEB da Escola")

# Valores correntes: relação negativa
df %>% 
  filter(despesa_educ_perc_pib_mun_def < .4 & despesa_educ_mun > 0) %>% 
  ggplot(aes(x = 100 * despesa_educ_perc_pib_mun,
             y = ideb,
             colour = factor(est_energia_rede_publica))) +
  geom_point(size = 2, alpha = .5) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  labs(color = str_wrap("Energia Pública", 8),
       x = "Despesa com Educação / PIB Municipal (%)",
       y = "IDEB da Escola") +
  facet_wrap(~ano, ncol = 2, nrow = 3, strip.position = 'top')

## X: Gasto_Aluno / PIB_Capita_Mun: ruim ----


## X: Proxy Despesa: Positiva/Neutra ----
### Outliers: Boxplots/Quantis ----
# Boxplot
df %>% 
  filter(despesa_matricula_pib_per_capita_mun_def < 1) %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = ano, y = 100 * proxy_despesa_escola_def, fill = ano)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "Despesa com Educação (R$ 2019) / PIB Municipal (R$ 2019) (%)") +
  theme_bw() +
  theme(legend.position = 'none')

# Quantis
## Deflacionado
# Anos
df %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.001, .01, .1, .25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(proxy_despesa_escola_def, 
                                 c(.001, .01, .1, .25, .5, .75, .9, .99, .999),
                                 na.rm = T)) %>% 
  pivot_wider(names_from = quantil, values_from = gasto_pib, values_fill = 0)

# Sem distinção de anos
df %>% 
  summarise(quantil = scales::percent(c(.001, .01, .1, .25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(proxy_despesa_escola_def, 
                                 c(.001, .01, .1, .25, .5, .75, .9, .99, .999),
                                 na.rm = T))

## Não-deflacionado
df %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(proxy_despesa_escola, 
                                 c(.25, .5, .75, .9, .99, .999),
                                 na.rm = T)) %>% 
  pivot_wider(names_from = quantil, values_from = gasto_pib, values_fill = 0)

### Energia Pública ----
# Valores deflacionados: relação neutra
df %>% 
  filter(despesa_educ_mun > 0 & proxy_despesa_escola_def > 40000 & proxy_despesa_escola_def < 20000000) %>% 
  ggplot(aes(x = proxy_despesa_escola_def,
             y = ideb,
             colour = factor(est_energia_rede_publica))) +
  geom_point(size = 2, alpha = .5) +
  scale_color_viridis(discrete = T) +
  scale_x_log10(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  labs(color = str_wrap("Energia Pública", 8),
       x = "Proxy da Despesa Escolar",
       y = "IDEB da Escola")

# Valores correntes: relação positiva em cada ano
df %>% 
  filter(despesa_educ_mun > 0 & proxy_despesa_escola_def > 40000 & proxy_despesa_escola_def < 20000000) %>% 
  ggplot(aes(x = proxy_despesa_escola_def,
             y = ideb,
             colour = factor(est_energia_rede_publica))) +
  geom_point(size = 2, alpha = .5) +
  scale_color_viridis(discrete = T) +
  scale_x_log10(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  labs(color = str_wrap("Energia Pública", 8),
       x = "Proxy da Despesa Escolar",
       y = "IDEB da Escola") +
  facet_wrap(~ano, ncol = 2, nrow = 3, strip.position = 'top')

### Valor gasto por aluno ----
## Distribuição
### Distinguindo anos
df %>% 
  group_by(ano) %>% 
  summarise(quantil = scales::percent(c(.001, .01, .1, .25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(despesa_por_matricula_mun, 
                                 c(.001, .01, .1, .25, .5, .75, .9, .99, .999),
                                 na.rm = T)) %>% 
  pivot_wider(names_from = ano, values_from = gasto_pib, values_fill = 0)


### Sem distinguir anos
df %>% 
  summarise(quantil = scales::percent(c(.001, .01, .1, .25, .5, .75, .9, .99, .999)),
            gasto_pib = quantile(despesa_por_matricula_mun_def, 
                                 c(.001, .01, .1, .25, .5, .75, .9, .99, .999),
                                 na.rm = T))

## Gráfico
df %>% 
  filter(despesa_educ_mun > 0 & 
         proxy_despesa_escola_def > 40000 & proxy_despesa_escola_def < 20000000 &
         despesa_por_matricula_mun_def < 23811) %>% 
  ggplot(aes(x = proxy_despesa_escola_def,
             y = ideb,
             colour = despesa_por_matricula_mun_def)) +
  geom_point(size = 2, alpha = .5) +
  scale_color_viridis(discrete = F) +
  scale_x_log10(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  labs(color = str_wrap("Gasto por Matrícula", 8),
       x = "Proxy da Despesa Escolar",
       y = "IDEB da Escola") +
  facet_wrap(~ano, ncol = 2, nrow = 3, strip.position = 'top')

## Gráfico com escalas de cores diferentes
# df %>% 
#   group_by(ano, .add = T) %>% 
#   filter(despesa_educ_mun > 0 & 
#          proxy_despesa_escola_def > 40000 & proxy_despesa_escola_def < 20000000 &
#          despesa_por_matricula_mun < quantile(despesa_por_matricula_mun, c(.9999), na.rm = T)) %>% 
#   group_split() %>% 
#   map(
#     ~ggplot(., aes(proxy_despesa_escola_def, ideb, color = despesa_por_matricula_mun)) + 
#       geom_point(size = 2, alpha = .5) +
#       scale_colour_viridis(discrete = F) +
#       scale_x_log10(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
#       theme_bw() + 
#       labs(color = str_wrap("Gasto por Matrícula", 9),
#            x = "Proxy da Despesa Escolar",
#            y = "IDEB da Escola") +
#       facet_grid(~ ano, labeller = function(x) label_value(x, multi_line = FALSE))
#   ) %>% 
#   plot_grid(plotlist = ., align = 'hv', ncol = 2)
# ggsave2("IDEB_Proxy_Despesa_Gasto_por_Matricula.pdf", dpi = 600)

# Boxplots e Densidades ----
## IDEB ----
## Boxplot
df %>% 
  ggplot(aes(x = anos_escolares, y = ideb, fill = anos_escolares)) +
  geom_boxplot() + 
  labs(x = "Etapa Escolar", y = "IDEB") +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(""))

## Densidade + Normal
# Normal
x <- seq(
  min(df$ideb),
  max(df$ideb),
  by = .1
)

# Estatísticas
nMediaIdeb <- round(mean(df$ideb), 2)
nSDIdeb <- sd(df$ideb)

# Gráfico
df %>% 
  ggplot(aes(x = ideb)) +
  stat_function(fun = dnorm, args = list(mean = nMediaIdeb, sd = nSDIdeb),
                aes(colour = "Normal")) +
  stat_density(geom="line", position="identity", aes(colour = "IDEB")) +
  labs(x = "IDEB", y = "Densidade") +
  scale_color_manual("", values = c("black", "brown2"), labels = c("IDEB", "Normal")) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() +
  theme(legend.position = 'top') 
ggsave("Normal.png", dpi=600)


## IDEB x Ano ----
# EF1 > EF2 e notas crescentes ao longo do tempo
## Boxplot
df %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = anos_escolares, y = ideb, fill = ano)) +
  geom_boxplot() + 
  labs(x = "Etapa Escolar", y = "IDEB") +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(""))

## Densidade
df %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = ideb, fill = ano, colour = ano)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend(""), fill = 'none')
  
## IDEB x Regiões x Ano ----
## Boxplot
df %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = regiao, y = ideb, fill = ano)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(""))

## Densidade
df %>% 
  mutate(ano = factor(ano)) %>% 
  ggplot(aes(x = ideb, fill = regiao, colour = regiao)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend(""), fill = 'none')

## IDEB x Infraestrutrua x Ano ----
### Agua ----
## Violin
df %>% 
  mutate(ano = factor(ano), est_agua = factor(est_agua)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_agua)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Água"))

## Boxplot
df %>% 
  mutate(ano = factor(ano), est_agua = factor(est_agua)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_agua)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Água"))

## Densidade
df %>% 
  mutate(ano = factor(ano), est_agua = factor(est_agua)) %>% 
  ggplot(aes(x = ideb, fill = est_agua, colour = est_agua)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend("Água"), fill = 'none')

### Agua Pública ----
## Violin
df %>% 
  mutate(ano = factor(ano), est_agua_rede_publica = factor(est_agua_rede_publica)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_agua_rede_publica)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Água Pública"))

## Boxplot
df %>% 
  mutate(ano = factor(ano), est_agua_rede_publica = factor(est_agua_rede_publica)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_agua_rede_publica)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Água Pública"))

## Densidade
df %>% 
  mutate(ano = factor(ano), est_agua_rede_publica = factor(est_agua_rede_publica)) %>% 
  ggplot(aes(x = ideb, fill = est_agua_rede_publica, colour = est_agua_rede_publica)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend("Água"), fill = 'none')

### Energia: MELHOR! ----
## Violin
df %>% 
  mutate(ano = factor(ano), est_energia = factor(est_energia)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_energia)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Energia"))

## Boxplot
df %>% 
  mutate(ano = factor(ano), est_energia = factor(est_energia)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_energia)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Energia"))

## Densidade
df %>% 
  mutate(ano = factor(ano), est_energia = factor(est_energia)) %>% 
  ggplot(aes(x = ideb, fill = est_energia, colour = est_energia)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend("Energia"), fill = 'none')

### Energia Publica ----
## Violin
df %>% 
  mutate(ano = factor(ano), est_energia_rede_publica = factor(est_energia_rede_publica)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_energia_rede_publica)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Energia Pública"))

## Boxplot
df %>% 
  mutate(ano = factor(ano), est_energia_rede_publica = factor(est_energia_rede_publica)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = est_energia_rede_publica)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Energia Pública"))

## Densidade
df %>% 
  mutate(ano = factor(ano), est_energia_rede_publica = factor(est_energia_rede_publica)) %>% 
  ggplot(aes(x = ideb, fill = est_energia_rede_publica, colour = est_energia_rede_publica)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend("Energia Pública"), fill = 'none')

### Internet ----
## Violin
df %>% 
  mutate(ano = factor(ano), tec_internet = factor(tec_internet)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = tec_internet)) +
  geom_violin(draw_quantiles = c(0.5)) + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Internet"))

## Boxplot
df %>% 
  mutate(ano = factor(ano), tec_internet = factor(tec_internet)) %>% 
  ggplot(aes(x = ano, y = ideb, fill = tec_internet)) +
  geom_boxplot() + 
  labs(x = "Ano", y = "IDEB") +
  scale_fill_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend("Internet"))

## Densidade
df %>% 
  mutate(ano = factor(ano), tec_internet = factor(tec_internet)) %>% 
  ggplot(aes(x = ideb, fill = tec_internet, colour = tec_internet)) +
  geom_density(alpha = .1, show.legend = F) + 
  stat_density(geom="line",position="identity") +
  labs(x = "IDEB", y = "Densidade") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_color_hue(labels = c("Não", "Sim")) +
  theme_bw() +
  theme(legend.position = 'top') +
  guides(colour = guide_legend("Internet"), fill = 'none')
