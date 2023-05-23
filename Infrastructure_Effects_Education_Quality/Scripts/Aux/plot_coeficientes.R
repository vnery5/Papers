#' Plotta os coeficientes e seus intervalos de confiança a partir da matriz de covariância especificada
#' Criador: Vinícius de Almeida Nery Ferreira
#' 
#' @param lModelos Lista (list()) com os objetos dos modelos
#' @param lMatrizCovariancias Lista (list()) com as matrizes de covariância
#' @param vNomesGrafico Vetor com os nomes dos modelos a serem usados como legenda no gráfico
#' @param vVariaveisGrafico Vetor contendo os nomes das variáveis a serem plottadas; default: todas
#' @param nNivelConfianca Número (0 < 1) contendo o intervalo de confiança a ser calculado
#' @return Objeto do ggplot que pode ser customizado
#' @examples 
#' Ver parte de baixo do script com a função

plot_coeficientes <- function(lModelos, 
                              lMatrizCovariancias, 
                              vNomesGrafico,
                              vVariaveisGrafico = names(lModelos[[1]]$coefficients),
                              nNivelConfianca = 0.95) {
    
    ## Pegando os coeficientes e intervalos de confiança do 1º modelo
    dfPlot <- as.data.frame(cbind(lModelos[[1]]$coefficients,
                                  stats::confint(lmtest::coeftest(lModelos[[1]], lMatrizCovariancias[[1]]), 
                                                 level = nNivelConfianca)
                                  )
                            )
    
    ## Criando nomes das colunas
    colnames(dfPlot) <- c("Coef", "LowCI", "HighCI")
    
    ## Adicionando colunas do modelo/nomes das variáveis
    dfPlot$Modelo <- vNomesGrafico[1]
    dfPlot$Variaveis <- rownames(dfPlot)
    
    ## Mesma coisa para os demais
    for (nModelo in 2:length(vNomesGrafico)){
        dfAux <- as.data.frame(cbind(lModelos[[nModelo]]$coefficients,
                                     stats::confint(lmtest::coeftest(lModelos[[nModelo]], 
                                                                     lMatrizCovariancias[[nModelo]]),
                                                    level = nNivelConfianca)
                                     )
                               )
        colnames(dfAux) <- c("Coef", "LowCI", "HighCI")
        dfAux$Modelo <- vNomesGrafico[nModelo]
        dfAux$Variaveis <- rownames(dfAux)
        
        ## Juntando
        dfPlot <- rbind(dfPlot, dfAux)
    }
    
    ## Vendo se apenas algumas variáveis serão plottadas
    dfPlot <- dfPlot %>% 
        filter(Variaveis %in% vVariaveisGrafico)
    
    ## Plottando
    dfPlot %>% 
        ggplot(aes(x = Variaveis, y = Coef, colour = Modelo)) +
        geom_point(position = position_dodge(width=0.75)) +
        geom_errorbar(aes(ymin=LowCI, ymax=HighCI), position = position_dodge(width=0.75)) +
        geom_hline(yintercept = 0, linetype = 'longdash', size = .5, colour = 'black', show.legend = F) + 
        labs(x = "Variáveis", y = "Coeficientes e Intervalos de Confiança") +
        theme_bw()
}

# ## Dois modelos diferentes (atribuir a um objeto para customizar ggplot posteriormente)
# plot_coeficientes(lModelos = list(reg.FE4thX, reg.RE4thX),
#                   lMatrizCovariancias = list(reg.FE4thX$robse, reg.RE4thX$robse),
#                   vNomesGrafico = c("FE", "RE"),
#                   vVariaveisGrafico = c("lrexpp", "lunch"))
# 
# ## Todos os modelos
# plot_coeficientes(lModelos = list(reg.POLS4thX, reg.FD4thX, reg.FE4thX, reg.RE4thX),
#                   lMatrizCovariancias = list(reg.POLS4thX$robse, reg.FD4thX$robse, reg.FE4thX$robse, reg.RE4thX$robse),
#                   vNomesGrafico = c("POLS", "FD", "FE", "RE"),
#                   vVariaveisGrafico = c("lrexpp", "lunch"))
# 
# ## Um mesmo modelo com duas matriz de covariâncias
# plot_coeficientes(lModelos = list(reg.FE4thX, reg.FE4thX),
#                   lMatrizCovariancias = list(reg.FE4thX$vcov, reg.FE4thX$robse),
#                   vNomesGrafico = c("FE Normal", "FE Robusto"),
#                   vVariaveisGrafico = c("lrexpp", "lunch"))
# 
# plot_coeficientes(lModelos = list(reg.RE4thX, reg.RE4thX),
#                   lMatrizCovariancias = list(reg.RE4thX$vcov, reg.RE4thX$robse),
#                   vNomesGrafico = c("RE Normal", "RE Robusto"),
#                   vVariaveisGrafico = c("lrexpp", "lunch"))
