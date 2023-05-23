# TidySynth ====================================================================
mspe_placebos_pos_trat <- function(objeto_cs_tidy, ano_tratamento){
   # Pegando os dados reais e do controle sintético
   dfPlacebos <- objeto_cs_tidy %>% 
      grab_synthetic_control(placebo = TRUE) %>% 
      # Apenas placebos pós-tratamento
      filter((.placebo == 1) & (time_unit > ano_tratamento)) %>% 
      mutate(sq_pred_error = (real_y - synth_y)**2)
   
   ## Média
   return(mean(dfPlacebos$sq_pred_error))
}


graficos_cs <- function(objeto_cs_tidy, nome_variavel_y, ano_tratamento,
                        ano_inicial, ano_final, breaks_grafico,
                        escala_trends, escala_gaps,
                        paleta_trend, paleta_gap, paleta_pesos,
                        paleta_placebos, limite_mspe_placebos = 10,
                        hjust_trends = .85, hjust_gap = .85){
   
   # Gráfico de Tendências
   ## DataFrame
   dfTrends <- objeto_cs_tidy %>% 
      grab_synthetic_control() %>% 
      pivot_longer(cols = !c(time_unit)) 
   
   ## ggplot
   grafTrends <- dfTrends %>% 
      ggplot(aes(x = time_unit, y = value, 
                 colour = name, fill = name, linetype = name)) +
      geom_line(size = 1.25) + 
      geom_point(size = 1.75) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_trends, vjust = .5) + 
      labs(x = 'Ano', y = nome_variavel_y) +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      scale_y_continuous(limits = escala_trends[1:2]) +
      scale_colour_manual(name = "", values = paleta_trend, 
                          labels = c("Brasil", "Sintético")) +
      scale_fill_manual(name = "", values = paleta_trend, guide = "none") +
      scale_linetype_manual(name = "", values = c('solid', 'dashed'), guide = "none") +
      theme_bw() + 
      theme(
         legend.position = 'top',
         legend.title = element_blank(),
         legend.background = element_rect(fill = NA),
         plot.caption = element_text(hjust = 0),
         legend.text = element_text(size = 14)
      )
   
   # Gráficos de Pesos
   ## DataFrame
   dfPesos <- rbind(
      objeto_cs_tidy %>% 
         grab_predictor_weights() %>% 
         rename(name = variable) %>% 
         mutate(tipo = "Variáveis", weight = round(weight, 2)),
      objeto_cs_tidy %>% grab_unit_weights() %>% 
         rename(name = unit) %>% 
         mutate(tipo = "Unidades", weight = round(weight, 2))
   )
   
   ## Gráfico
   grafPesos <- dfPesos %>% 
      ggplot(aes(x = reorder(name, weight), y = weight, fill = tipo)) +
      geom_bar(stat = "identity", colour = "black") + 
      scale_fill_manual(name = "", 
                        values = paleta_pesos,
                        guide = "none") +
      labs(x = '', y = 'Peso') + 
      theme_bw() +
      coord_flip() + 
      facet_wrap(vars(tipo), scales = "free")
   
   # Gap Plot
   ## DataFrame
   dfGaps <- objeto_cs_tidy %>% 
      grab_synthetic_control(placebo = T) %>% 
      mutate(gap = real_y - synth_y) %>% 
      pivot_longer(cols = !c(.id, time_unit, .placebo))  %>% 
      filter(name == "gap") %>% 
      mutate(gap_sq = value**2,
             pre_trat = ifelse(time_unit <= ano_tratamento, 1, 0)) %>% 
      group_by(.id) %>% 
      mutate(mspe = sum(gap_sq * pre_trat) / sum(pre_trat)) %>% 
      ungroup() %>% 
      mutate(mspe_br = mean(ifelse(.id == "BR", mspe, NA), na.rm = T))
   
   ## ggplot
   grafGap <- dfGaps %>% 
      filter(.placebo == 0) %>% 
      ggplot(aes(x = time_unit, y = value, colour = name, fill = name)) +
      geom_line(size = 1.25) + 
      geom_ribbon(aes(ymin = 0, ymax = value), 
                  linetype = 0, alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = 'dotted', 
                 size = .5, colour = 'black', show.legend = F) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_gap, vjust = .5) +  
      labs(x = 'Ano', y = "Brasil - Sintético") +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      scale_y_continuous(limits = escala_gaps[1:2]) +
      scale_colour_manual(name = "", values = paleta_gap[1],
                          labels = c("Diferença: Brasil e Sintético")) +
      scale_fill_manual(name = "", values = paleta_gap[2],
                        labels = c("Diferença: Brasil e Sintético")) +
      theme_bw() + 
      theme(
         legend.position = 'top',
         legend.title = element_blank(),
         legend.background = element_rect(fill = NA),
         plot.caption = element_text(hjust = 0),
         legend.text = element_text(size = 14)
      )
   
   # Placebos
   grafPlacebos <- dfGaps %>% 
      filter((.placebo == 1) & (mspe <= limite_mspe_placebos*mspe_br)) %>% 
      ggplot(aes(x = time_unit, y = value, group = .id)) +
      geom_line(size = .75, colour = paleta_placebos[1]) + 
      geom_line(
         data = dfGaps %>% filter(.placebo == 0),
         size = 1.5, colour = paleta_placebos[2]
      ) + 
      geom_hline(yintercept = 0, linetype = 'dotted', 
                 size = .5, colour = 'black', show.legend = F) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_gap, vjust = .5) + 
      labs(x = 'Ano', y = "Real - Sintético") +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      theme_bw() 
   
   # Razões MSPE
   ## DataFrame
   dfMSPE <- objeto_cs_tidy %>% 
      grab_signficance() %>% 
      as.data.frame()
   
   ## ggplot
   grafRazaoMSPE <- dfMSPE %>% 
      ggplot(aes(x = reorder(unit_name, mspe_ratio), y = mspe_ratio, fill = type)) +
      geom_bar(stat = "identity", colour = "black") + 
      scale_fill_manual(name = "", values = paleta_placebos, guide = "none") +
      labs(x = 'Razão MSPE', y = '') + 
      theme_bw() +
      coord_flip()
   
   # Retornando
   lista_graficos <- list("trends" = grafTrends,
                          "pesos" = grafPesos,
                          "gap" = grafGap,
                          "placebos" = grafPlacebos,
                          "razao_mspe" = grafRazaoMSPE)
   return(lista_graficos)
}


tabela_media_cs <- function(objeto_cs_tidy, stargazer = TRUE){
   
   # Pegando a tabela de médias
   tabMedias <- objeto_cs_tidy %>% 
      grab_balance_table() %>% 
      as.data.frame() %>% 
      mutate_at(vars(!c("variable")), ~round(. , 2))
   
   # Nomes de colunas
   names(tabMedias) <- c("Variável", "Brasil", "Sintético", "Amostra de Doadores")

   # Juntando com tabela de pesos
   ## Pesos
   tabPesos <- objeto_cs_tidy %>% grab_predictor_weights()
   
   ## Juntando e ordenando por peso
   tabMedias %<>% 
      left_join(tabPesos, by = c("Variável" = "variable")) %>% 
      arrange(desc(weight)) %>% 
      mutate(weight = paste0(100*round(weight, 3), "%")) %>% 
      rename(Peso = weight) %>% 
      relocate(Peso, .after = "Variável")
   
   # Arrumando nomes das linhas
   rownames(tabMedias) <- tabMedias$Variável
   
   # Stargazer e retornando
   if(stargazer){
      stargazer(tabMedias[, -1], summary = F, decimal.mark = ",", digit.separator = ".")
      return(tabMedias[, -1])
   } else {
      return(tabMedias[, -1])
   }
}

# scpi =========================================================================
mspe_pre_trat_scpi <- function(objeto_cs_scpi){
   # Pegando diferença entre o fit e o observado pré
   erro <- objeto_cs_scpi$est.results$res
   
   ## Erro quadratico
   erro_quad <- erro^2
   
   ## MSPE
   mspe <- round(mean(erro_quad), 2)
   
   return(mspe)
}

placebos_scpi <- function(dados, string_coluna_unidade, string_coluna_tempo,
                          string_variavel_interesse,
                          ano_tratamento, ano_inicial, ano_final,
                          unidade_tratada = "BR", restricao_w, 
                          constant = FALSE, cointegrated = FALSE){
   # Pegando lista de países
   lista_paises <- unique(dados[[string_coluna_unidade]])
   
   # Loopando
   n <- 0
   for (pais in lista_paises) {
      # Atualizando contagem
      n <- n + 1
      
      # Preparando dados
      dados_scpi <- scdata(
         dados, 
         id.var = string_coluna_unidade, time.var = string_coluna_tempo,
         outcome.var = string_variavel_interesse, 
         period.pre = (ano_inicial:ano_tratamento),
         period.post = ((ano_tratamento + 1):ano_final), 
         unit.tr = pais, unit.co = lista_paises[-n],
         constant = constant, cointegrated.data = cointegrated
      )
      
      # Estimando
      cs <- scest(dados_scpi, w.constr = restricao_w)
      
      # Guardando dados
      df_est <- tibble(
         real_y = c(cs$data$Y.pre, cs$data$Y.post), 
         synth_y = c(cs$est.results$Y.pre.fit, cs$est.results$Y.post.fit),
         gap = real_y - synth_y,
         unit = pais,
         time_unit = c(ano_inicial:ano_final)
      )
      
      # Salvando
      if (n == 1){
         df_placebos <- df_est
      } else {
         df_placebos <- rbind(df_placebos, df_est)
      }
   }
   
   # Colunas auxiliares
   df_placebos %<>% 
      mutate(placebo = ifelse(unit == unidade_tratada, 0, 1),
             sq_pred_error = (real_y - synth_y)**2,
             pre_trat = ifelse(time_unit <= ano_tratamento, 1, 0))
   
   # Retornando
   return(df_placebos)
}

razao_mspe_scpi <- function(df_placebos, unit_treated = "BR"){
   # DataFrame
   df_razao <- df_placebos %>% 
      group_by(unit, pre_trat) %>% 
      summarise(mspe = mean(sq_pred_error)) %>% 
      pivot_wider(names_from = pre_trat, values_from = mspe) %>% 
      rename(post_mspe = "0", pre_mspe = "1") %>% 
      mutate(mspe_ratio = post_mspe / pre_mspe,
             type = ifelse(unit == unit_treated, "treated", "donor")) %>% 
      arrange(desc(mspe_ratio))
   
   # Retornando
   return(df_razao)
}


graficos_sc_scpi <- function(objeto_sc_scpi, df_placebos, df_razao, 
                             nome_variavel_y, ano_tratamento,
                             ano_inicial, ano_final, breaks_grafico,
                             paleta_trend, paleta_gap, paleta_pesos,
                             paleta_placebos, limite_mspe_placebos = 10,
                             hjust_trends = .85, hjust_gap = .85) {
   # Pesos
   ## Dados
   dfPesos <- as.data.frame(objeto_sc_scpi$est.results$w)
   dfPesos$name <- substring(rownames(dfPesos), 4)
   dfPesos$weight <- dfPesos[, 1]
   
   ## ggplot
   grafPesos <- dfPesos %>% 
      ggplot(aes(x = reorder(name, weight), y = weight)) +
      geom_bar(stat = "identity", colour = "black", fill = paleta_pesos[1]) + 
      labs(x = '', y = 'Peso') + 
      theme_bw() +
      coord_flip() 
   
   
   # Tendências
   ## Dados
   df_placebos %<>% 
      pivot_longer(cols = c(real_y, synth_y, gap))
   
   dfPlot <- df_placebos %>% 
      filter(placebo == 0)
   
   ## ggplot
   grafTrends <- dfPlot %>% 
      filter(name != "gap") %>% 
      ggplot(aes(x = time_unit, y = value, 
                 colour = name, fill = name, linetype = name)) +
      geom_line(size = 1.25) + 
      geom_point(size = 1.75) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_trends, vjust = .5) +  
      labs(x = 'Ano', y = nome_variavel_y) +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      scale_colour_manual(name = "", values = paleta_trend, 
                          labels = c("Brasil", "Sintético")) +
      scale_fill_manual(name = "", values = paleta_trend, guide = "none") +
      scale_linetype_manual(name = "", values = c('solid', 'dashed'), guide = "none") +
      theme_bw() + 
      theme(
         legend.position = 'top',
         legend.title = element_blank(),
         legend.background = element_rect(fill = NA),
         plot.caption = element_text(hjust = 0),
         legend.text = element_text(size = 14)
      )
   
   # Gaps
   grafGap <- dfPlot %>% 
      filter(name == "gap") %>% 
      ggplot(aes(x = time_unit, y = value, colour = name, fill = name)) +
      geom_line(size = 1.25) + 
      geom_ribbon(aes(ymin = 0, ymax = value), 
                  linetype=0, show.legend=F, alpha=0.5) +
      geom_hline(yintercept = 0, linetype = 'dotted', 
                 size = .5, colour = 'black', show.legend = F) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_gap, vjust = .5) +  
      labs(x = 'Ano', y = "Brasil - Sintético") +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      scale_colour_manual(name = "", values = paleta_gap[1], guide = "none") +
      scale_fill_manual(name = "", values = paleta_gap[2], guide = "none") +
      theme_bw() 
   
   # Placebos
   ## Calculando MSPEs pré-tratamento
   df_placebos %<>% 
      group_by(unit) %>% 
      mutate(mspe = sum(sq_pred_error * pre_trat) / sum(pre_trat)) %>% 
      ungroup() %>% 
      mutate(mspe_br = mean(ifelse(unit == "BR", mspe, NA), na.rm = T))
   
   grafPlacebos <- df_placebos %>% 
      filter((name == "gap") & (mspe < limite_mspe_placebos * mspe_br)) %>% 
      ggplot(aes(x = time_unit, y = value, group = unit)) +
      geom_line(size = .75, colour = paleta_placebos[1]) + 
      geom_line(
         data = df_placebos %>% filter((name == "gap") & (placebo == 0)),
         size = 1.5, colour = paleta_placebos[2]
      ) + 
      geom_hline(yintercept = 0, linetype = 'dotted', 
                 size = .5, colour = 'black', show.legend = F) + 
      geom_textvline(xintercept = ano_tratamento, linetype = 'dotted', 
                     colour = 'black', size = 3,
                     label = "Aprovação do Teto", hjust = hjust_gap, vjust = .5) +  
      labs(x = 'Ano', y = "Real - Sintético") +
      scale_x_continuous(breaks = seq(ano_inicial, ano_final, by = breaks_grafico)) +
      theme_bw() 
   
   
   # Razão MSPE
   grafRazaoMSPE <- df_razao %>% 
      ggplot(aes(x = reorder(unit, mspe_ratio), y = mspe_ratio, fill = type)) +
      geom_bar(stat = "identity", colour = "black") + 
      scale_fill_manual(name = "", values = paleta_placebos, guide = "none") +
      labs(x = 'Razão MSPE', y = '') + 
      theme_bw() +
      coord_flip()
   
   # Retornando
   lista_graficos <- list("trends" = grafTrends,
                          "pesos" = grafPesos,
                          "gap" = grafGap,
                          "placebos" = grafPlacebos,
                          "razao_mspe" = grafRazaoMSPE)
   return(lista_graficos)
}
