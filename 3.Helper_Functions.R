# Helper Functions

# Função para interpretar o resultado do teste ADF -----------------------------
############################################################################################
# This R function helps to interpret the output of the urca::ur.df function.
# The rules are based on https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results
#
# urdf is the output of the urca::ur.df function
# level is one of c("1pct", "5pct", "10pct")
#
# Author: Hank Roark
# Date: October 2019
############################################################################################
interp_urdf <- function(urdf, level="5pct") {
  if(class(urdf) != "ur.df") stop('parameter is not of class ur.df from urca package')
  if(!(level %in% c("1pct", "5pct", "10pct") ) ) stop('parameter level is not one of 1pct, 5pct, or 10pct')
  
  cat("========================================================================\n")
  cat( paste("At the", level, "level:\n") )
  if(urdf@model == "none") {
    cat("The model is of type none\n")
    tau1_crit = urdf@cval["tau1",level]
    tau1_teststat = urdf@teststat["statistic","tau1"]
    tau1_teststat_wi_crit = tau1_teststat > tau1_crit
    if(tau1_teststat_wi_crit) {
      cat("tau1: The null hypothesis is not rejected, unit root is present\n")
    } else {
      cat("tau1: The null hypothesis is rejected, unit root is not present\n")
    }
  } else if(urdf@model == "drift") {
    cat("The model is of type drift\n")
    tau2_crit = urdf@cval["tau2",level]
    tau2_teststat = urdf@teststat["statistic","tau2"]
    tau2_teststat_wi_crit = tau2_teststat > tau2_crit
    phi1_crit = urdf@cval["phi1",level]
    phi1_teststat = urdf@teststat["statistic","phi1"]
    phi1_teststat_wi_crit = phi1_teststat < phi1_crit
    if(tau2_teststat_wi_crit) {
      # Unit root present branch
      cat("tau2: The first null hypothesis is not rejected, unit root is present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is drift.\n")
      }
    } else {
      # Unit root not present branch
      cat("tau2: The first null hypothesis is rejected, unit root is not present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
        warning("This is inconsistent with the first null hypothesis.")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there is drift.\n")
      }
    }
  } else if(urdf@model == "trend") {
    cat("The model is of type trend\n")
    tau3_crit = urdf@cval["tau3",level]
    tau3_teststat = urdf@teststat["statistic","tau3"]
    tau3_teststat_wi_crit = tau3_teststat > tau3_crit
    phi2_crit = urdf@cval["phi2",level]
    phi2_teststat = urdf@teststat["statistic","phi2"]
    phi2_teststat_wi_crit = phi2_teststat < phi2_crit
    phi3_crit = urdf@cval["phi3",level]
    phi3_teststat = urdf@teststat["statistic","phi3"]
    phi3_teststat_wi_crit = phi3_teststat < phi3_crit
    if(tau3_teststat_wi_crit) {
      # First null hypothesis is not rejected, Unit root present branch
      cat("tau3: The first null hypothesis is not rejected, unit root is present\n")
      if(phi3_teststat_wi_crit) {
        # Second null hypothesis is not rejected
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is no trend, and there is drift\n")
        }
      }
      else {
        # Second null hypothesis is rejected
        cat("phi3: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is trend, and there may or may not be drift\n")
          warning("Presence of drift is inconclusive.")
        }
      }
    } else {
      # First null hypothesis is rejected, Unit root not present branch
      cat("tau3: The first null hypothesis is rejected, unit root is not present\n")
      if(phi3_teststat_wi_crit) {
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        warning("This is inconsistent with the first null hypothesis.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there is no trend, and there is drift\n")
        }
      } else {
        cat("phi3: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there may or may not be trend\n")
        warning("Presence of trend is inconclusive.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first and second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there may or may not be trend, and there may or may not be drift\n")
          warning("Presence of trend and drift is inconclusive.")
        }
      }
    }
  } else warning('urdf model type is not one of none, drift, or trend')
  cat("========================================================================\n")
}

summary_urdf <- function(y, y_name){
  
  ## Passo (1): Testar ADF usando modelo com constante e com tendência:
  
  adf.ct <- urca::ur.df(y, type="trend", selectlags = "AIC")
  
  ## Passo (2): Testar ADF usando modelo só com constante.
  
  adf.c <- urca::ur.df(y, type="drift", selectlags = "AIC")
  
  ## Passo (3): Testar ADF usando modelo sem constante e sem tendência linear:
  
  adf <- urca::ur.df(y, type="none", selectlags = "AIC")
  
  ## DF para o passo (1)
  
  adf.ct_df <- data.frame(
    "variable" = c(rep(y_name,3)),
    "model" = c(rep(adf.ct@model,3)),
    "lags" = c(rep(adf.ct@lags,3)),
    "statistic" = c("tau3", "phi2", "phi3"),
    "test-statistic" = c(adf.ct@teststat[1], adf.ct@teststat[2], adf.ct@teststat[3]),
    "cv_1pct" = c(adf.ct@cval[1,1], adf.ct@cval[2,1], adf.ct@cval[3,1]),
    "cv_5pct" = c(adf.ct@cval[1,2], adf.ct@cval[2,2], adf.ct@cval[3,2]),
    "cv_10pct" = c(adf.ct@cval[1,3], adf.ct@cval[2,3], adf.ct@cval[3,3]),
    row.names = NULL
  )
  
  ## DF para o passo (2)
  
  adf.c_df <- data.frame(
    "variable" = c(rep(y_name,2)),
    "model" = c(rep(adf.c@model,2)),
    "lags" = c(rep(adf.c@lags,2)),
    "statistic" = c("tau2", "phi1"),
    "test-statistic" = c(adf.c@teststat[1], adf.c@teststat[2]),
    "cv_1pct" = c(adf.c@cval[1,1], adf.c@cval[2,1]),
    "cv_5pct" = c(adf.c@cval[1,2], adf.c@cval[2,2]),
    "cv_10pct" = c(adf.c@cval[1,3], adf.c@cval[2,3]),
    row.names = NULL
  )
  
  ## DF para o passo (3)
  
  adf_df <- data.frame(
    "variable" = c(rep(y_name,1)),
    "model" = c(rep(adf@model,1)),
    "lags" = c(rep(adf@lags,1)),
    "statistic" = c("tau1"),
    "test-statistic" = c(adf@teststat[1]),
    "cv_1pct" = c(adf@cval[1,1]),
    "cv_5pct" = c(adf@cval[1,2]),
    "cv_10pct" = c(adf@cval[1,3]),
    row.names = NULL
  )
  
  ## Juntando os DFs dos passos 1 a 3
  df_adf <- rbind(adf.ct_df, adf.c_df, adf_df)
  
  # objetos de saída
  
  sum_adf <- list(
    trend_adf = adf.ct,
    drift_adf = adf.c,
    none_adf = adf,
    sum_adf = df_adf
  )
  
  return(sum_adf)
}

# orthog = FALSE gives GIRF.

psGIRF <- function(x, n.ahead = 20, cumulative = TRUE, orthog = FALSE){
  
  y.names <- colnames(x$y)
  impulse <- y.names
  response <- y.names
  
  # Ensure n.ahead is an integer
  n.ahead <- abs(as.integer(n.ahead))
  
  # Create arrays to hold calculations     
  # [1:nlags, 1:nvariables, shocked variable ] 
  
  IRF_o  = array(data = 0, dim = c(n.ahead,x$K,x$K),
                 dimnames = list(NULL,y.names,y.names))       
  IRF_g  = array(data = 0, dim = c(n.ahead,x$K,x$K),
                 dimnames = list(NULL,y.names,y.names))
  IRF_g1 = array(data = 0, dim = c(n.ahead,x$K,x$K))
  
  # Estimation of orthogonalised and generalised IRFs
  SpecMA <- Phi(x, n.ahead)
  params <- ncol(x$datamat[, -c(1:x$K)])
  sigma.u <- crossprod(resid(x))/(x$obs - params)
  P <- t(chol(sigma.u))
  sig_jj <- diag(sigma.u)
  
  for (jj in 1:x$K){
    indx_      <- matrix(0,x$K,1)
    indx_[jj,1] <- 1
    
    for (kk in 1:n.ahead){  #kk counts the lag
      
      IRF_o[kk, ,jj] <- SpecMA[, ,kk]%*%P%*%indx_  # Peseran-Shin eqn 7 (OIRF)
      
      IRF_g1[kk, ,jj] <- SpecMA[, ,kk]%*%sigma.u%*%indx_
      IRF_g[kk, ,jj] <- sig_jj[jj]^(-0.5)*IRF_g1[kk, ,jj]  # Peseran-Shin eqn 10 (GIRF)
      
    }
  }
  
  if(orthog==TRUE){
    irf <- IRF_o
  } else if(orthog==FALSE) {
    irf <- IRF_g
  } else {
    stop("\nError! Orthogonalised or generalised IRF?\n")
  }
  
  idx <- length(impulse)
  irs <- list()
  for (ii in 1:idx) {
    irs[[ii]] <- matrix(irf[1:(n.ahead), response, impulse[ii]], nrow = n.ahead)
    colnames(irs[[ii]]) <- response
    if (cumulative) {
      if (length(response) > 1) 
        irs[[ii]] <- apply(irs[[ii]], 2, cumsum)
      if (length(response) == 1) {
        tmp <- matrix(cumsum(irs[[ii]]))
        colnames(tmp) <- response
        irs[[ii]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  result <- irs
  return(result)
  
}

# Função para realizar o Bootstrap e gerar Intervalos de Confiança para a GIRF
boot_girf_var <- function(model, n.ahead = 10, runs = 100, conf = 0.95) {
  
  # Extrair dados básicos do modelo original
  data_orig <- model$y
  p <- model$p
  type <- model$type
  obs <- nrow(data_orig)
  n_vars <- ncol(data_orig)
  resids <- residuals(model)
  coefs <- Bcoef(model) # Coeficientes do modelo
  
  # Lista para armazenar as GIRFs de cada rodada
  boot_results <- list()
  
  pb <- txtProgressBar(min = 0, max = runs, style = 3)
  
  for(i in 1:runs) {
    # 1. Reamostragem dos resíduos (Bootstrap não-paramétrico)
    # Sorteamos linhas dos resíduos originais com reposição
    resids_boot <- resids[sample(nrow(resids), replace = TRUE), ]
    
    # 2. Reconstrução da Série Temporal (Recursão do VAR)
    # Começamos com os valores iniciais (lags) do dado original
    data_sim <- matrix(0, nrow = obs, ncol = n_vars)
    data_sim[1:p, ] <- data_orig[1:p, ]
    
    # Geramos os novos valores baseados na estrutura do VAR + erro sorteado
    for(t in (p + 1):obs) {
      # Extrair lags atuais
      lags <- as.vector(t(data_sim[(t-1):(t-p), ]))
      
      # Adicionar termo determinístico (constante, trend, etc se houver)
      if(type == "const") {
        lags <- c(lags, 1)
      } else if(type == "trend") {
        lags <- c(lags, t)
      } else if(type == "both") {
        lags <- c(lags, 1, t)
      }
      
      # Calcular valor previsto + resíduo do bootstrap
      data_sim[t, ] <- coefs %*% lags + resids_boot[t - p, ]
    }
    
    colnames(data_sim) <- colnames(data_orig)
    
    # 3. Ajustar o modelo VAR aos dados simulados
    # Usamos tryCatch para evitar que o loop pare se um modelo simulado for instável
    model_boot <- try(VAR(data_sim, p = p, type = type), silent = TRUE)
    
    if(!inherits(model_boot, "try-error")) {
      # 4. Calcular a GIRF usando a função do Watkins
      # Certifique-se que a função psGIRF está carregada
      boot_results[[length(boot_results) + 1]] <- psGIRF(model_boot, 
                                                         n.ahead = n.ahead, 
                                                         cumulative = FALSE)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(boot_results)
}

# Função para realizar o Bootstrap e gerar Intervalos de Confiança para a GIRF

boot_girf_vec <- function(model, n.ahead = 10, runs = 100, conf = 0.95) {
  
  # 1. Extração manual de coeficientes para contornar a limitação do Bcoef()
  if (inherits(model, "vec2var")) {
    # Para vec2var: Combina as matrizes A (lags) e as determinísticas
    A_matrices <- do.call(cbind, model$A)
    det_matrices <- if(!is.null(model$deterministic)) model$deterministic else NULL
    coefs <- cbind(A_matrices, det_matrices)
  } else if (inherits(model, "varest")) {
    coefs <- Bcoef(model)
  } else {
    stop("O modelo deve ser da classe 'varest' ou 'vec2var'.")
  }
  
  data_orig <- model$y
  p <- model$p
  n_vars <- ncol(data_orig)
  obs <- nrow(data_orig)
  resids <- residuals(model)
  
  boot_results <- list()
  pb <- txtProgressBar(min = 0, max = runs, style = 3)
  
  for(i in 1:runs) {
    # 2. Bootstrap dos resíduos
    resids_boot <- resids[sample(nrow(resids), replace = TRUE), ]
    
    # 3. Reconstrução da Série (Recursão)
    data_sim <- matrix(0, nrow = obs, ncol = n_vars)
    data_sim[1:p, ] <- data_orig[1:p, ]
    
    for(t in (p + 1):obs) {
      # Vetor de lags: [y_{t-1}, y_{t-2}, ..., y_{t-p}]
      lags <- as.vector(t(data_sim[(t-1):(t-p), ]))
      
      # Adicionar 1 se houver termo constante no modelo original
      if(ncol(coefs) > (p * n_vars)) {
        lags <- c(lags, 1) 
      }
      
      # Cálculo do novo ponto (Garantindo que a multiplicação bata com o nº de colunas)
      data_sim[t, ] <- coefs[, 1:length(lags)] %*% lags + resids_boot[t - p, ]
    }
    
    colnames(data_sim) <- colnames(data_orig)
    
    # 4. Re-estimação e Cálculo da GIRF
    # Estimamos como VAR para facilitar a computação da GIRF
    model_boot <- try(VAR(data_sim, p = p, type = "const"), silent = TRUE)
    
    if(!inherits(model_boot, "try-error")) {
      boot_results[[length(boot_results) + 1]] <- psGIRF(model_boot, 
                                                         n.ahead = n.ahead,
                                                         cumulative = FALSE)
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(boot_results)
}

# # FUN chart VERSÃO COM PLOT BASE DO R
# plot_girf_mc <- function(boot_results, shock_idx = 1, response_idx = 2, conf = 0.95) {
#   
#   # 1. Extração da trajetória de cada simulação
#   # Garantimos que estamos pegando a coluna correta e transformando em matriz
#   dist_values <- sapply(boot_results, function(x) {
#     # x[[shock_idx]] é a matriz de resposta para o choque dado
#     as.numeric(x[[shock_idx]][, response_idx])
#   })
#   
#   # 2. Cálculo dos Quantis
#   lower_q <- (1 - conf) / 2
#   upper_q <- 1 - lower_q
#   
#   stats <- apply(dist_values, 1, function(x) {
#     quantile(x, probs = c(lower_q, 0.5, upper_q), na.rm = TRUE)
#   })
#   
#   # 3. Ajuste do Horizonte (Eixo X)
#   # O número de linhas em 'stats' deve ser igual ao comprimento do horizonte
#   n_steps <- nrow(dist_values)
#   horizon <- 0:(n_steps - 1)
#   
#   # 4. Resgate de nomes para o gráfico
#   var_names <- colnames(boot_results[[1]][[1]])
#   shock_name <- names(boot_results[[1]])[shock_idx]
#   resp_name <- if(!is.null(var_names)) var_names[response_idx] else paste("Var", response_idx)
#   
#   # 5. Criar o Plot
#   # Ajuste do limite vertical para comportar a banda de confiança
#   y_lims <- c(min(stats[1, ], na.rm = TRUE), max(stats[3, ], na.rm = TRUE))
#   
#   plot(horizon, stats[2, ], type = "n", 
#        ylim = y_lims,
#        xlab = "Períodos após o choque", 
#        ylab = "Resposta",
#        main = paste("GIRF: Choque em", shock_name, "-> Resposta de", resp_name),
#        las = 1) # Números do eixo Y na horizontal
#   
#   # Adicionar a banda de confiança sombreada
#   polygon(c(horizon, rev(horizon)), 
#           c(stats[1, ], rev(stats[3, ])), 
#           col = rgb(0.1, 0.5, 0.8, 0.2), border = NA)
#   
#   # Adicionar linha da mediana e linha zero
#   lines(horizon, stats[2, ], lwd = 2, col = "darkblue")
#   lines(horizon, stats[1, ], lty = 3, col = "darkblue") # Opcional: linha pontilhada inferior
#   lines(horizon, stats[3, ], lty = 3, col = "darkblue") # Opcional: linha pontilhada superior
#   
#   abline(h = 0, col = "red", lwd = 1)
#   
#   legend("topright", legend = c("Mediana", paste(conf*100, "% IC")),
#          fill = c(NA, rgb(0.1, 0.5, 0.8, 0.2)), 
#          border = c(NA, NA),
#          lty = c(1, NA), 
#          lwd = c(2, NA), 
#          bty = "n")
# }

# Versão para GGPLOT2
#library(ggplot2)

plot_girf_mc <- function(boot_results, shock_idx = 1, response_idx = 2, conf = 0.95) {
  
  # 1. Extração da trajetória de cada simulação
  dist_values <- sapply(boot_results, function(x) {
    as.numeric(x[[shock_idx]][, response_idx])
  })
  
  # 2. Cálculo dos Quantis
  lower_q <- (1 - conf) / 2
  upper_q <- 1 - lower_q
  
  stats <- apply(dist_values, 1, function(x) {
    quantile(x, probs = c(lower_q, 0.5, upper_q), na.rm = TRUE)
  })
  
  # 3. Criar Data Frame para o ggplot
  n_steps <- nrow(dist_values)
  df_plot <- data.frame(
    horizon = 0:(n_steps - 1),
    low  = stats[1, ],
    mid  = stats[2, ],
    high = stats[3, ]
  )
  
  # 4. Resgate de nomes para títulos
  var_names <- colnames(boot_results[[1]][[1]])
  shock_name <- names(boot_results[[1]])[shock_idx]
  resp_name <- if(!is.null(var_names)) var_names[response_idx] else paste("Var", response_idx)
  
  # 5. Construção do Objeto Gráfico
  p <- ggplot(df_plot, aes(x = horizon)) +
    geom_ribbon(aes(ymin = low, ymax = high, fill = "Intervalo de Confiança"), alpha = 0.2) +
    geom_line(aes(y = mid, color = "Mediana"), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_fill_manual("", values = "steelblue") +
    scale_color_manual("", values = "darkblue") +
    labs(
      title = paste("GIRF: Choque em", shock_name),
      subtitle = paste("Resposta de", resp_name, "| IC de", conf*100, "%"),
      x = "Horizonte (Períodos)",
      y = "Resposta"
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      legend.title = element_blank()
    )
  
  # Retorna o objeto (essencial para que o plot apareça fora da função)
  return(p)
}

# FUNÇÃO PARA SALVAR TODAS AS IRFs GERADAS PELO MODELO
save_all_girfs <- function(boot_results, folder_name = "GIRF_Results", output_type = "pdf") {
  
  # 1. Criar diretório se não existir
  if (!dir.exists(folder_name)) dir.create(folder_name)
  
  # 2. Identificar dimensões (quantas variáveis existem)
  # Pegamos os nomes das variáveis do primeiro elemento da lista
  var_names <- colnames(boot_results[[1]][[1]])
  n_vars <- length(var_names)
  
  message(paste("Iniciando exportação de", n_vars^2, "gráficos..."))
  
  # 3. Opção PDF: Abre um único arquivo para todos os gráficos
  if(output_type == "pdf") {
    pdf(file.path(folder_name, "GIRF_Completo.pdf"), width = 10, height = 7)
  }
  
  # 4. Loops aninhados para percorrer Choques e Respostas
  for (i in 1:n_vars) {     # i = índice do choque
    for (j in 1:n_vars) {   # j = índice da resposta
      
      # Gera o gráfico usando a função que criamos anteriormente
      p <- plot_girf_mc(boot_results, shock_idx = i, response_idx = j)
      
      if(output_type == "pdf") {
        print(p) # Adiciona uma nova página ao PDF
      } else {
        # Salva como arquivos individuais PNG
        file_name <- paste0("Shock_", var_names[i], "_Resp_", var_names[j], ".png")
        ggsave(filename = file.path(folder_name, file_name), plot = p, 
               width = 8, height = 6, dpi = 300)
      }
    }
  }
  
  # 5. Fechar dispositivo se for PDF
  if(output_type == "pdf") {
    dev.off()
    message(paste("Sucesso! O arquivo 'GIRF_Completo.pdf' foi salvo em:", folder_name))
  } else {
    message(paste("Sucesso! Gráficos individuais salvos em:", folder_name))
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)

# função para ver as IRFs em painel
plot_girf_panel <- function(boot_results, conf = 0.95) {
  
  var_names <- colnames(boot_results[[1]][[1]])
  n_vars <- length(var_names)
  n_steps <- nrow(boot_results[[1]][[1]])
  
  all_data <- list()
  
  # 1. Empilhar todos os dados de todas as combinações
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      dist_values <- sapply(boot_results, function(x) as.numeric(x[[i]][, j]))
      
      lower_q <- (1 - conf) / 2
      upper_q <- 1 - lower_q
      stats <- apply(dist_values, 1, quantile, probs = c(lower_q, 0.5, upper_q), na.rm = TRUE)
      
      temp_df <- data.frame(
        horizon = 0:(n_steps - 1),
        low = stats[1, ],
        mid = stats[2, ],
        high = stats[3, ],
        shock = names(boot_results[[1]])[i],
        response = var_names[j]
      )
      all_data[[length(all_data) + 1]] <- temp_df
    }
  }
  
  df_panel <- do.call(rbind, all_data)
  
  # 2. Garantir que a ordem das facetas siga a ordem das variáveis
  df_panel$shock <- factor(df_panel$shock, levels = names(boot_results[[1]]))
  df_panel$response <- factor(df_panel$response, levels = var_names)
  
  # 3. Construir o gráfico em painel
  p <- ggplot(df_panel, aes(x = horizon)) +
    geom_ribbon(aes(ymin = low, ymax = high), fill = "steelblue", alpha = 0.2) +
    geom_line(aes(y = mid), color = "darkblue", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    facet_grid(response ~ shock, scales = "free_y") + # Linhas = Resposta, Colunas = Choque
    labs(
      title = "Matriz de Funções Impulso-Resposta Generalizada (GIRF)",
      subtitle = paste("Bandas de confiança de", conf*100, "% via Simulação de Monte Carlo"),
      x = "Horizonte",
      y = "Resposta Acumulada"
    ) +
    theme_classic() +
    theme(
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")
    )
  
  return(p)
}