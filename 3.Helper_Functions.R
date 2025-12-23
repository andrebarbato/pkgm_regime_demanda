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
