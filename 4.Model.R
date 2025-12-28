# Teste de raiz unitária das séries -----------------------------------------
# Teste ADF de Raíz Unitária da Tx. Cresc. PIB ------------------------------

# tx. cresc. PIB em nível
g <- ts(pkgm_data$gdp_rg, start = 1970, end = 2019, frequency = 1)
# tx. cresc. PIB em primeira diferença
g_d <- diff(g)

# teste de raíz unitária
out_g <- summary_urdf(g, "g")
out_g_d <- summary_urdf(g_d, "diff(g)")

# juntando os resultados de g e diff(g)
dplyr::bind_rows(out_g$sum_adf, 
                 out_g_d$sum_adf) |>
  stargazer::stargazer(summary = FALSE, type = "text") # mudar para latex 

interp_urdf(out_g$trend_adf, level = "5pct")
interp_urdf(out_g$drift_adf, level = "1pct")
interp_urdf(out_g$none_adf, level = "1pct")

interp_urdf(out_g_d$trend_adf, level = "5pct")
interp_urdf(out_g_d$drift_adf, level = "5pct")
interp_urdf(out_g_d$none_adf, level = "5pct")

# Testando a robustez
urca::ur.kpss(pkmg_data_treated[,1], type = "mu", lags = "short") |> summary()
fable::as_tsibble(pkmg_data_treated, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(gdp_rg, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(gdp_rg, unitroot_ndiffs)

plot(out_g_d$none_adf)

# Teste ADF de Raíz Unitária da Utilização da Capacidade------------------------

# utilização da capacidade em nível
u <- ts(pkgm_data$uci, start = 1970, end = 2019, frequency = 1)
# utilização da capacidade em primeira diferença
u_d <- diff(u)

# teste de raíz unitária
out_u <- summary_urdf(u, "u")
out_u_d <- summary_urdf(u_d, "diff(u)")

# juntando os resultados de u e diff(u)
dplyr::bind_rows(out_u$sum_adf, 
                 out_u_d$sum_adf) |>
  stargazer::stargazer(summary = FALSE, type = "text") # mudar para latex 

interp_urdf(out_u$trend_adf, level = "5pct")
interp_urdf(out_u$drift_adf, level = "5pct")
interp_urdf(out_u$none_adf, level = "5pct")

interp_urdf(out_u_d$trend_adf, level = "5pct")
interp_urdf(out_u_d$drift_adf, level = "5pct")
interp_urdf(out_u_d$none_adf, level = "5pct")

# Testando a robustez
urca::ur.kpss(u, type = "tau", lags = "short") |> summary()
fable::as_tsibble(pkgm_data, index = year) |>
  #dplyr::filter(year %in% c(1970:2008)) |> 
  features(uci, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(uci, unitroot_ndiffs)

plot(out_u_d$none_adf)

# Teste ADF de Raíz Unitária da profit share ----------------------------------

# tx. cresc. PIB em níve
p <- ts(pkgm_data$profitsh, start = 1970, end = 2019, frequency = 1)
# tx. cresc. PIB em primeira diferença
p_d <- diff(p)

# teste de raíz unitária
out_p <- summary_urdf(p, "p")
out_p_d <- summary_urdf(p_d, "diff(p)")

# juntando os resultados de u e diff(u)
dplyr::bind_rows(out_p$sum_adf, 
                 out_p_d$sum_adf) |>
  stargazer::stargazer(summary = FALSE, type = "text") # mudar para latex 

interp_urdf(out_p$trend_adf, level = "5pct")
interp_urdf(out_p$drift_adf, level = "5pct")
interp_urdf(out_p$none_adf, level = "5pct")

interp_urdf(out_p_d$trend_adf, level = "5pct")
interp_urdf(out_p_d$drift_adf, level = "5pct")
interp_urdf(out_p_d$none_adf, level = "5pct")

# Testando a robustez
urca::ur.kpss(p, type = "tau", lags = "short") |> summary()
fable::as_tsibble(pkgm_data, index = year) |>
  #dplyr::filter(year %in% c(1970:2008)) |> 
  features(profitsh, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(profitsh, unitroot_ndiffs)

plot(out_p_d$none_adf)

# Teste ADF de Raíz Unitária da wage share ----------------------------------

# wage share em nível
w <- ts(pkgm_data$wagesh, start = 1970, end = 2019, frequency = 1)
# wage share em nível em primeira diferença
w_d <- diff(w)

# teste de raíz unitária
out_w <- summary_urdf(w, "w")
out_w_d <- summary_urdf(w_d, "diff(w)")

# juntando os resultados de u e diff(u)
dplyr::bind_rows(out_w$sum_adf, 
                 out_w_d$sum_adf) |>
  stargazer::stargazer(summary = FALSE, type = "text") # mudar para latex 

interp_urdf(out_w$trend_adf, level = "5pct")
interp_urdf(out_w$drift_adf, level = "5pct")
interp_urdf(out_w$none_adf, level = "5pct")

interp_urdf(out_w_d$trend_adf, level = "5pct")
interp_urdf(out_w_d$drift_adf, level = "5pct")
interp_urdf(out_w_d$none_adf, level = "5pct")

# Testando a robustez
urca::ur.kpss(w, type = "tau", lags = "short") |> summary()
fable::as_tsibble(pkgm_data, index = year) |>
  #dplyr::filter(year %in% c(1970:2008)) |> 
  features(wagesh, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(wagesh, unitroot_ndiffs)

plot(out_w_d$none_adf)

# Teste de Cointegração de Johansen ------------------------------------

var_df_nivel <- pkmg_data_treated[,c(1:3)]
colnames(var_df_nivel) <- c("g", "u", "p")
plot.ts(var_df_nivel)

order_selection <- vars::VARselect(var_df_nivel,lag.max = 5, type = "both")

stargazer::stargazer(order_selection$selection, summary = FALSE, type = "text")

stargazer::stargazer(t(order_selection$criteria), summary = FALSE, type = "text")

lags_ci <- order_selection$selection |> min()

var_base <- vars::VAR(var_df_nivel, p = 1, type = "both", season = NULL, exogen = NULL)

acf(residuals(var_base),36)

# Teste de Cointegração de Johansen

H1.eigen <- urca::ca.jo(var_df_nivel, 
                        type="eigen", 
                        ecdet="none", 
                        K= ifelse(lags_ci<2, 2, lags_ci), 
                        spec="longrun", 
                        season = NULL)
summary(H1.eigen)

H1.trace <- urca::ca.jo(var_df_nivel, 
                        type="trace", 
                        ecdet="none", 
                        K= ifelse(lags_ci<2, 2, lags_ci), 
                        spec="longrun", 
                        season = NULL)
summary(H1.trace)

matjo.eigin <-  H1.eigen@cval
matjo.eigin <- cbind(matjo.eigin, H1.eigen@teststat)
colnames(matjo.eigin) <- c("10pct", "5pct", "1pct", "test")

stargazer::stargazer(matjo.eigin, summary = FALSE, type = "latex")

matjo.trace <-  H1.trace@cval
matjo.trace <- cbind(matjo.trace, H1.trace@teststat)
colnames(matjo.trace) <- c("10pct", "5pct", "1pct", "test")

stargazer::stargazer(matjo.trace, summary = FALSE, type = "latex")

# VAR Model ------------------------------------------------------------

# pré-seleção do modelo
acf(var_df_nivel,24)

vars::VARselect(var_df_nivel,lag.max = 5, type = "both")

# Modelo VAR p 1

vec_model <- vars::vec2var(H1.eigen, r=1)

plot.ts(residuals(vec_model))
acf(residuals(vec_model),36)

# Verificação Resíduos por Ljung-Box:
serial.test(vec_model)

# Verificação da Normalidade dos Resíduos por Jarque-Bera:

normality.test(vec_model)

# Verificação da Homocedasticidade dos Resíduos:

arch.test(vec_model, lags.multi = 6)

# Teste de Estabilidade Paramétrica das Eq. do VAR:

plot(vars::stability(vec_model), nc=1)

# Rodar a simulação de monte carlo
resultados_mc_95 <- boot_girf_vec(vec_model, n.ahead = 8, runs = 10000)

# 3. Exportar todos os gráficos para um PDF organizado
save_all_girfs(resultados_mc_95, folder_name = "GIRF_output", output_type = "pdf")

painel_geral_95 <- plot_girf_panel(resultados_mc_95)

# Rodar a simulação de monte carlo para outros IC
resultados_mc_68 <- boot_girf_vec(vec_model, n.ahead = 8, runs = 10000, conf = 0.68)

painel_geral_68 <- plot_girf_panel(resultados_mc_95, conf = 0.68)

print(painel_geral_68)
