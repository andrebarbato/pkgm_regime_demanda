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
urca::ur.kpss(g, type = "tau", lags = "short") |> summary()
fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(gdp_rg, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(gdp_rg, unitroot_ndiffs)

plot(out_g_d$none_adf)

# Teste ADF de Raíz Unitária da Utilização da Capacidade------------------------

# tx. cresc. PIB em níve
u <- ts(pkgm_data$uci, start = 1970, end = 2019, frequency = 1)
# tx. cresc. PIB em primeira diferença
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
  dplyr::filter(year %in% c(1970:2008)) |> 
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
  dplyr::filter(year %in% c(1970:2008)) |> 
  features(profitsh, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  dplyr::filter(year %in% c(1970:2019)) |> 
  features(wagesh, unitroot_ndiffs)

plot(out_p_d$none_adf)

# VAR Model ------------------------------------------------------------
var_df <- pkgm_data |> 
  dplyr::filter(year %in% c(1970:2008)) |> 
  dplyr::mutate(dp = profitsh - lag(profitsh)) |> 
  dplyr::select(!c(year,wagesh,dp)) |> tidyr::drop_na()

w <- ts(pkgm_data$wagesh, start = 1970, end = 2019, frequency = 1)
w_d <- diff(w)
var_df <- cbind(g_d, u_d, w_d)
colnames(var_df) <- c("dg", "du", "dw")
plot.ts(var_df)

# pré-seleção do modelo
acf(var_df,24)

vars::VARselect(var_df,lag.max = 5, type = "none")

H1.eigen <- urca::ca.jo(var_df, 
                        type="eigen", 
                        ecdet="const", 
                        K= 2, 
                        spec="longrun", 
                        season = NULL)
summary(H1.eigen)

H1.trace <- urca::ca.jo(var_df, 
                        type="trace", 
                        ecdet="const", 
                        K= 2, 
                        spec="longrun", 
                        season = NULL)
summary(H1.trace)

modelo_var <- vars::vec2var(H1.eigen, r = 2)

vars::serial.test(modelo_var)

vars::normality.test(modelo_var)

vars::arch.test(modelo_var)

impulso_resposta <- vars::irf(
  x = modelo_var,
  impulse = "dw",
  response = "du",
  n.ahead = 24,
  runs = 1000,
  seed = 42
)
plot(impulso_resposta)

impulso_resposta2 <- vars::irf(
  x = modelo_var,
  impulse = "dw",
  response = "dg",
  n.ahead = 24,
  runs = 1000,
  seed = 42
)
plot(impulso_resposta2)

impulso_resposta3 <- vars::irf(
  x = modelo_var,
  impulse = "du",
  response = "dg",
  n.ahead = 24,
  runs = 1000,
  seed = 42
)

plot(impulso_resposta3)

summary(modelo_var)

VECM.vars <- urca::cajorls(H1.eigen, r=1)
VECM.vars
summary(VECM.vars$rlm)

impulso_resposta3 <- vars::irf(
  x = VECM.vars$rlm,
  impulse = "p",
  response = "g",
  n.ahead = 8,
  runs = 1000,
  seed = 42
)

var_est_p1 <- vars::VAR(var_df, p = 1, type = "const", season = NULL, exogen = NULL)
var_est_p4 <- vars::VAR(var_df, p = 4, season = NULL, exogen = NULL)
var_est_p7 <- vars::VAR(var_df, p = 7, season = NULL, exogen = NULL)
var_est_p8 <- vars::VAR(var_df, p = 8, season = NULL, exogen = NULL)
var_est_p9 <- vars::VAR(var_df, p = 9, season = NULL, exogen = NULL)
var_est_p10 <- vars::VAR(var_df, p = 10, season = NULL, exogen = NULL)
var_est_p12 <- vars::VAR(var_df, p = 12, season = NULL, exogen = NULL)
var_est_p9s <- vars::VAR(var_df, p = 10, season = 12, exogen = NULL)

acf(residuals(var_est_p1),24)
summary(var_est_p1)

vars::Acoef(var_est_p1)                                # Gera as Matrizes Pi parâmetro auto e inte-regressivos.
vars::roots(var_est_p1, modulus = TRUE)                # Autovalores da Matriz Associada 

## Testes de Auocorrelação: Portmanteau e FAC´s
# Portmanteau: H0: Erros Não serialmente correlacionados até o lag #

ACF <- vars::serial.test(var_est_p1, lags.pt= 24, type = "PT.asymptotic")
ACF

plot(ACF, names="g")
plot(ACF, names="u")
plot(ACF, names="dp")

## Teste de Normalidade dos Resíduos:
# H0: Erros Normalmente distribuídos

vars::normality.test(var_est_p1, multivariate.only = TRUE)

## Previsão com modelo VAR

plot(predict(var_est_p1, n.ahead = 10, ci = 0.95, dumvar = NULL))

## Teste de Causalidade de Granger:

vars::causality(var_est_p1, cause= "g")
vars::causality(var_est_p1, cause= "u")
vars::causality(var_est_p1, cause= "p")

## Estimação do SVAR Modelo Tipo A:

N.Restrictions <- function(K){
  R <- K*(K-1)/2
  return(R)}

N.Restrictions(3)

## SVAR: Ver Apresentação sobre aula prácita SVAR

# Restrições do Tipo A:
# Hipóteses:
# (i)    PIB não reage contemporaneamente à inflação e à tx. juros (PIB só reage defasadamente à essas variáveis)
# (ii)   Inflação reage contemporaneamente ao PIB e não reage contemporaneamente à tx. juros.
# (iii)  tx. juros reage contemporaneamente ao PIB e à inflação (BC Monitora ativamente DA e Preços)

# Geraçãoda Matriz A de Efeitos Feed-Back: Modelo tipo A

Amat <- diag(3)
colnames(Amat) <- rownames(Amat) <- c("g", "u", "p")
Amat[2,1] <-NA 
Amat[3,1] <-NA 
Amat[3,2] <-NA 
Amat

SVAR.A <- vars::SVAR(var_est_p1, Amat=Amat, Bmat=NULL, hessian=TRUE, estmethod ="direct")
SVAR.A

# Estimação das Funções Impulso-Resposta com base no SVAR:

irfA.1 <- vars::irf(SVAR.A, n.ahead= 8,  impulse = "p", response = c("u"), runs = 1000, seed = 1)
plot(irfA.1)

irfA.2<- vars::irf(SVAR.A, n.ahead=8, impulse = "p", response = "g", runs = 1000, seed = 1)     
plot(irfA.2)

irfA.3 <- vars::irf(SVAR.A, n.ahead=8, impulse = "u", response = "g", runs = 1000, seed = 1)     
plot(irfA.3)

# Restrições do Tipo B:
# Hipóteses:

# (i)    O PIB e a inflação não são afetados instantaneamente por choques monetários
# (ii)   O Banco Central reage contemporaneamente a choques de PIB e inflação.

# Geração da Matriz B:

Bmat <- matrix(NA, 3, 3)
colnames(Bmat) <- rownames(Bmat) <- c("g", "u", "p")

Bmat[1,3] <- 0  # choque monetário não afeta PIB contemporaneamente
Bmat[1,2] <- 0  # choque Inflação não afeta PIB contemporaneamente
Bmat[2,3] <- 0  # choque monetário não afeta inflação contemporaneamente
Bmat

SVAR.B <- vars::SVAR(var_est_p1, Amat=NULL, Bmat=Bmat, hessian=TRUE, estmethod ="direct")
SVAR.B

irfB.1 <- vars::irf(SVAR.B, n.ahead=20,  impulse = "p", response = c("u"))
plot(irfB.1)

irfB.2<- vars::irf(SVAR.B, n.ahead=20, impulse = "p", response = "g")     
plot(irfB.2)

irfB.3 <- vars::irf(SVAR.B, n.ahead=20, impulse = "u", response = "g")     
plot(irfB.3)
