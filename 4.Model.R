# Teste de raiz unitária das séries -----------------------------------------
# Teste ADF de Raíz Unitária da Tx. Cresc. PIB ------------------------------
g <- ts(pkgm_data$gdp_rg, start = 1970, end = 2019, frequency = 1)

tsdisplay(g)

## Passo (1): Testar ADF usando modelo com constante e com tendência:

adf.ct <- urca::ur.df(g, type="trend", selectlags = "AIC")

plot(adf.ct)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: beta0 = gama = 0

summary(adf.ct)

interp_urdf(adf.ct)

## Passo (2): Testar ADF usando modelo só com constante.

adf.c <- ur.df(g, type="drift", selectlags = "AIC")

plot(adf.c)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: mu = gama = 0 

summary(adf.c)
interp_urdf(adf.c)

## Passo (3): Testar ADF usando modelo sem constante e sem tendência linear:

adf <- ur.df(g, type="none", selectlags = "AIC")

plot(adf)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]

summary(adf)
interp_urdf(adf, level = "10pct")

urca::ur.kpss(g, type = "mu", lags = "long") |> summary()
urca::ur.kpss(g, type = "tau", lags = "long") |> summary()

fable::as_tsibble(pkgm_data, index = year) |>
  features(gdp_rg, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  features(gdp_rg, unitroot_ndiffs)


# Teste ADF de Raíz Unitária da Utilização da Capacidade------------------------
u <- ts(pkgm_data$uci, start = 1970, end = 2019, frequency = 1)
plot(u)
tsdisplay(diff(u))

## Passo (1): Testar ADF usando modelo com constante e com tendência:

adf.ct <- urca::ur.df(diff(u), type="trend", selectlags = "AIC")

plot(adf.ct)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: beta0 = gama = 0

summary(adf.ct)

interp_urdf(adf.ct)

## Passo (2): Testar ADF usando modelo só com constante.

adf.c <- ur.df(diff(u), type="drift", selectlags = "AIC")

plot(adf.c)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: mu = gama = 0 

summary(adf.c)
interp_urdf(adf.c)

## Passo (3): Testar ADF usando modelo sem constante e sem tendência linear:

adf <- ur.df(diff(u), type="none", selectlags = "AIC")

plot(adf)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]

summary(adf)
interp_urdf(adf)

urca::ur.kpss(u, type = "mu", lags = "short") |> summary()
urca::ur.kpss(u, type = "tau", lags = "short") |> summary()

fable::as_tsibble(pkgm_data, index = year) |>
  features(uci, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |>
  features(uci, unitroot_ndiffs)

# Teste ADF de Raíz Unitária da profit share ----------------------------------
psh <- ts(pkgm_data$profitsh, start = 1970, end = 2019, frequency = 1)
plot(psh)
tsdisplay(diff(psh))

## Passo (1): Testar ADF usando modelo com constante e com tendência:

adf.ct <- urca::ur.df(diff(psh), type="trend", selectlags = "AIC")

plot(adf.ct)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: beta0 = gama = 0

summary(adf.ct)

interp_urdf(adf.ct)

## Passo (2): Testar ADF usando modelo só com constante.

adf.c <- ur.df(diff(psh), type="drift", selectlags = "AIC")

plot(adf.c)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]
# b. Testar H0: mu = gama = 0 

summary(adf.c)
interp_urdf(adf.c)

## Passo (3): Testar ADF usando modelo sem constante e sem tendência linear:

adf <- ur.df(diff(psh), type="none", selectlags = "AIC")

plot(adf)	# Analisar a FAC e FAC-P. Se necessário, eleve o número de lags. 

# a. Testar H0: gama = 0 [d=1] vs. H0: gama < 0 [d=0]

summary(adf)
interp_urdf(adf)

urca::ur.kpss(diff(psh), type = "mu",lags = "short") |> summary()
urca::ur.kpss(diff(psh), type = "tau", lags = "short") |> summary()

fable::as_tsibble(pkgm_data, index = year) |> 
  #mutate(diff_profitsh = tsibble::difference(profitsh)) |> 
  features(profitsh, unitroot_kpss)

fable::as_tsibble(pkgm_data, index = year) |> 
  #mutate(diff_profitsh = tsibble::difference(profitsh)) |> 
  features(profitsh, unitroot_ndiffs)

           

# MODELO VAR ------------------------------------------------------------------
