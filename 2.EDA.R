# Exploratory Data Analysis

# Figura 1 - Wage share and 
# #4f7ea5
# #d20a0a
pkgm_data |> 
  tidyr::pivot_longer(
    !year,
    names_to = "series",
    values_to = "value"
  ) |> 
  dplyr::filter(series %in% c("profitsh", "uci")) |> 
  ggplot2::ggplot(aes(x = year, y = value, colour = series)) +
  ggplot2::geom_line(linewidth = 0.75) +
  ggplot2::theme_classic() +
  ggplot2::labs(
    x = "Anos",
    y = "Profit share e Utilização da Capacidade (%)",
    colour = "Série"
    ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = "lightgray"), # Linhas de grade principais
    panel.grid.minor.y = ggplot2::element_line(color = "lightgray", 
                                               linetype = "dotted"), # Linhas de grade menores
    legend.position = "bottom"
    ) +
  ggplot2::scale_color_manual(
    values = c("profitsh" = "#d20a0a", "uci" = "#4f7ea5"),  # Define as cores das séries
    labels = c("profitsh" = "Profit Share", "uci" = "Utilização da Capacidade na Indústria")
    )

# Tabela 1 - Estatísticas descritivas do banco de dados

pkgm_data |> 
  dplyr::select(!year) |> 
  as.data.frame() |> 
  stargazer::stargazer(type = "latex")

# Gráficos do teste de cointegração de Johansen
j_df <- as.data.frame(pkmg_data_treated[,1:3])
j_df$year <- time(pkmg_data_treated[,1:3])
j_ts <- pkmg_data_treated[,1:3]

summary(j_df)

plot.ts(j_df[,c(1,2)])
plot.ts(j_df[,c(1,3)])
plot.ts(j_df[,c(2,3)])

# grafico conjunto g e u
j_df |> 
  dplyr::mutate(
  g_norm = (g -min(g))/(max(g)-min(g)),
  u_norm = (u -min(u))/(max(u)-min(u))
) |> 
  tidyr::pivot_longer(
    !year,
    names_to = "variable",
    values_to = "value"
  ) |> 
  dplyr::filter(variable %in% c("g_norm","u_norm")) |> 
  ggplot2::ggplot(aes(x = year, y = value, colour = variable)) +
  ggplot2::geom_line() +
  ggplot2::labs(
    x = "Time",               # Rótulo do eixo horizontal
    y = "Value",              # Rótulo do eixo vertical
    colour = NULL             # Remove o título da legenda
  ) +
  ggplot2::scale_colour_manual(values = c("g_norm" = "blue", "u_norm" = "red"),
                      labels = c("g", "u")) +
  ggplot2::theme_classic() +
  ggplot2:: theme(legend.position = "bottom")
  

# grafico conjunto g e p
j_df |> 
  dplyr::mutate(
    g_norm = (g -min(g))/(max(g)-min(g)),
    p_norm = (p -min(p))/(max(p)-min(p))
  ) |> 
  tidyr::pivot_longer(
    !year,
    names_to = "variable",
    values_to = "value"
  ) |> 
  dplyr::filter(variable %in% c("g_norm","p_norm")) |> 
  ggplot2::ggplot(aes(x = year, y = value, colour = variable)) +
  ggplot2::geom_line() +
  ggplot2::labs(
    x = "Time",               # Rótulo do eixo horizontal
    y = "Value",              # Rótulo do eixo vertical
    colour = NULL             # Remove o título da legenda
  ) +
  ggplot2::scale_colour_manual(values = c("g_norm" = "blue", "p_norm" = "red"),
                               labels = c("g", "p")) +
  ggplot2::theme_classic() +
  ggplot2:: theme(legend.position = "bottom")

# grafico conjunto u e p
j_df |> 
  dplyr::mutate(
    u_norm = (u -min(u))/(max(u)-min(u)),
    p_norm = (p -min(p))/(max(p)-min(p))
  ) |> 
  tidyr::pivot_longer(
    !year,
    names_to = "variable",
    values_to = "value"
  ) |> 
  dplyr::filter(variable %in% c("u_norm","p_norm")) |> 
  ggplot2::ggplot(aes(x = year, y = value, colour = variable)) +
  ggplot2::geom_line() +
  ggplot2::labs(
    x = "Time",               # Rótulo do eixo horizontal
    y = "Value",              # Rótulo do eixo vertical
    colour = NULL             # Remove o título da legenda
  ) +
  ggplot2::scale_colour_manual(values = c("u_norm" = "blue", "p_norm" = "red"),
                               labels = c("u", "p")) +
  ggplot2::theme_classic() +
  ggplot2:: theme(legend.position = "bottom")






cor(pkmg_data_treated[,2], pkmg_data_treated[,1])

vars::VARselect(j_df[,1:2],lag.max = 4, type = "both")

H1.eigen.teste <- urca::ca.jo(j_df[,1:2], 
                        type="eigen", 
                        ecdet="none", 
                        K= 3, 
                        spec="longrun", 
                        season = NULL)

summary(H1.eigen.teste)
