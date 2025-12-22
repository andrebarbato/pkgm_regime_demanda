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