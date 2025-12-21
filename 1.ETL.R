# ETL

# wage share -------------------------------------------------------------------
# Dados de wage share vem do artigo Miedbach e Marquetti (2022) - A distribuição 
# funcional da renda no Brasil: 1947-2019
# http://dx.doi.org/10.1590/0103-6351/7434

pdf_file <- "data/download.pdf"

all_tables_list <- extract_tables(pdf_file)

tab12 <- all_tables_list[[12]]

tab12_1 <- tab12 |> 
  dplyr::select(`Ano...1`, `Parcela salarial...2`) |> 
  dplyr::rename(
    year = `Ano...1`,
    wagesh = `Parcela salarial...2`
  )

tab12_2 <- tab12 |> 
  dplyr::select(`Ano...3`, `Parcela salarial...4`) |> 
  dplyr::rename(
    year = `Ano...3`,
    wagesh = `Parcela salarial...4`
  )

tab12_3 <- tab12 |> 
  dplyr::select(`Ano...5`, `Parcela salarial...6`) |> 
  dplyr::rename(
    year = `Ano...5`,
    wagesh = `Parcela salarial...6`
  )

wagesh <- dplyr::bind_rows(tab12_1, tab12_2, tab12_3) |> 
  tidyr::drop_na() |> 
  dplyr::filter(year %in% c(1970:2019))

# PIB taxa de crescimento anual ------------------------------------------------
# Fonte: IBGE via IPEADATA código: SCN10_PIBG10

gdp_rg <- ipeadatar::ipeadata(code = "SCN10_PIBG10") |>
  dplyr::rename(
    gdp_rg = value
  ) |>
  dplyr::mutate(year = lubridate::year(date)) |> 
  dplyr::select(year, gdp_rg) |> 
  dplyr::filter(year %in% c(1970:2019))

# Uilização da capacidade instalada na indístria 
# Fonte: FGV via IPEADATA código: CE12_CUTIND12

uci <- ipeadatar::ipeadata(code = "CE12_CUTIND12") |> 
  dplyr::rename(
    uci = value
    ) |>
  dplyr::mutate(year = lubridate::year(date),
                uci = uci / 100) |>
  dplyr::group_by(year) |> 
  dplyr::summarise(uci = mean(uci)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(year %in% c(1970:2019))

# Base de dados concatenada

pkgm_data <- gdp_rg |> 
  dplyr::left_join(uci, by = "year") |> 
  dplyr::left_join(wagesh, by = "year") |> 
  dplyr::mutate(profitsh = 1 - wagesh)

save(pkgm_data,
     file = "data/pkgm_data.rda")

load(file = "data/pkgm_data.rda")
