for (pacote in c("tidyverse", "geobr", "sf")) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(geobr)
library(sf)

# Nossa base: população + PIB por município (mesmo join de analise_populacao.r)
populacao <- read.csv("dados/populacao_censo2022.csv")
pib <- read.csv("dados/tabela5938.csv")

populacao_pib <- populacao |>
  left_join(
    pib |> select(-municipio),
    by = c("codigo_municipio" = "cod_municipio")
  ) |>
  mutate(pib_per_capita = (pib_mil_reais * 1000) / populacao) |>
  as_tibble()

# Malha municipal do IBGE de 2022. simplified = TRUE baixa a versão com
# polígonos simplificados, bem mais leve e suficiente para mapas
malha <- read_municipality(code_muni = "all", year = 2022, simplified = TRUE)

# A malha vem na frente do left_join para manter todos os seus municípios
# (e a classe sf, com a geometria). code_muni é numérico na malha e
# codigo_municipio é lido como número no CSV, então os tipos já batem
malha_populacao <- malha |>
  left_join(populacao_pib, by = c("code_muni" = "codigo_municipio"))

# Municípios da malha que ficaram sem dados da nossa base
malha_populacao |>
  st_drop_geometry() |>
  filter(is.na(populacao)) |>
  select(code_muni, name_muni, abbrev_state)

nrow(malha)
nrow(malha_populacao)

# Mapa da parcela de idosos (65+) por município
idosos <- read.csv("dados/idosos_municipios.csv")

malha_idosos <- malha |>
  left_join(idosos, by = c("code_muni" = "cod_municipio"))

# color = NA tira as bordas: com 5570 municípios elas cobririam o mapa.
# direction = -1 na escala viridis "rocket" deixa os valores altos mais escuros
mapa_idosos <- ggplot(malha_idosos) +
  geom_sf(aes(fill = pct_idosos), color = NA) +
  scale_fill_viridis_c(
    option = "rocket", direction = -1,
    labels = scales::percent
  ) +
  labs(
    title = "Parcela de moradores com 65 anos ou mais, por município",
    subtitle = "Censo 2022 - quanto mais escuro, mais envelhecido",
    fill = "65 anos ou mais"
  ) +
  theme_void()

ggsave("imagens/mapa_idosos.png", mapa_idosos, width = 8, height = 8, dpi = 150, bg = "white")
