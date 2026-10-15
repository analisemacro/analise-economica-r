if (!requireNamespace("sidrar", quietly = TRUE)) {
  install.packages("sidrar", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

library(sidrar)
library(tidyverse)

# Tabela 4714 do SIDRA: Censo 2022 - população, área e densidade por município
# Vem em formato longo (3 variáveis por município), então usamos pivot_wider
# para virar 1 linha por município com uma coluna por variável
dados_brutos <- get_sidra(x = 4714, geo = "City")

populacao <- dados_brutos %>%
  select(
    codigo_municipio = `Município (Código)`,
    municipio = Município,
    variavel = Variável,
    valor = Valor
  ) %>%
  pivot_wider(names_from = variavel, values_from = valor) %>%
  rename(
    populacao = `População residente`,
    area_km2 = `Área da unidade territorial`,
    densidade = `Densidade demográfica`
  ) %>%
  mutate(uf = str_extract(municipio, "(?<=- )[A-Z]{2}$"))

write.csv(populacao, "dados/populacao_censo2022.csv", row.names = FALSE)

nrow(populacao)
