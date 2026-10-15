if (!requireNamespace("sidrar", quietly = TRUE)) {
  install.packages("sidrar", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

library(sidrar)
library(tidyverse)

# Tabela 5938 do SIDRA: PIB dos municípios a preços correntes.
# A tabela tem muitas variáveis (PIB, valor adicionado por setor, participações etc.)
# e cobre 2002-2023, então restringimos à variável 37 (PIB) e ao ano mais recente,
# senão o pedido ultrapassa o limite de linhas da API.
dados_brutos <- get_sidra(x = 5938, variable = 37, period = "2023", geo = "City")

pib <- dados_brutos %>%
  select(
    cod_municipio = `Município (Código)`,
    municipio = Município,
    pib_mil_reais = Valor
  )

write.csv(pib, "dados/tabela5938.csv", row.names = FALSE)

nrow(pib)
