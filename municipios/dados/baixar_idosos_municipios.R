if (!requireNamespace("sidrar", quietly = TRUE)) {
  install.packages("sidrar", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

library(sidrar)
library(tidyverse)

# Tabela 9514 do SIDRA: Censo 2022 - população residente por sexo e idade.
# Pedimos o total e os grupos de idade de 65 anos ou mais (65-69, 70-74, ...,
# 95-99 e 100+) para todos os municípios. Isso dá 9 categorias x 5.570
# municípios = 50.130 linhas, que passa do limite de 50.000 da API, então
# quebramos em 2 chamadas: uma com o total, outra com os grupos de idosos.
categorias_idade <- c(100362, 93096, 93097, 93098, 49108, 49109, 60040, 60041, 6653)

total_pop <- get_sidra(
  x = 9514,
  period = "last",
  classific = c("c2", "c287"),
  category = list("c2" = 6794, "c287" = categorias_idade[1]),
  geo = "City"
)

idade_65_mais <- get_sidra(
  x = 9514,
  period = "last",
  classific = c("c2", "c287"),
  category = list("c2" = 6794, "c287" = categorias_idade[-1]),
  geo = "City"
)

dados_brutos <- bind_rows(total_pop, idade_65_mais)

idosos <- dados_brutos %>%
  select(cod_municipio = `Município (Código)`, idade_cod = `Idade (Código)`, valor = Valor) %>%
  # Grupos de idade muito pequenos (95-99, 100+) vêm como NA em municípios
  # pequenos: é sigilo estatístico do IBGE para não identificar poucas pessoas,
  # não ausência de dado. Tratamos como 0 antes de somar, senão NA + número = NA
  # e o município inteiro ficaria sem informação de idosos.
  mutate(valor = replace_na(valor, 0)) %>%
  mutate(grupo = if_else(idade_cod == 100362, "populacao_total", "idosos")) %>%
  group_by(cod_municipio, grupo) %>%
  summarise(valor = sum(valor), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(pct_idosos = idosos / populacao_total)

write.csv(idosos, "dados/idosos_municipios.csv", row.names = FALSE)

nrow(idosos)
