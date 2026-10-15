if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

library(tidyverse)

populacao <- read.csv("dados/populacao_censo2022.csv")

pib <- read.csv("dados/tabela5938.csv")


mg_acima_100mil <- populacao %>%
  filter(str_detect(municipio, "MG$"), populacao > 100000)

nrow(mg_acima_100mil)

populacao_resumida <- populacao %>%
  select(codigo_municipio, municipio, populacao)

populacao <- populacao %>%
  mutate(densidade = populacao / area_km2)

populacao_ordenada <- populacao %>%
  arrange(desc(densidade))

resumo_brasil <- populacao %>%
  summarise(
    total_habitantes = sum(populacao),
    media_populacao = mean(populacao),
    mediana_populacao = median(populacao),
    numero_municipios = n()
  )

resumo_brasil

# Mesma análise (municípios de MG com mais de 100 mil habitantes), de duas formas:

# 1) Sem pipe: cada etapa aninha a anterior, de dentro para fora
mg_acima_100mil_sem_pipe <- filter(populacao, str_detect(municipio, "MG$"), populacao > 100000)

# 2) Com o pipe nativo do R (|>): cada etapa alimenta a próxima, de cima para baixo
mg_acima_100mil_com_pipe <- populacao |>
  filter(str_detect(municipio, "MG$"), populacao > 100000)

# Exemplo com 5 operações encadeadas no pipe:
# entre com a tabela -> filtra -> cria coluna -> escolhe colunas -> ordena -> limita linhas
# Pergunta: quais os 10 municípios mais densos entre os que têm mais de 50 mil habitantes?
top10_mais_densos <- populacao |>
  filter(populacao > 50000) |>                      # 1. mantém só municípios maiores
  mutate(densidade = populacao / area_km2) |>        # 2. cria a coluna de densidade
  select(municipio, uf, populacao, densidade) |>     # 3. mantém só as colunas de interesse
  arrange(desc(densidade)) |>                        # 4. ordena do mais denso para o menos denso
  slice_head(n = 10)                                 # 5. mantém só as 10 primeiras linhas

top10_mais_densos

# A tabela não tem a região de cada UF, então criamos esse mapeamento antes de agrupar
regioes <- tibble(
  uf = c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
         "AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE",
         "DF", "GO", "MS", "MT",
         "ES", "MG", "RJ", "SP",
         "PR", "RS", "SC"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9), rep("Centro-Oeste", 4),
             rep("Sudeste", 4), rep("Sul", 3))
)

resumo_por_regiao <- populacao |>
  left_join(regioes, by = "uf") |>
  group_by(regiao) |>
  summarise(
    media_populacao = mean(populacao),
    numero_municipios = n()
  )

resumo_por_regiao

# Quanto cada município representa da população da sua região
populacao_com_pct <- populacao |>
  left_join(regioes, by = "uf") |>
  group_by(regiao) |>                                       # agrupa, mas mantém 1 linha por município
  mutate(pct_populacao_regiao = populacao / sum(populacao)) |> # populacao do município / soma da região
  ungroup()

# n = Inf mostra todas as linhas, width = Inf mostra todas as colunas (sem truncar por largura do terminal)
print(populacao_com_pct, n = 5, width = Inf)

# Junta população e PIB pelo código do município.
# As colunas de código têm nomes diferentes em cada tabela (codigo_municipio e cod_municipio),
# por isso o "by" mapeia explicitamente qual coluna de cada lado representa a mesma coisa.
# O nome do município também se repete nas duas tabelas, então descartamos o de "pib" antes do join.
populacao_pib <- populacao |>
  left_join(
    pib |> select(-municipio),
    by = c("codigo_municipio" = "cod_municipio")
  ) |>
  as_tibble() # left_join() herda a classe de "populacao" (data.frame); as_tibble() troca para tibble, que imprime melhor

print(populacao_pib, n = 5, width = Inf)

# PIB per capita: o PIB está em mil reais, então multiplicamos por 1000
# antes de dividir pela população, para o resultado ficar em reais por habitante
populacao_pib <- populacao_pib |>
  mutate(pib_per_capita = (pib_mil_reais * 1000) / populacao)

print(populacao_pib, n = 5, width = Inf)

# Renda por habitante de cada região: soma do PIB da região dividida pela soma
# da população da região (não a média do pib_per_capita dos municípios, que
# daria peso igual a cidades grandes e pequenas)
renda_por_regiao <- populacao_pib |>
  left_join(regioes, by = "uf") |>
  group_by(regiao) |>
  summarise(renda_per_capita = sum(pib_mil_reais * 1000) / sum(populacao)) |>
  arrange(desc(renda_per_capita))

grafico_renda <- ggplot(renda_por_regiao, aes(x = reorder(regiao, renda_per_capita), y = renda_per_capita)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() + # deita as barras: a região fica no eixo y, o valor no eixo x
  labs(
    title = "Renda por habitante, por região",
    subtitle = "PIB total da região / população total da região",
    x = NULL,
    y = "Renda per capita (R$)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ggsave("imagens/renda_por_regiao.png", grafico_renda, width = 8, height = 5, dpi = 150)

# Dispersão: população x renda por habitante, 1 ponto por município.
# Os dois eixos em escala log (municípios vão de centenas a milhões de habitantes,
# e de poucos mil a centenas de mil reais per capita) e alpha baixo para que a
# sobreposição de milhares de pontos não vire uma mancha única
grafico_dispersao <- ggplot(populacao_pib, aes(x = populacao, y = pib_per_capita)) +
  geom_point(alpha = 0.1, color = "#2c7fb8") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "População x renda por habitante dos municípios",
    subtitle = "Cada ponto é um município - escalas logarítmicas",
    x = "População (escala log)",
    y = "Renda per capita, R$ (escala log)"
  ) +
  theme_minimal()

ggsave("imagens/dispersao_populacao_renda.png", grafico_dispersao, width = 8, height = 6, dpi = 150)

# Mesmo gráfico de dispersão, mas em 5 quadros (1 por região). facet_wrap()
# usa por padrão a mesma escala em todos os quadros, então dá para comparar
# a posição e o formato das nuvens de pontos entre regiões.
populacao_pib_regiao <- populacao_pib %>%
  left_join(regioes, by = "uf")

grafico_dispersao_regiao <- ggplot(populacao_pib_regiao, aes(x = populacao, y = pib_per_capita)) +
  geom_point(alpha = 0.1, color = "#2c7fb8") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  facet_wrap(~regiao) +
  labs(
    title = "População x renda por habitante dos municípios, por região",
    subtitle = "Cada ponto é um município - escalas logarítmicas, iguais em todos os quadros",
    x = "População (escala log)",
    y = "Renda per capita, R$ (escala log)"
  ) +
  theme_minimal()

ggsave("imagens/dispersao_populacao_renda_por_regiao.png", grafico_dispersao_regiao, width = 10, height = 7, dpi = 150)

idosos <- read.csv("dados/idosos_municipios.csv")

# Como a parcela de idosos se distribui entre os municípios de cada região.
# Boxplot: a linha do meio da caixa é a mediana, a caixa cobre do 1º ao 3º
# quartil (os 50% centrais dos municípios), e os "bigodes" mostram o alcance
# do restante dos dados - tudo isso de uma vez, por região.
idosos_por_regiao <- populacao %>%
  select(codigo_municipio, uf) %>%
  left_join(idosos, by = c("codigo_municipio" = "cod_municipio")) %>%
  left_join(regioes, by = "uf")

# a mediana de cada região guia a ordem das caixas, da maior para a menor
ordem_regioes <- idosos_por_regiao %>%
  group_by(regiao) %>%
  summarise(mediana = median(pct_idosos)) %>%
  arrange(desc(mediana)) %>%
  pull(regiao)

idosos_por_regiao <- idosos_por_regiao %>%
  mutate(regiao = factor(regiao, levels = ordem_regioes))

grafico_idosos <- ggplot(idosos_por_regiao, aes(x = regiao, y = pct_idosos)) +
  geom_boxplot(fill = "#2c7fb8", alpha = 0.6, outlier.alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Parcela de idosos (65+) nos municípios, por região",
    subtitle = "Ordenado pela mediana da região - Censo 2022",
    x = NULL,
    y = "Parcela de idosos no município"
  ) +
  theme_minimal()

ggsave("imagens/idosos_por_regiao.png", grafico_idosos, width = 8, height = 5, dpi = 150)
