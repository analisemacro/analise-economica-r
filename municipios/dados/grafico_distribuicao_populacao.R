# Instala os pacotes só se ainda não estiverem no computador
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}

library(ggplot2)   # gramática de gráficos: monta o histograma
library(patchwork) # junta dois gráficos ggplot lado a lado com "+"

populacao <- read.csv("dados/populacao_censo2022.csv")

# Histograma com eixo x em escala logarítmica: cada intervalo do eixo
# multiplica o anterior (1.000 -> 10.000 -> 100.000...), em vez de somar
# um valor fixo. Isso "espalha" os municípios pequenos, que na escala
# normal ficariam todos amontoados perto do zero.
grafico_log <- ggplot(populacao, aes(x = populacao)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", color = "white") +
  scale_x_log10(labels = scales::comma) + # log na escala + números sem notação científica
  labs(
    title = "Escala logarítmica",
    x = "População (escala log)",
    y = "Número de municípios"
  ) +
  theme_minimal()

# Mesmo histograma, mas com o eixo x "cru" (escala linear). Serve de
# comparação: mostra o problema que a escala log resolve.
grafico_bruto <- ggplot(populacao, aes(x = populacao)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", color = "white") +
  scale_x_continuous(labels = scales::comma) + # escala linear normal
  labs(
    title = "Escala original (dados brutos)",
    x = "População",
    y = "Número de municípios"
  ) +
  theme_minimal()

# O "+" do patchwork posiciona os dois gráficos lado a lado numa imagem só
grafico <- grafico_bruto + grafico_log +
  plot_annotation(
    title = "Distribuição da população dos municípios brasileiros",
    subtitle = "Censo 2022"
  )

# Salva a imagem final em disco (largura maior por ter 2 gráficos lado a lado)
ggsave("imagens/distribuicao_populacao.png", grafico, width = 13, height = 5, dpi = 150)
