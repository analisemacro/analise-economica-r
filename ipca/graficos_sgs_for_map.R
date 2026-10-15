# Gráfico de linha para IPCA, SELIC e dólar: a mesma tarefa com for e com map()
source("baixar_sgs.R")
library(ggplot2)
library(purrr)

series <- data.frame(
  nome    = c("ipca", "selic", "dolar"),
  codigo  = c(433, 4189, 3698),
  titulo  = c("IPCA - variação mensal",
              "SELIC acumulada no mês, anualizada",
              "Dólar comercial (venda) - média mensal"),
  unidade = c("% a.m.", "% a.a.", "R$/US$")
)

# Baixa uma série, monta o gráfico de linha e salva o PNG
grafico_linha <- function(nome, codigo, titulo, unidade) {
  dados <- baixar_sgs(codigo, inicio = "2010-01-01")

  g <- ggplot(dados, aes(x = data, y = valor)) +
    geom_line(color = "#1f3b73", linewidth = 0.8) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title = titulo, x = NULL, y = unidade,
      caption = paste0("Fonte: BCB/SGS (série ", codigo, ")")
    ) +
    theme_minimal()

  ggsave(paste0("imagens/linha_", nome, ".png"), g, width = 10, height = 5, dpi = 150)
  g
}

# Versão 1: for
# É preciso criar a lista vazia antes e preencher posição por posição
tempo_for <- system.time({
  graficos_for <- vector("list", nrow(series))
  names(graficos_for) <- series$nome
  for (i in seq_len(nrow(series))) {
    graficos_for[[i]] <- grafico_linha(series$nome[i], series$codigo[i],
                                       series$titulo[i], series$unidade[i])
  }
})

# Versão 2: map()
# pmap() percorre as linhas de series e já devolve a lista pronta
tempo_map <- system.time({
  graficos_map <- pmap(series, grafico_linha)
  names(graficos_map) <- series$nome
})

# Comparação: os dois caminhos devem chegar aos mesmos dados
iguais <- map_lgl(series$nome, \(n) identical(graficos_for[[n]]$data, graficos_map[[n]]$data))
names(iguais) <- series$nome
print(iguais)

cat("\nTempo do for:  ", round(tempo_for["elapsed"], 2), "s\n")
cat("Tempo do map():", round(tempo_map["elapsed"], 2), "s\n")

walk(graficos_map, print)
