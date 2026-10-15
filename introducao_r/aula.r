populacao <- 203080756
populacoes <- c(11451999, 6211223, 2817381, 2703391, 2428920)
municipios <- c("São Paulo", "Rio de Janeiro", "Brasília", "Fortaleza", "Salvador")

info_cidade <- list(nome = "São Paulo", populacao = 11451999, capital = FALSE)
tabela <- data.frame(municipio = municipios, populacao = populacoes)

media_populacao <- mean(tabela$populacao)

populacoes_com_na <- c(11451999, 6211223, NA, 2703391, 2428920)
mean(populacoes_com_na)                  # retorna NA
mean(populacoes_com_na, na.rm = TRUE)    # ignora o NA e calcula a média dos demais

tabela_com_na <- data.frame(municipio = municipios, populacao = populacoes_com_na)
sum(is.na(tabela_com_na))   # total de NAs na tabela inteira
