# Sazonalidade do IPCA mensal via SGS/BCB - série 433, 2015 a 2025
source("baixar_sgs.R")
library(ggplot2)

ipca <- baixar_sgs(433, inicio = "2015-01-01", fim = "2025-12-31")
ipca$ano   <- as.integer(format(ipca$data, "%Y"))
ipca$mes   <- as.integer(format(ipca$data, "%m"))

write.csv(ipca, "dados/ipca_sazonal.csv", row.names = FALSE)

meses <- c("jan", "fev", "mar", "abr", "mai", "jun",
           "jul", "ago", "set", "out", "nov", "dez")

g <- ggplot(ipca, aes(x = mes, y = valor, color = ano, group = ano)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  scale_color_viridis_c(option = "plasma", end = 0.9,
                        breaks = seq(2015, 2025, 2), name = NULL) +
  labs(
    title = "Sazonalidade do IPCA - variação mensal (%), 2015 a 2025",
    x = NULL, y = "% a.m.",
    caption = "Fonte: IBGE, via BCB/SGS (série 433)"
  ) +
  theme_minimal() +
  theme(legend.key.height = unit(1.5, "cm"))

print(g)
ggsave("imagens/ipca_sazonal.png", g, width = 10, height = 5, dpi = 150)
