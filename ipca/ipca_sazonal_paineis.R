# Sazonalidade do IPCA por mês (subseries plot) via SGS/BCB - série 433, 2015 a 2025
source("baixar_sgs.R")
library(ggplot2)

ipca <- baixar_sgs(433, inicio = "2015-01-01", fim = "2025-12-31")
ipca$ano   <- as.integer(format(ipca$data, "%Y"))

meses <- c("jan", "fev", "mar", "abr", "mai", "jun",
           "jul", "ago", "set", "out", "nov", "dez")
ipca$mes <- factor(meses[as.integer(format(ipca$data, "%m"))], levels = meses)

# Média de cada mês no período
medias <- aggregate(valor ~ mes, data = ipca, FUN = mean)

g <- ggplot(ipca, aes(x = ano, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(data = medias, aes(yintercept = valor),
             color = "#c0392b", linewidth = 0.8) +
  geom_line(color = "#1f3b73", linewidth = 0.7) +
  geom_point(color = "#1f3b73", size = 1) +
  facet_wrap(~ mes, nrow = 2) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  labs(
    title = "IPCA - variação mensal (%) por mês do ano, 2015 a 2025",
    subtitle = "Linha vermelha: média do mês no período",
    x = NULL, y = "% a.m.",
    caption = "Fonte: IBGE, via BCB/SGS (série 433)"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "grey85"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))

print(g)
ggsave("imagens/ipca_sazonal_paineis.png", g, width = 11, height = 5.5, dpi = 150)
