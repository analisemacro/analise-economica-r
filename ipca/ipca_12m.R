# IPCA mensal e acumulado em 12 meses via SGS/BCB - série 433, desde 2010
source("baixar_sgs.R")
library(ggplot2)

# Começa em fev/2009 para ter 12 meses completos já em jan/2010
ipca <- baixar_sgs(433, inicio = "2009-02-01")

# Acumulado em 12 meses: produto de (1 + taxa/100) nos últimos 12 meses
fator <- 1 + ipca$valor / 100
ipca$acum12 <- (exp(stats::filter(log(fator), rep(1, 12), sides = 1)) - 1) * 100

ipca <- subset(ipca, data >= as.Date("2010-01-01"))
write.csv(ipca, "dados/ipca_12m.csv", row.names = FALSE)

g <- ggplot(ipca, aes(x = data)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = valor, color = "Mensal"), linewidth = 0.6) +
  geom_line(aes(y = acum12, color = "Acumulado em 12 meses"), linewidth = 1.1) +
  scale_color_manual(
    values = c("Mensal" = "grey60", "Acumulado em 12 meses" = "#c0392b"),
    breaks = c("Acumulado em 12 meses", "Mensal"), name = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IPCA - variação mensal e acumulada em 12 meses (%)",
    x = NULL, y = "%",
    caption = "Fonte: IBGE, via BCB/SGS (série 433)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g)
ggsave("imagens/ipca_12m.png", g, width = 10, height = 5, dpi = 150)
