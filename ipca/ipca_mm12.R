# IPCA mensal e média móvel de 12 meses via SGS/BCB - série 433, desde 2010
source("baixar_sgs.R")
library(ggplot2)

# Começa em fev/2009 para ter 12 meses completos já em jan/2010
ipca <- baixar_sgs(433, inicio = "2009-02-01")

# Média móvel simples dos últimos 12 meses
ipca$mm12 <- as.numeric(stats::filter(ipca$valor, rep(1 / 12, 12), sides = 1))

ipca <- subset(ipca, data >= as.Date("2010-01-01"))
write.csv(ipca, "dados/ipca_mm12.csv", row.names = FALSE)

ini_pand <- as.Date("2020-03-01")
fim_pand <- as.Date("2021-12-31")

g <- ggplot(ipca, aes(x = data)) +
  annotate("rect", xmin = ini_pand, xmax = fim_pand,
           ymin = -Inf, ymax = Inf, fill = "#f4d03f", alpha = 0.25) +
  annotate("text", x = ini_pand + (fim_pand - ini_pand) / 2,
           y = max(ipca$valor) * 0.97, label = "pandemia",
           color = "grey25", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = valor, color = "Mensal"), linewidth = 0.6) +
  geom_line(aes(y = mm12, color = "Média móvel de 12 meses"), linewidth = 1.1) +
  scale_color_manual(
    values = c("Mensal" = "grey60", "Média móvel de 12 meses" = "#1f3b73"),
    breaks = c("Média móvel de 12 meses", "Mensal"), name = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IPCA - variação mensal e média móvel de 12 meses (%)",
    x = NULL, y = "% a.m.",
    caption = "Fonte: IBGE, via BCB/SGS (série 433)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g)
ggsave("imagens/ipca_mm12.png", g, width = 10, height = 5, dpi = 150)
