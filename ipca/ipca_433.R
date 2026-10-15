# IPCA mensal (IBGE) via SGS/BCB - série 433, desde 2010
source("baixar_sgs.R")
library(ggplot2)

ipca <- baixar_sgs(433, inicio = "2010-01-01")

write.csv(ipca, "dados/ipca_433.csv", row.names = FALSE)

g <- ggplot(ipca, aes(x = data, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "grey60", linewidth = 0.7) +
  geom_smooth(aes(color = "Tendência linear"), method = "lm", se = FALSE, linewidth = 1) +
  geom_smooth(aes(color = "Curva suave (LOESS)"), method = "loess", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("Tendência linear" = "#c0392b", "Curva suave (LOESS)" = "#1f3b73"), name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IPCA - variação mensal (%)",
    x = NULL, y = "% a.m.",
    caption = "Fonte: IBGE, via BCB/SGS (série 433)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(g)
ggsave("imagens/ipca_433.png", g, width = 10, height = 5, dpi = 150)
