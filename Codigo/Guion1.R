## Gráficos da serie de incendios en Galicia
## Eduardo Corbelle, 26 de febreiro de 2024

library(data.table)
library(ggplot2)
library(ggokabeito)
# library(gridExtra)

# Datos orixinais ----
galicia1 <- fread("Datos/DatosGalicia.csv")
galicia2 <- fread("Datos/DatosGalicia2.csv", skip = 6)
españa   <- fread("Datos/DatosEspaña.csv",   skip = 6)


galicia1[ , `:=`(value = numero * sup.media)]
galicia2.area <- melt(galicia2, id.vars = "ano", measure.vars = c("sup.arborada", "sup.rasa"))

españa.area   <- melt(españa, id.vars = "ano", measure.vars = c("sup.arborada", "sup.rasa"))


plot_galicia <- ggplot(galicia2.area, aes( x = ano, y = value/1e3)) +
  geom_col(mapping = aes(fill = variable)) +
  scale_fill_okabe_ito(name = "",
                       labels = c("Área arborada", "Área rasa")) +
  expand_limits(x = 1960) +
  geom_col(data = galicia1, fill = "darkgrey") +
  geom_line(data = rbind(galicia1[,c("ano", "numero")],
                         galicia2[,c("ano", "numero")]),
            mapping = aes(x = ano, y = numero/1e2, col = "Número"),
            alpha = .4,
            linewidth = .25) +
  geom_point(data = rbind(galicia1[,c("ano", "numero")],
                          galicia2[,c("ano", "numero")]),
             mapping = aes(x = ano, y = numero/1e2, col = "Número"),
             alpha = .6,
             cex = .75,
             pch = 16) +
  scale_color_manual(name = "", breaks = "Número", 
                     values = c("Número"="black")) +
  ggtitle(label = "Galicia. Número de incendios e área afectada",
          subtitle = "Datos: Estatística de incendios forestais, MITECO") +
  xlab("Ano") +
  ylab("Número (centenares) / Área (miles ha)") +
  theme_light() +
  theme(legend.position = c(.88,.84),
        legend.spacing.y =  unit(-4, "mm"),
        legend.background = element_blank())



plot_españa <- ggplot(españa.area, aes( x = ano, y = value/1e3)) +
  geom_col(mapping = aes(fill = variable)) +
  scale_fill_okabe_ito(name = "",
                       labels = c("Área arborada", "Área rasa")) +
  expand_limits(x = 1960) +
  geom_line(data = españa,
            mapping = aes(x = ano, y = numero/1e2, col = "Número"),
            alpha = .4,
            linewidth = .25) +
  geom_point(data = españa,
             mapping = aes(x = ano, y = numero/1e2, col = "Número"),
             alpha = .6,
             cex = .75,
             pch = 16) +
  scale_color_manual(name = "", breaks = "Número", 
                     values = c("Número"="black")) +
  ggtitle(label = "España. Número de incendios e área afectada",
          subtitle = "Datos: Estatística de incendios forestais, MITECO") +
  xlab("Ano") +
  ylab("Número (centenares) / Área (miles ha)") +
  theme_light() +
  theme(legend.position = c(.88,.84),
        legend.spacing.y =  unit(-4, "mm"),
        legend.background = element_blank())




png("Galicia.png", width = 15, height = 10, units = "cm", res = 300)
print(plot_galicia)
dev.off()
  
png("España.png", width = 15, height = 10, units = "cm", res = 300)
print(plot_españa)
dev.off()

