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




png("Incendios_Galicia.png", width = 15, height = 10, units = "cm", res = 300)
print(plot_galicia)
dev.off()
  
png("Incendios_España.png", width = 15, height = 10, units = "cm", res = 300)
print(plot_españa)
dev.off()


#### Preparación dos gráficos ####
gr.galicia.area <- ggplot(galicia3, aes(x = Ano, y = Area/1000, fill = Cuberta)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "bottom") +
  ylab("Área queimada (miles ha)") +
  xlab("Anos")

gr.galicia.area.total <- ggplot(galicia4, aes(x = Ano, y = sup.total/1000)) +
  geom_line() +
  stat_smooth(aes(x = Ano, y = sup.total/1000)) +
  ylab("Área queimada (miles ha)") +
  xlab("Anos")

# "Frond. per.",204827,
# "Coníferas",363337,

gr.galicia.area2 <- ggplot() +
  geom_col(data = galicia3, aes(x = Ano, y = Area/1000, fill = Cuberta)) +
  scale_fill_manual(values = c("#00A600", "#A6E64D")) +
  geom_col(data = galicia4[1:7,], aes(x = Ano, y = sup.total/1000), fill = "grey") +
  # stat_smooth(data = galicia4, aes(x = Ano, y = sup.total/1000), se = FALSE, size=.8, lty=2) +
  ylab("Área queimada (miles ha)") +
  xlab("Anos") +
  theme_light() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.88,.91)) +
  theme(legend.background = element_blank()) +
  theme(legend.text = element_text(size = 7)) 

gr.galicia.num <- ggplot(galicia4, aes(x = Ano, y = numero/1000)) +
  geom_line() +
  stat_smooth(aes(x = Ano, y = numero/1000)) +
  ylab("Núm. incendios (miles)") +
  xlab("Anos")


gr.españa.num <- ggplot(españa, aes(x = Ano, y = numero/1000)) +
  geom_line() +
  stat_smooth(aes(x = Ano, y = numero/1000)) +
  ylab("Núm. incendios (miles)") +
  xlab("Anos")

gr.españa.area <- ggplot(españa3, aes(x = Ano, y = Area/1000, fill = Cuberta)) +
  geom_bar(stat = "identity") + 
  stat_smooth(aes(x = Ano, y = sup.total/1000), data = españa4, method="loess", se = FALSE, inherit.aes = FALSE) +
  theme(legend.position = "bottom") +
  ylab("Área queimada (miles ha)") +
  xlab("Anos")


#### Xeneración dos gráficos ####

pdf("IncendiosGalicia.pdf", height = 7*9/16, width = 7)
print(gr.galicia.area)
print(gr.galicia.area.total)
print(gr.galicia.area2)
print(gr.galicia.num)
dev.off()


pdf("Incendios_ForoEconomico.pdf", width = 15/2.54, height = 10/2.54)
print(gr.galicia.area2)
dev.off()


pdf("IncendiosEspaña.pdf", height = 7*9/16, width = 7)
print(gr.españa.area)
print(gr.españa.num)
dev.off()
