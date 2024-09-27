# Supplemnetary Figure 5 HGAM model using Ponta Delgada 
# air temperature and Precipitation instrumental record

# Loading library
library("mgcv")
library("wesanderson")
library(tidyverse)
library(readxl)
library(gratia)
library("scico")
library(scales)

# Color Palettes ####
# Define Colors Time interval Periods
period_col_5 <- wes_palette("Zissou1", 16, type = c("continuous"))[c(3,5,7,11,15)]
# Define colrs for 5 lakes
lakes_col <- scico(5, begin = 0, end = 0.95, palette = 'roma', categorical = F)
# Define colors for derivative significance
color_mapping <- scico(2, begin = 0, end = 0.9, palette = 'vik', categorical =F)

# Loading data
df <- readRDS("Data/DCA_meteo.RDS") %>% 
  tidyr::drop_na(Tanomaly) # Remove NA values (Temperature time series starts in 1850)

# Models GI ####

## Model Global I with interaction ####

mod_to <-gam (DCA1 ~ s(Tanomaly, bs = "tp", k = 40,  m = 2) +
                         s(Tanomaly, by = lake, k = 5, m = 1, bs = "tp") +
                         s(Panomaly, bs = "tp", k = 60, m = 2) +
                         s(Panomaly, by = lake, k = 12, m = 1, bs = "tp") +
                         s(lake, bs = "re"),
                       data = df, method = "REML")
# Summary
summary(mod_to)

#Model diagnostics
k.check(mod_to, n.rep=500)

# 
appraise(mod_to, method = "simulate") 


vis.gam(mod_sm.all_GI.2, theta = 210, n.grid = 50, lwd = 0.4, ticktype="detailed",
        view = c("Tanomaly", "Panomaly"), color = "topo")
vis.gam(mod_sm.all_GI.1, theta = 230, n.grid = 50, lwd = 0.4, ticktype="detailed",
        view = c("Tanomaly", "Panomaly"))
vis.gam(mod_sm.all_GI, theta = 230, n.grid = 50, lwd = 0.4, ticktype="detailed",
        view = c("Tanomaly", "Panomaly"))

library(patchwork)

p1 <- draw(mod_to, select = "s(Tanomaly)", residuals = TRUE, nrow = 1) + theme_classic()
p2 <- draw(mod_to, select = "s(Panomaly)", residuals = TRUE, nrow = 1) + theme_classic()
p3 <- wrap_elements(panel = ~vis.gam(mod_to, theta = 210, n.grid = 50, lwd = 0.4, ticktype="detailed",
                                     view = c("Tanomaly", "Panomaly"), type = "response", color = "topo"))


plot <- p1+ p2 + p3 +  plot_layout(widths = c(2,2,3)) + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 20))
# Export plot using the export option in R


# Optionally add vis.gam graphic and wrap_elemnts from pachwork if the formula (requires gridGraphics package)
if (requireNamespace("gridGraphics", quietly = TRUE)) {
  p1 + p2 + wrap_elements(full = ~ vis.gam(mod_to, theta = 210, n.grid = 50, lwd = 0.4, ticktype="detailed",
                                      view = c("Tanomaly", "Panomaly"), type = "response", color = "topo"))
  
}

ggsave("Plots/Sup_figure_5.png", width = 40, height = 20, units = "cm", dpi = 300)



