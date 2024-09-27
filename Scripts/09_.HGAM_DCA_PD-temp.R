# HGAM model DCA ~ Ponta Delgaf¡da Air Temperature ####
# Supplementary Figure 3  with Ponta delgada Instrumental record ####

rm(list = ls())

# Upload libraries
library(tidyverse)
library(mgcv)
library(gratia)
library(ggExtra)
library(patchwork)
library("wesanderson")
library("scico")
library(scales)

# Color Palettes ####
# Define Colors Time interval Periods
period_col_5 <- wes_palette("Zissou1", 16, type = c("continuous"))[c(3,5,7,11,15)]
# Define colrs for 5 lakes
lakes_col <- scico(5, begin = 0, end = 0.95, palette = 'roma', categorical = F)
# Define colors for derivative significance
color_mapping <- scico(2, begin = 0, end = 0.9, palette = 'vik', categorical =F)

# Upload data
df_ta <- readRDS("Data/DCA_meteo.RDS") %>% 
  tidyr::drop_na(Tanomaly) # Remove NA values (NHSAT time series starts in 1850)


# HGMA Model ####

mod_ta_GI2 <- gam(DCA1 ~ s(Tanomaly, bs = "tp", k = 35, m = 2) +
                    s(Tanomaly, by = lake, k = 10, m = 1, bs = "tp") +
                    s(lake, bs = "re", k = 5),
                  data = df_ta, method = "REML")

# best model
mod_ta <- mod_ta_GI2

summary(mod_ta)

# Model diagnostics 
#Check the model
k.check(mod_ta, n.rep=400)

# Model diagnostics
appraise(mod_ta, method = "simulate")


## Plot the model data ####

# get the model data to plot
# evaluate the smooths
sm_ta <- smooth_estimates(mod_ta) %>%
  add_confint()

# add partial residuals to data
df_ta <- df_ta %>%
  add_partial_residuals(mod_ta)

### Plot for "Island-Scale" response ( updated for the dev version of gratia) ####

p_TA.g <- sm_ta %>%
  filter(.smooth == "s(Tanomaly)") %>%
  ggplot() +
  geom_vline(xintercept = 0, lwd = 0.5, color = "grey")+
  geom_hline(yintercept = 0, lwd = .5, color = "grey")+
  geom_rug(aes(x = Tanomaly),
           data = df_ta,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Tanomaly),
              alpha = 0.2) +
  geom_point(aes(x = Tanomaly, y = `s(Tanomaly)`, color = period), 
             data = df_ta, alpha = 0.8, size = 2.5) +
  geom_line(aes(x = Tanomaly, y = .estimate), lwd = 1.2) +
  labs(y = "Partial effect in DCA", x = "Ponta Delgada Temperature Anomaly ºC") +
  # scale_x_continuous(n.breaks = 10, limits = c(-0.6, 0.9))+
  scale_x_continuous(n.breaks = 6)+
  scale_color_manual("Time-Interval", values = period_col_5) +
  theme_minimal(base_size = 9) + 
  theme( panel.grid = element_blank())+
  theme(
    legend.position = c(.22, .96),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2)
  )

p_TA.g.hist <-ggMarginal(p_TA.g,  color= "grey", size=4, alpha = 1, groupFill = T, type = "histogram")


# derivatives of the common smooth, Island-Scale ####
mod_ta_dev <- derivatives(mod_ta, select = "s(Tanomaly)", eps = 0.01,
                          interval = "simultaneous", seed = 22)

TA.derivative <- mod_ta_dev |>
  draw() +
  ggtitle(element_blank())+
  geom_hline(yintercept = 0, linewidth = .5, color = "grey50")+
  geom_vline(xintercept = 0, linewidth = .5, color = "grey50")+
  theme_classic (base_size = 12) +
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 5)+
  geom_line(aes(x = Tanomaly, y = .derivative, color = (.lower_ci) > 0 | (.upper_ci) < 0, 
                linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1), show.legend = FALSE) +
  scale_discrete_manual("linewidth", values = c(0.5, 1.5)) +
  labs(x = "Ponta Delgada Air temperature Anomaly ºC")+
  scale_color_manual(values = color_mapping) +
  theme( panel.grid.minor = element_blank())+
  theme( panel.grid.minor.x = element_blank())+
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))+
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  theme(axis.line=element_line(color="black"))


# FIGURES ####
## Supplementary Figure 3 ####

library("grid")
library("ggplotify")

p1 <- as.ggplot(p_TA.g.hist)

p2 <- as.ggplot(TA.derivative + theme_classic(base_size = 7))

p.inset <- p1 + inset_element(p2, 0.5, 0.07, 0.818, 0.29) # (left,bottom,right, top)

p.inset

#Save Paper Supplementary Figure 3
ggsave("Plots/Sup_figure_3.png", width = 22, height = 22, units = "cm", dpi = "retina")

