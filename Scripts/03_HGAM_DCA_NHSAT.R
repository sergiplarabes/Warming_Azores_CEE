
# HGAM model DCA ~ NHSAT #
# Figure 5 with HGAM results plots #
# Supplementary figure 2 #

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
period_col_6 <- wes_palette("Zissou1", 16, type = c("continuous"))[c(1,3,5,7,11,15)]
# Define colrs for 5 lakes
lakes_col <- scico(5, begin = 0, end = 0.95, palette = 'roma', categorical = F)
# Define colors for derivative significance
color_mapping <- scico(2, begin = 0, end = 0.9, palette = 'vik', categorical =F)

# Upload data
df_nh <- readRDS("Data/DCA_meteo.RDS") %>% 
  tidyr::drop_na(NHanomaly) # Remove NA values (NHSAT time series starts in 1850)


# HGMA Model ####

mod_nh_GI2 <- gam(DCA1 ~ s(NHanomaly, bs = "tp", k = 35, m = 2) +
                    s(NHanomaly, by = lake, k = 10, m = 1, bs = "tp") +
                    s(lake, bs = "re", k = 5),
                  data = df_nh, method = "REML")

# best model
mod_nh <- mod_nh_GI2

summary(mod_nh)

# Model diagnostics

k.check(mod_nh, n.rep=400)

appraise(mod_nh, method = "simulate")

## Plot the model data ####

# get the model data to plot
# evaluate the smooths
sm_nh <- smooth_estimates(mod_nh) %>%
  add_confint()

# add partial residuals to data
df_nh <- df_nh %>%
  add_partial_residuals(mod_nh)

## Plot for "Island-Scale" response ( updated for the dev version of gratia) ####

p_NH.g <- sm_nh %>%
  filter(.smooth == "s(NHanomaly)") %>%
  ggplot() +
  geom_vline(xintercept = 0, lwd = 0.5, color = "grey")+
  geom_hline(yintercept = 0, lwd = .5, color = "grey")+
  geom_rug(aes(x = NHanomaly),
           data = df_nh,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = NHanomaly),
              alpha = 0.2) +
  geom_point(aes(x = NHanomaly, y = `s(NHanomaly)`, color = period), 
             data = df_nh, alpha = 0.8, size = 2.5) +
  geom_line(aes(x = NHanomaly, y = .estimate), lwd = 1.2) +
  labs(y = "Partial effect in DCA", x = "NH Temperature Anomaly ºC") +
  scale_x_continuous(n.breaks = 10, limits = c(-0.6, 0.9))+
  #scale_x_continuous(n.breaks = 8)+
  scale_color_manual("Time-Interval", values = period_col_6) +
  theme_classic(base_size = 7)+
  theme( panel.grid = element_blank())+
  theme(
    legend.position = c(.22, .96),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.title = element_text(size=6, face = "bold"),
    legend.text = element_text(size=6),
    legend.key.size = unit(0.4, "cm")
  )

p_NH.g.hist <-ggMarginal(p_NH.g,  color= "grey", size=4, alpha = 1, groupFill = T, type = "histogram")

# data slice evenly over NHanomaly and lake
ds_nh <- data_slice(mod_nh, NHanomaly = evenly(NHanomaly, n = 500),
                    lake = evenly(lake))

# NH fitted values
fv_nh <- fitted_values(mod_nh, data = ds_nh, scale = "response")

# plot the fitted values
p.fv.nh <- fv_nh |>
  ggplot(aes(x = NHanomaly, y = .fitted, group = lake, colour = lake)) +
  geom_point(data = df_nh, mapping = aes(y = DCA1)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                  colour = NULL), alpha = 0.1) +
  geom_line()+
  labs(y = "DCA axis-1", x = "Temperature Anomaly ºC")+
  scale_x_continuous(n.breaks = 10, limits = c(-0.6, 1.1)) +
  scale_color_manual("Lake", values = lakes_col)+
  scale_fill_manual("Lake", values = lakes_col)+
  theme_minimal(base_size = 12) +
  theme( panel.grid.minor.y = element_blank())+
  theme(legend.position = "top", legend.title = element_text(size=10, face = "bold"))+
  scale_y_continuous(n.breaks = 6)


## lake scale response derivatives ####
resp_dev_nh <- response_derivatives(mod_nh, data = ds_nh, focal = "NHanomaly",
                                                    eps = 0.01, seed = 42)

# plot the response derivatives Figure 2_Supplementary (lake scale)
p.resp_dev_nh <- resp_dev_nh |>
  ggplot(aes(x = NHanomaly, colour = lake, y = .derivative)) +
  geom_ribbon(aes(x = NHanomaly, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                  colour = NULL), alpha = 0.1) +
  geom_line() +
  labs(y = "Derivative", x = "Temperature Anomaly ºC") +
  scale_color_manual("Lake", values = lakes_col) +
  scale_fill_manual("Lake", values = lakes_col) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", legend.title = element_text(size=10, face = "bold")
  ) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.6, 1))+
  scale_y_continuous(n.breaks = 5)


## derivatives of the common smooth, Island-Scale ####
mod_nh_dev <- derivatives(mod_nh, select = "s(NHanomaly)", eps = 0.01,
                          interval = "simultaneous", seed = 22)

NH.derivative <- mod_nh_dev |>
  draw() +
  ggtitle(element_blank())+
  geom_hline(yintercept = 0, linewidth = .5, color = "grey50")+
  geom_vline(xintercept = 0, linewidth = .5, color = "grey50")+
  theme_classic (base_size = 12) +
  scale_x_continuous(n.breaks = 10, limits = c(-0.6, 0.9))+
  scale_y_continuous(n.breaks = 5)+
  geom_line(aes(x = NHanomaly, y = .derivative, color = (.lower_ci) > 0 | (.upper_ci) < 0, 
                linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1), show.legend = FALSE) +
  scale_discrete_manual("linewidth", values = c(0.5, 1.5)) +
  labs(x = "NH Temperature Anomaly ºC")+
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


# Resume statistics
# mod_nh_dev %>% filter(lower > 0) %>% 
#   summarise(mean_ = mean(data, na.rm = T),
#             medi_ = median(data, na.rm = T),
#             max_  = max(data, na.rm = T),
#             min_  = min(data, na.rm = T))


# FIGURES ####
## Figure 5 ####

library("grid")
library("ggplotify")

p1 <- as.ggplot(p_NH.g.hist)

p2 <- as.ggplot(NH.derivative + theme_classic(base_size = 7))

 p.inset <- p1 + inset_element(p2, 0.52, 0.089, 0.81, 0.32) # (left, bottom, right, top )

p.inset

ggsave("Plots/figure_5.pdf",  plot = p.inset, width = 180, height = 180, units = "mm", dpi = "retina")

## Supplementary Figure 2 ####

Figure2 <- p.fv.nh + p.resp_dev_nh + plot_annotation(tag_levels = 'a')

ggsave("Plots/Sup_figure_2.png",  width = 22, height = 12, units = "cm", dpi = 300)
