# HGAM modeling trends in DCA #####
# Figure 3 ####

rm(list = ls())

# Upload libraries
library(tidyverse)
library(mgcv)
library(gratia)

library("scico")
library(scales)
# Color Palettes

lakes_col <- scico(5, begin = 0, end = 0.95, palette = 'roma', categorical = F)

# Upload data 
DF_to <- readRDS("Data/DCA_meteo.RDS")

# HGMA Model ####

mod_toGI2 <- gam(DCA1 ~ s(year_CE, bs = "tp", k = 25) +
                   s(year_CE, by = lake, k = 20, m = 1, bs = "tp") +
                   s(lake, bs = "re"),
                 data = DF_to, method = "REML")

# turnover model to use
mod_to <- mod_toGI2

summary(mod_to)
# Check the model
k.check(mod_to, n.rep=400)

# Model diagnostics
appraise(mod_to)

# get the model data to plot
# evaluate the smooths
sm_to <- smooth_estimates(mod_to) %>%
  add_confint()

# add partial residuals to data --- not working
df <- DF_to %>%
  add_partial_residuals(mod_to)

# Plot updated for the dev version of gratia
p.time.GI2 <- sm_to %>%
  filter(.smooth == "s(year_CE)") %>%
  ggplot() +
  geom_rug(aes(x = year_CE),
           data = df,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = year_CE),
              alpha = 0.2) +
  geom_point(aes(x = year_CE, y = `s(year_CE)`, color = lake), 
             data = df, alpha = 0.8, size = 1.5) +
  geom_line(aes(x = year_CE, y = .estimate), lwd = 1) +
  labs(y = "DCA axis-1", title = "s(year_CE) GI model") +
  scale_color_manual("Lake", values = lakes_col) +
  theme_minimal(base_size = 7) +
  scale_x_continuous(n.breaks = 10, limits = c(1800, 2012)) +
  labs(title = NULL)+
  theme( panel.grid.minor.y = element_blank())+
  theme( legend.position = "none") 

## Plot derivatives ####

# original version updated 0.9 gratia

mod_to_dev <- derivatives(mod_to, select = "s(year_CE)", eps = 1,
                          interval = "simultaneous", seed = 22)

### Plot the "Common" trend derivative ~ Rate of change ###

# Define colors for derivative significance
color_mapping <- scico(2, begin = 0, end = 0.9, palette = 'vik', categorical =F)

p.time.GI2.dev <-  mod_to_dev %>%
  add_sizer(type = "change") %>% 
  ggplot() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = year_CE),
              alpha = 0.2 ) +
  geom_line(aes(x = year_CE, y = .derivative, color = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1, 
                          linewidth = (.lower_ci) > 0 | (.upper_ci) < 0), show.legend = FALSE) +
  scale_discrete_manual("linewidth", values = c(0.5, 1.5)) +
  labs(y = "Derivative")+
  scale_x_continuous(n.breaks = 10, limits = c(1800, 2012))+
  scale_color_manual(values = color_mapping) +
  #scale_color_manual(values = c("black", "red")) +
  theme_minimal(base_size = 7) +
  theme( panel.grid.minor.y = element_blank())

## Response derivative for each group (lake) ####

# Find the min age for the shortest record (Lake Furnas) 
min_furnas <- DF_to |>
  dplyr::filter(lake == "Furnas") |>
  summarise(min_year = min(year_CE)) |>
  pull(min_year)

# create a data slice evenly over the lakes
ds <- data_slice(mod_to, year_CE = evenly(year_CE, n = 500),
                 lake = evenly(lake)) |>
  dplyr::filter(!(lake == "Furnas" & year_CE < min_furnas))

# compute response derivatives
resp_dev <- response_derivatives(mod_to, data = ds, focal = "year_CE",
                                 eps = 10, seed = 42)

### plot the response derivative ####

res.dev.p <- resp_dev |> filter(lake == "Azul") %>% 
  ggplot(aes(x = year_CE, colour = lake, y = .derivative)) +
  geom_ribbon(aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                  colour = NULL), alpha = 0.1) +
  geom_line(aes(x = year_CE, y = .derivative, linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1),
            show.legend = FALSE) +
  scale_discrete_manual("linewidth", values = c(0.5, 1.5)) +
  scale_color_manual("Lake", values = lakes_col) +
  scale_fill_manual("Lake", values = lakes_col) +
  scale_x_continuous(n.breaks = 10, limits = c(1800, 2012))+
  scale_y_continuous(n.breaks = 3) +
  labs(y = "Derivative", x = "year CE") +
  theme_minimal(base_size = 7) +
  theme(legend.position = "none")+
  # Highlight significant intervals for all studied lakes
  geom_line(data = resp_dev |> filter(lake == "Empadadas Norte"), aes(x = year_CE, y = .derivative, linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1),
            show.legend = FALSE)+
  geom_ribbon(data = resp_dev |> filter(lake == "Empadadas Norte"), aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                                                                        colour = NULL), alpha = 0.1) +
  geom_line(data = resp_dev |> filter(lake == "Fogo"), aes(x = year_CE, y = .derivative, linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1),
            show.legend = FALSE)+
  geom_ribbon(data = resp_dev |> filter(lake == "Fogo"), aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                                                             colour = NULL), alpha = 0.1) +
  geom_line(data = resp_dev |> filter(lake == "Furnas"), aes(x = year_CE, y = .derivative, linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1),
            show.legend = FALSE)+
  geom_ribbon(data = resp_dev |> filter(lake == "Furnas"), aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                                                               colour = NULL), alpha = 0.1) +
  geom_line(data = resp_dev |> filter(lake == "Santiago"), aes(x = year_CE, y = .derivative, linewidth = (.lower_ci) > 0 | (.upper_ci) < 0, group = 1),
            show.legend = FALSE)+
  geom_ribbon(data = resp_dev |> filter(lake == "Santiago"), aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci, fill = lake,
                                                                 colour = NULL), alpha = 0.1) 


## Fitted trends for each lake ####
fv <- fitted_values(mod_to, data = ds, scale = "response")

## plot the fitted trends per lake
DCA1.p <- fv |>
  ggplot(aes(x = year_CE, y = .fitted, colour = lake)) +
  geom_point(data = DF_to, mapping = aes(y = DCA1, color = lake), alpha = 0.5, size = 1) +
  geom_ribbon(aes(x = year_CE, ymin = .lower_ci, ymax = .upper_ci,
                  fill = lake,
                  colour = NULL), alpha = 0.2) +
  geom_line()+
  labs(y = "DCA axis-1")+
  scale_x_continuous(n.breaks = 10, limits = c(1800, 2012)) +
  scale_color_manual("Lake", values = lakes_col, labels = c("Azul", "Empadadas N.", "Fogo", "Furnas", "Santiago"))+
  scale_fill_manual("Lake", values = lakes_col, labels = c("Azul", "Empadadas N.", "Fogo", "Furnas", "Santiago"))+
  theme_minimal(base_size = 7) +
  theme( panel.grid.minor.y = element_blank())+
  theme(legend.position = "top", legend.justification="center",
  legend.title = element_text(size=7, face = "bold"),
        legend.key.size = unit(0.4, "cm")
  )

# FIGURES ####
library(scales)

## NHSAT ####

NH_temp_1850 <- read_csv("Data/NHAST.csv") %>%  
  filter(Year >= 1850) %>% 
  filter(Year <=2012)


p.NH <- NH_temp_1850 %>% ggplot(aes(Year, Anomaly, fill = Anomaly)) +
  #geom_point(alpha = 0.5)+
  geom_col(show.legend = F) +
  # scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", midpoint = 0)+
  scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                    values = rescale(c(min(NH_temp_1850$Anomaly), 0, max(NH_temp_1850$Anomaly))),
                    limits = c(min(NH_temp_1850$Anomaly), max(NH_temp_1850$Anomaly)),
                    n.breaks = 6) +
  scale_x_continuous(n.breaks = 10, limits = c(1800, 2012)) +
  scale_y_continuous(n.breaks = 5, limits = c(-0.55, 1.01)) +
  theme_minimal(base_size = 7) +
  theme( panel.grid.minor.y = element_blank())+
  expand_limits(x = 1790)+
  xlab(" year CE")+ 
  ylab("NH Anomaly ºC") 


## Building Figure 3 ####

library(patchwork)

p.model <- p.time.GI2 + theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank(),
                              axis.title.x = element_blank())+
  theme(legend.position = "none")

P.DCA <- DCA1.p + theme(axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.title.x = element_blank())
P.dev <- p.time.GI2.dev + theme(axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.x = element_blank())
P.dev.lake <- res.dev.p + theme(axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.x = element_blank())


p <- (P.DCA) / plot_spacer() / (P.dev.lake ) / plot_spacer() /(p.model) /plot_spacer() / P.dev / plot_spacer() / (p.NH) + 
  plot_layout(heights = c(4.5, -0.5, 4, -0.5, 4.5, -0.5, 4, -0.5, 4.5)) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 7, hjust = 2, vjust = 3, face = "bold")) +
  theme(plot.margin = unit(c(0,0,0,0.5), "cm"))


ggsave("Plots/figure_3.pdf", plot = p, width = 180, height = 160, units = "mm", dpi = "retina")
#ggsave("Plots/figure_3b.pdf", plot = p, width = 88, height = 140, units = "mm", dpi = "retina")


