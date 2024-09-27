# Calculate Species gains and losses per periods ####
# Figure 7 ###
rm(list = ls())

# Loading libraries
library(codyn)
library(vegan)
library(foreach)
library(tidyverse)

## Functions for Figure 7 Species losses and gains using Codyn library
source("Functions/Figure_7_functions.R")

# Upload the Diatoms sediment records data set from São Miguel Island ####
DF.dt.counts <- readRDS("Data/Diatoms_SaoMiguel_Data.RDS") %>% 
  select(-c(rel_abund)) %>% 
  pivot_wider(values_from = counts, names_from = taxon_code,
              values_fill = 0)  %>% 
  filter(ID != "AZ06_28.25") %>% 
  column_to_rownames("ID") %>% 
  filter(Age.ce >= 1830) %>% 
  filter(Age.ce <= 2011) %>%
  mutate(period2 = cut_interval(Bot.ce,  17, dig.lab = 4)) %>% 
  relocate(period2, .after = Age.int )

## Calculation for the 30-years intervals ####    
df.dt1 <- DF.dt.counts %>% 
# Remove Furnas, the shortes record
    filter(lake != "Furnas") %>% 
   droplevels()

# Table of available samples per period and lake
with(df.dt1,table(period,lake))


# The following function is resampling the dataset at a regional scale to have 
#a measure of variability (see Figure_7_functions)
set.seed(123)

result.df <- codyn_input_data(df.dt1, R = 100)

## Calculate Gains and losses with Codyn library functions ####
df_turn <- calculate_turnover_metrics(result.df)

# Prepare data to plot in a wide format
DF.wide_30 <- result.df %>% select(Age.ce.m:period) %>% 
  distinct() %>% 
  right_join(df_turn, by = "Age.ce.m") %>% 
  select(-AgeT.p, -AgeB.p) %>% 
  # Pivot the data wider to have separate columns for Appearance, Disappearance, and Turnover
  pivot_wider(
    names_from = metric, 
    values_from = c(mean, sd),
    names_glue = "{metric}_{.value}"  # Custom names: metric_mean, metric_sd
  ) %>% 
  mutate(set = "30-years")

## Calculation for the 10-years intervals ####    
df.dt1 <- DF.dt.counts %>%
  #filter(lake != "Furnas") %>% 
  filter(Age.ce >= 1969) %>% 
  droplevels() %>% 
  select(-period) %>% 
  rename(period = period2) %>% 
  # Change period anotation to have a correct function input
  mutate(period = fct_recode(period, "1968-1979" = "(1968,1979]", "1979-1989" = "(1979,1989]",
                             "1989-2000" = "(1989,2000]", "2000-2010"  = "(2000,2010]"))

with(df.dt1,table(period,lake))

# The following function is resampling the dataset at a regional scale to have 
#a measure of variability (see Figure_7_functions)
set.seed(123)

result.df <- codyn_input_data(df.dt1, R = 100)

## Calculate Gains and losses with Codyn library functions ####
df_turn <- calculate_turnover_metrics(result.df)

# Prepare data to plot in a wide format
DF.wide_10 <- result.df %>% select(Age.ce.m:period) %>% 
  distinct() %>% 
  right_join(df_turn, by = "Age.ce.m") %>% 
  select(-AgeT.p, -AgeB.p) %>% 
  # Pivot the data wider to have separate columns for Appearance, Disappearance, and Turnover
  pivot_wider(
    names_from = metric, 
    values_from = c(mean, sd),
    names_glue = "{metric}_{.value}"  # Custom names: metric_mean, metric_sd
  ) %>% 
  mutate(set = "10-years")

## Join both results to plot ####

DF.final <- bind_rows(DF.wide_30, DF.wide_10)
saveRDS(DF.final, file = "Data/Data_gains_losses.RDS" )

# Figure_7 ####

library(ggrepel)
library(patchwork)
library(wesanderson)
library(tidyverse)
# Set Colors palettes

period_col_5 <- wes_palette("Zissou1", 18, type = c("continuous"))[c(1,5,7,13,17)]
period_col_3 <- wes_palette("Zissou1", 18, type = c("continuous"))[c(14,16,18)]

# load data to plot

DF_data <- readRDS("Data/Data_gains_losses.RDS" )

# Plots
# 30 years intervals plot
to_plot <- DF_data %>% 
  filter(set == "30-years")

p1 <- to_plot %>% ggplot(aes(Disappearance_mean, Appearance_mean, factor = period)) +
  geom_point(aes(color = period), size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "grey60")+
  coord_cartesian(xlim = c(0.1,0.3), ylim = c(0.1,0.3))+
  geom_errorbarh(aes(xmin = Disappearance_mean-Disappearance_sd, 
                     xmax = Disappearance_mean+Disappearance_sd, color = period))+
  geom_errorbar(aes(ymin=Appearance_mean-Appearance_sd, 
                  ymax=Appearance_mean+Appearance_sd, color = period))+
  scale_color_manual(values = period_col_5)+
   ylab("Proportion of gains")+
  xlab("Proportion of losses")+
  geom_text_repel(aes(Disappearance_mean, Appearance_mean, label = period),
                  point.padding = .25,force = 1, size = 2)+
  theme_minimal(base_size = 7)+
  theme(legend.position="none")

# 10 years intervals plot

to_plot <- DF_data %>% 
  filter(set == "10-years")

p2 <- to_plot %>% ggplot(aes(Disappearance_mean, Appearance_mean, factor = period)) +
  geom_point(aes(color = period), size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "grey60")+
  coord_cartesian(xlim = c(0.1,0.3), ylim = c(0.1,0.3))+
  geom_errorbarh(aes(xmin = Disappearance_mean-Disappearance_sd, 
                     xmax = Disappearance_mean+Disappearance_sd, color = period))+
  geom_errorbar(aes(ymin=Appearance_mean-Appearance_sd, 
                    ymax=Appearance_mean+Appearance_sd, color = period))+
  scale_color_manual(values = period_col_3)+
  ylab("Proportion of gains")+
  xlab("Proportion of losses")+
  geom_text_repel(aes(Disappearance_mean, Appearance_mean, label = period),
                  point.padding = .25,force = 1, size = 2)+
  theme_minimal(base_size = 7)+
  theme(legend.position="none")

# Join plots

plot <- p1 + p2 +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 7, face = "bold"))


ggsave("Plots/figure_7.pdf",  plot = plot, width = 180, height = 90, units = "mm", dpi = "retina")

