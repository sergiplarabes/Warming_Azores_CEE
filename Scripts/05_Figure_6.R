
## Figure 6 ####
rm(list = ls())

library(tidyverse)
library(ggh4x)
library(wesanderson)
library(scales)
#Loading data to Plot
df.res.dom <- readRDS("Data/Dominance_J_6_4period_99runs.Rds")
df.res.div <- readRDS("Data/Beta_coverage_1000_paper.RDS")

# Define color palettes
period_col_10 <- wes_palette("Zissou1", 18, type = c("continuous"))[c(1,3,5,7,13,17,14,15,16,18)]

# Join data frames to plot
DF.div <- rbind(df.res.dom, df.res.div) %>% 
  mutate(Diversity = as.factor(Diversity),
         set = as.factor(set)) %>% 
  mutate(period = fct_recode(period, "1968-1979" = "(1968,1979]", "1979-1989" = "(1979,1989]",
                             "1989-2000" = "(1989,2000]", "2000-2010"  = "(2000,2010]")) %>% 
  mutate(period = factor(period, levels = c( "1830-1860", "1860-1890", "1890-1920", "1920-1950", 
                                             "1950-1980", "1980-2010", "1968-1979", "1979-1989",
                                             "1989-2000", "2000-2010")))

# Plots all diversity metrics

p <- DF.div %>% 
  mutate(Diversity = factor(Diversity, levels= c("dominance", "evenness", 
                                                 "gamma", "alpha", "beta"))) %>% 
  ggplot(aes(period, value)) +
  geom_boxplot(aes(fill = period), alpha = .5, linewidth = 0.3)+
  scale_y_continuous(expand = expansion(mult=c(0.2,0.2))) +
  stat_summary(fun.data=mean_sdl,  
               geom="pointrange", color="grey50", size = .2)+
  facet_grid(Diversity ~ set,  scales="free" ) +
  scale_fill_manual(values = (period_col_10))+
  force_panelsizes(cols = c(1, 0.7)) +
  labs(x ="Time Period", y = "Diversity value") +
  theme_bw(base_size = 7) +
  theme(legend.position = "none")


# New facet label names for supp variable
set.labs <- c("a) 4 lakes 30-year intervals", "b) 5 lakes 10-year intervals")
names(set.labs) <- c("4lakes_6periods", "5lakes_4periods")

# Create the plot
plot <- p + facet_grid(scales="free",
                       Diversity ~ set, 
                       labeller = labeller(set = set.labs)) +
  force_panelsizes(cols = c(1, 0.7)) +
  theme(strip.text.x = element_text(
    size = 7, color = "black"))

ggsave("Plots/figure_6.pdf",  plot = plot, width = 180, height = 140, units = "mm", dpi = "retina")


plot <- p + facet_grid(scales="free",
                       Diversity ~ set, 
                       labeller = labeller(set = set.labs)) +
  force_panelsizes(cols = c(1, 0.7)) +
  theme(strip.text.x = element_text(
    size = 6, color = "black")) +
  theme_bw(base_size = 6)+
  theme(legend.position = "none")

ggsave("Plots/figure_6b.pdf",  plot = plot, width = 88, height = 140, units = "mm", dpi = "retina")

