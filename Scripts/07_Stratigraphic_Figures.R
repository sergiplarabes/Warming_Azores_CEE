# Diatoms Stratigraphic figures ####
rm(list = ls())

# Loading libraries
library(tidyverse)
library(tidypaleo)
library(patchwork)

# Figure 4 ####

# Diatom Traits #
df.hab <- read_csv("Data/Diatoms_habitat.csv")

dt.new <- readRDS("Data/Diatoms_SaoMiguel_Data.RDS") 
# Check if could be any diatoms on the data without a corresponding trait
# setdiff(dt.new$taxon_code, Traits$taxon_code) 

# join data set and traits

df.wk <- dt.new %>% 
  select(taxon_code:core_id, Age.ce) %>% 
  left_join(df.hab, by = "taxon_code") %>% 
  mutate(Habitat = as.factor(Habitat)) %>% 
  mutate(Habitat = fct_recode(Habitat, "Aulacoseira spp." = "Tychoplankton.AU", 
                              "Fragilarioids" = "Tychoplankton.FR")) %>% 
  group_by(Habitat, ID, lake, Age.ce) %>% 
  summarise(counts = sum(counts)) %>% 
  ungroup() %>%  
  group_by(ID) %>%
  mutate(rel_abund = counts / sum(counts) * 100) %>%
  ungroup() 

Color <- c("orange" , "#ABDDA4", "#66C2A5",  "#5E4FA2"  )
names(Color) <- levels(df.wk$Habitat)

lakes <- levels(df.wk$lake)

## Plotting· ####


## Extract the legend from the list ####
i=1
p <- df.wk %>% 
  filter(lake == lakes[i]) %>% 
  group_by(lake, ID, Habitat, Age.ce) %>% 
  summarise(rel_abund = sum(rel_abund)) %>% 
  ungroup() %>% 
  mutate(Habitat = factor(Habitat, levels = c("Benthos", "Fragilarioids",
                                              "Aulacoseira spp.", "Plankton")))  %>% 
  ggplot(aes(rel_abund, Age.ce)) +
  geom_areah(alpha=0.8, aes(fill=Habitat)) +
  scale_fill_manual(values = Color, name = "Functional Group") +
  #geom_col_segsh(size = 0.3, color = "grey90", alpha= 0.5) +
  facet_abundanceh(vars(Habitat)) +
  #scale_y_continuous(n.breaks=8)
  scale_y_continuous(n.breaks=8, limits = c(1800,2010)) +
  labs(x = NULL, y = "Age CE") +
  theme_minimal(base_size = 7) +
  theme(axis.text.x = element_text(size = 8, angle=45))+
  theme(strip.text.x = element_text(size = 9, angle = 45, hjust = 0, vjust=0)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "left") +
  ggtitle(paste("Lake", lakes[i],"Diatoms"))  +
  theme( plot.title = element_text(color="black", size = 14),# face="italic"),
         axis.title.y = element_text(size = 10)) 



# Extract the legend. Returns a gtable
library(ggpubr)
leg <- get_legend(p)

# Convert to a ggplot and print
p.leg <-as_ggplot(leg)

## Plot loop Only Habitat ####  

plot_list.strat <- list()

for(i in 1:length(lakes)){
  
  p.dt <- df.wk %>% 
    filter(lake == lakes[i]) %>% 
    group_by(lake, ID, Habitat, Age.ce) %>% 
    summarise(rel_abund = sum(rel_abund)) %>% 
    ungroup() %>% 
    mutate(Habitat = factor(Habitat, levels = c("Benthos", "Fragilarioids", "Aulacoseira spp.", "Plankton")))  %>% 
    ggplot(aes(rel_abund, Age.ce)) +
    geom_areah(alpha=0.8, aes(fill=Habitat)) +
    scale_fill_manual(values = Color)+
    #geom_col_segsh(size = 0.3, color = "grey90", alpha= 0.5) +
    facet_abundanceh(vars(Habitat)) +
    #scale_y_continuous(n.breaks=8)
    scale_y_continuous(n.breaks=8, limits = c(1800,2011)) +
    labs(x = NULL, y = "Age CE") +
    theme_minimal(base_size =7) +
    #theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(size = 5, angle=0))+
    theme(strip.text.x = element_text(size = 6, angle = 45, hjust = 0, vjust=0)) +
    theme(legend.position = "none") +
    ggtitle(paste("Lake", lakes[i]))  +
    theme( plot.title = element_text(color="black", size = 7), #face="italic"),
           axis.title.y = element_text(size = 7)) +
    theme(strip.background = element_blank(), strip.text.y = element_blank(),
          strip.text.x = element_blank()) # remove facet labels
  
  plot_list.strat[[i]] <- p.dt 
  
}


# Azul
p1 <- plot_list.strat[[1]] +  geom_hline(yintercept=2000, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1901, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1962, linetype="dashed", color = "grey20", linewidth = .5) +
  ggtitle("Azul (1.27 SD)") + theme(plot.title = element_text(size = 7, face = "bold"))

# Empadadas
p2 <- plot_list.strat[[2]] + geom_hline(yintercept=1982, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1858, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1958, linetype="dashed", color = "grey20", linewidth = .5) +
  ggtitle("Empadadas Norte (1.63 SD)") + theme(plot.title = element_text(size = 7, face = "bold"))
#Fogo
p3 <- plot_list.strat[[3]] + geom_hline(yintercept=1989, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1935, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1880, linetype="dashed", color = "grey20", linewidth = .5)+
  ggtitle("Fogo (1.53 SD)") + theme(plot.title = element_text(size = 7, face = "bold"))

#Furnas
p4 <- plot_list.strat[[4]] + geom_hline(yintercept=1985.5, linetype="dashed", color = "grey20", linewidth = .5) +
  ggtitle("Furnas (2.32 SD)") + theme(plot.title = element_text(size = 7, face = "bold"))
#Santiago
p5 <- plot_list.strat[[5]] +geom_hline(yintercept=1991, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1927, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1875, linetype="dashed", color = "grey20", linewidth = .5) +
  ggtitle("Santiago (1.80 SD)") + theme(plot.title = element_text(size = 7, face = "bold"))


### CONISS LOOP ####
plot_list.con <- list()

for(i in 1:length(lakes)){
  
  
  #CONISS
  coniss <-  dt.new %>% 
    filter(lake == lakes[i]) %>% 
    mutate(hellinger = sqrt(rel_abund)) %>% 
    nested_data(Age.ce, taxon_code, hellinger, fill = 0) %>%
    nested_chclust_coniss(n_groups = 3)
  
  #standalone CONISS
  
  coniss_plot <- ggplot() +
    layer_dendrogram(coniss, aes(y = Age.ce), colour = "grey10", size = .4) +
    facet_geochem_gridh(vars("Coniss")) +
    labs(x = NULL, y = NULL)+
    # scale_x_continuous(n.breaks=2) +
    scale_x_continuous(breaks = c(0, 500)) +
    scale_y_continuous(n.breaks=8, limits = c(1800,2011)) +
    theme_minimal(base_size = 7) +
    theme(axis.text.x = element_text(size = 6, angle=0)) +
    theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank())
  
  plot_list.con[[i]] <- coniss_plot 
  
  
}

p1.c <- plot_list.con[[1]] + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
p2.c <- plot_list.con[[2]] + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
p3.c <- plot_list.con[[3]] + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
p4.c <- plot_list.con[[4]] + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
p5.c <- plot_list.con[[5]] + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())

### Plots
plot <- wrap_plots(
  p1, p1.c ,
  p3, p3.c,
  p5, p5.c,
  p2, p2.c,
  p4, p4.c,
  p.leg,
  widths = c(5, 1, 5, 1, 5,1, 5, 1, 5,1, 6),
  ncol = 6)
plot


ggsave("Plots/figure_4.pdf",  plot = plot, width = 180, height = 140, units = "mm", dpi = "retina")


# Supp. Figure 1 ####

#Traits <- read_csv("Data/DT_habitat.csv")

dt.new <- readRDS("Data/Diatoms_Age_Depth.RDS")

# Check if could be any diatoms on the data without a corresponding trait
#setdiff(dt.new$taxon_code, Traits$taxon_code) 

# join data set and traits

df.wk <- dt.new %>% 
  select(taxon_code:core_id, Age.ce) %>% 
  left_join(df.hab, by = "taxon_code") %>% 
  mutate(Habitat = as.factor(Habitat)) %>% 
  mutate(Habitat = fct_recode(Habitat, "Aulacoseira spp." = "Tychoplankton.AU", 
                              "Fragilarioids" = "Tychoplankton.FR")) 

# Set the colors
Color <- c("orange" , "#ABDDA4", "#66C2A5",  "#5E4FA2"  )
names(Color) <- levels(df.wk$Habitat)

# Lakes
lakes <- levels(df.wk$lake)

## Plotting· ####


## Extract the legend from the list ####
i=1
p <- df.wk %>% 
  filter(lake == lakes[i]) %>% 
  group_by(lake, ID, Habitat, Age.ce) %>% 
  summarise(rel_abund = sum(rel_abund)) %>% 
  ungroup() %>% 
  mutate(Habitat = factor(Habitat, levels = c("Benthos", "Fragilarioids",
                                              "Aulacoseira spp.", "Plankton")))  %>% 
  ggplot(aes(rel_abund, Age.ce)) +
  geom_areah(alpha=0.8, aes(fill=Habitat)) +
  scale_fill_manual(values = Color, name = "Functional Group") +
  #geom_col_segsh(size = 0.3, color = "grey90", alpha= 0.5) +
  facet_abundanceh(vars(Habitat)) +
  #scale_y_continuous(n.breaks=8)
  scale_y_continuous(n.breaks=8, limits = c(1800,2010)) +
  labs(x = NULL, y = "Age CE") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle=45))+
  theme(strip.text.x = element_text(size = 9, angle = 45, hjust = 0, vjust=0)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle(paste("Lake", lakes[i],"Diatoms"))  +
  theme( plot.title = element_text(color="black", size = 14),# face="italic"),
         axis.title.y = element_text(size = 10)) +
  theme(legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

# Extract the legend. Returns a gtable
library(ggpubr)
leg <- get_legend(p)

# Convert to a ggplot and print
p.leg <-as_ggplot(leg)

# Loop

plot_list.strat <- list()

for(i in 1:length(lakes)){
  
  # Select species to plot by relative abundance
  selected <- df.wk %>% 
    filter(lake == lakes[i]) %>% 
    filter(rel_abund >= 5) %>% 
    distinct(taxon_code) %>% 
    droplevels() %>% 
    pull(taxon_code)
  
  df.plot <-  df.wk %>% 
    filter(taxon_code %in% selected)
  
  ### Sort by weighted mean ####
  
  b <-  df.plot %>% 
    filter(lake == lakes[i]) %>% 
    group_by(taxon_code, Age.ce) %>% 
    summarize(mean = mean(rel_abund)) %>%
    group_by(taxon_code) %>%
    summarise(wg.mean = weighted.mean(Age.ce, mean)) %>% 
    arrange(wg.mean) %>% 
    pull(taxon_code)
  b <- as.vector(b)
  
  df.plot$taxon_code <- factor(df.plot$taxon_code,      # Reordering group factor levels
                               levels = b)
  
  ### Plot ####
  p <- df.plot %>% 
    filter(lake == lakes[i]) %>% 
    ggplot(aes(rel_abund, Age.ce))+
    geom_areah(alpha=0.7, aes(fill=Habitat)) +
    geom_col_segsh(size = 0.3,  alpha= 0.9, aes(color=Habitat))+
    facet_abundanceh(vars(taxon_code))+
    scale_y_continuous(n.breaks=10)+
    scale_fill_manual(values = Color)+
    scale_color_manual(values = Color)+
    labs(x = NULL, y = "Age CE")+
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(size=8, angle=0))+
    theme(strip.text.x = element_text(size=8, angle = 45, hjust = 0, vjust=0))+
    theme(legend.position = "none")+
    ggtitle(paste("Lake", lakes[i]))  +
    theme( plot.title = element_text(color="black", size=12), # face="bold.italic"),
           axis.title.y = element_text(size=10)) 
  
  plot_list.strat[[i]] <- p 
}


# Azul
p1 <- plot_list.strat[[1]] +  geom_hline(yintercept=2000, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1901, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1962, linetype="dashed", color = "grey20", linewidth = .5)
# Empadadas
p2 <- plot_list.strat[[2]] + geom_hline(yintercept=1982, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1858, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1958, linetype="dashed", color = "grey20", linewidth = .5)
#Fogo
p3 <- plot_list.strat[[3]] + geom_hline(yintercept=1989, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1935, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1880, linetype="dashed", color = "grey20", linewidth = .5)

#Furnas
p4 <- plot_list.strat[[4]] + geom_hline(yintercept=1985.5, linetype="dashed", color = "grey20", linewidth = .5) 
#Santiago
p5 <- plot_list.strat[[5]] +geom_hline(yintercept=1991, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1927, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1875, linetype="dashed", color = "grey20", linewidth = .5)



### CONISS Plots #### 

plot_list.con <- list()

for(i in 1:length(lakes)){
  
  
  #CONISS
  coniss <-  dt.new %>% 
    filter(lake == lakes[i]) %>% 
    mutate(hellinger = sqrt(rel_abund)) %>% 
    nested_data(Age.ce, taxon_code, hellinger, fill = 0) %>%
    nested_chclust_coniss(n_groups = 3)
  
  #standalone CONISS
  
  coniss_plot <- ggplot() +
    layer_dendrogram(coniss, aes(y = Age.ce), colour = "grey10", size = .5) +
    facet_geochem_gridh(vars("Coniss")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(n.breaks=3) +
    # scale_y_continuous(n.breaks=8, limits = c(1800,2011)) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(size = 8, angle=0)) +
    theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank())
  
  plot_list.con[[i]] <- coniss_plot 
  
}

# Azul
p1.c <- plot_list.con[[1]] + geom_hline(yintercept=2000, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1901, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1962, linetype="dashed", color = "grey20", linewidth = .5) +
  theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
# Empadadas
p2.c <- plot_list.con[[2]] + geom_hline(yintercept=1982, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1858, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1958, linetype="dashed", color = "grey20", linewidth = .5) + 
  theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
#Fogo
p3.c <- plot_list.con[[3]] + geom_hline(yintercept=1989, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1935, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1880, linetype="dashed", color = "grey20", linewidth = .5)+ theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())

#Furnas
p4.c <- plot_list.con[[4]] + geom_hline(yintercept=1985.5, linetype="dashed", color = "grey20", linewidth = .5) + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())
#Santiago
p5.c <- plot_list.con[[5]] + geom_hline(yintercept=1991, linetype="dashed", color = "grey20", linewidth = .5)+
  geom_hline(yintercept=1927, linetype="dashed", color = "grey20", linewidth = .5) +
  geom_hline(yintercept=1875, linetype="dashed", color = "grey20", linewidth = .5) + theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank())



# 2 columns
plot <- wrap_plots(
  p1, p1.c ,
  p3, p3.c,
  p5, p5.c,
  p2, p2.c,
  p4, p4.c,
  p.leg,
  widths = c(5, 1, 5, 1, 5,1, 5, 1, 5,1, 6),
  ncol = 2)
plot

ggsave("Plots/Sup_figure_1.png",  width = 18, height = 35, units = "cm", dpi = 300)

## Two pages plots

plot <- wrap_plots(
  p1, p1.c ,
  p3, p3.c,
  p5, p5.c,
  p.leg,
  widths = c(5, 1, 5, 1, 5,1, 6),
  heights = c(8,8,8,2),
  ncol = 2)
plot

ggsave("Plots/Sup_figure_1a.png",  width = 20, height = 30, units = "cm", dpi = 300)

plot <- wrap_plots(
  p2, p2.c,
  p4, p4.c,
  p.leg,
  widths = c(5, 1, 5, 1, 6),
  heights = c(8,8,2),
  ncol = 2)
plot

ggsave("Plots/Sup_figure_1b.png",  width = 20, height = 19, units = "cm", dpi = 300)
