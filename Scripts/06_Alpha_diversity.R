# Diversity metrics using Inext ####
rm(list = ls())
# Loading Libraries
library(iNEXT)
library(tidyverse)

## call source code. Functions from Engel et al
source("Functions/Beta_C.R")

#Loading data
DF.dt.counts <-readRDS("Data/Diatoms_SaoMiguel_Data.RDS") %>% 
  #wide format
  select(-c(rel_abund)) %>% 
  pivot_wider(values_from = counts, names_from = taxon_code,
              values_fill = 0)

# Find the minimum sample size of the data set
sort(DF.dt.counts %>% column_to_rownames("ID") %>% 
       select(-c(lake:period)) %>% rowSums())
# Remove Azul_1964 with low counts. Thi sample would have constraint
#the accuracy of Richness metrics

df.dtS <- DF.dt.counts %>% 
  filter(ID != "AZ06_28.25") %>% 
  column_to_rownames("ID") 
  

#Create Matrix for species and environment (record info)
df.spe <- df.dtS %>%  
  select(-c(lake:period)) 

df.env <- df.dtS %>%  
  select(lake:period) %>% 
  rownames_to_column("Assemblage")


# Find the minimum coverage at the double sample size to standardize

cov <- C_target(df.spe) # AT the double of sample size
size <- round(invChat(colSums(df.spe), cov), 0)
# Minimum coverage at the double sample Size
df.spe.t <- t(df.spe)

div.cov2<-estimateD(df.spe.t, datatype="abundance", base="coverage", 
                    level=cov,  conf=0.95) 


df.div <- div.cov2 %>% 
  left_join(df.env, by = "Assemblage" )  %>% 
  rename("ID" = "Assemblage") %>% 
  mutate(Order.q=as.character(Order.q)) %>% 
  mutate(Order.q = fct_recode(Order.q, Richness= "0", Exp.Shannon = "1", Inv.Simpson ="2")) %>% 
  mutate(Order.q = fct_relevel(Order.q, "Richness","Exp.Shannon", "Inv.Simpson")) %>% 
  mutate(base = rep("coverage", nrow(div.cov2)))


saveRDS(df.div, file = "Data/Inext_Diversity_all_sd.RDS")

# Supplementary Figure 4 Diversity Alpha diversity through time per data point ###
rm(list = ls())

library(tidyverse)
library("scico")
library(scales)
# Color Palettes

lakes_col <- scico(5, begin = 0, end = 0.95, palette = 'roma', categorical = F)

# Data
df.div <- readRDS("Data/Inext_Diversity_all_sd.RDS")

# Plot
p.div.cov <-   df.div  %>% filter(base == "coverage") %>% 
  mutate(group = paste(lake, Order.q, sep="-"))  %>% 
  ggplot2::ggplot(aes(Age.ce, qD)) + 
  geom_point(aes(color= lake),  show.legend = FALSE)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", fx = TRUE, k = 10),
              aes(color= lake), show.legend = FALSE)  +
  facet_wrap( ~ group, scales = "free_y", ncol = 3)+
  scale_color_manual(values=lakes_col)+
  theme_minimal(base_size = 12) 

p.div.cov

ggsave("Plots/Sup_figure_4.png",  width = 30, height = 25, units = "cm", dpi = 300, bg = "white")

