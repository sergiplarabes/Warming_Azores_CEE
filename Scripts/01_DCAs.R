# Source data: Diatoms sediment records data set from São Miguel Island
# Data/Diatoms_SaoMiguel_Data.RDS

# 1. Calculate DCA from diatoms data in all lake core sediment records #
# 2. Integrate Meteo Data at similar time resolution of Sediment Core record
# 3. Join and save the Meteo data set with DCA results #

# Loading Libraries
library(foreach)
library(tidyverse)
library(vegan)

# Clean working space
rm(list = ls())

# Upload the Diatoms sediment records data set from São Miguel Island ####
df.wk <- readRDS("Data/Diatoms_SaoMiguel_Data.RDS") %>% 
  rename(year_CE = Age.ce)

# 1. Loop to calculate a DCA for each lake ####
lakes <- levels(df.wk$lake)

results_list <- list()

for(i in 1:length(lakes)){
  
  df.sp <- df.wk %>% 
    filter(year_CE >= 1800) %>% 
    filter(lake == lakes[i]) %>% 
    select(ID, counts, taxon_code) %>% 
    # Change the data set from long to wide format
    pivot_wider(values_from = counts, names_from = taxon_code,
                values_fill = 0) %>% 
    column_to_rownames("ID")
  
  # Hellinger transformation before DCA analysis
  df.spe <- decostand(df.sp[,-1], method = "hellinger")
  
  # DCA
  res.DCA <- df.spe %>% decorana()  %>% 
    scores(choices=c(1)) %>% as.data.frame() %>% 
    rownames_to_column("ID") 
  
  # Sediment record data
  site <-  df.wk %>% 
    filter(year_CE >= 1800) %>% 
    filter(lake == lakes[i]) %>% 
    select(taxon_code:core_id, year_CE, Top.ce, Bot.ce, period) %>% 
    distinct(ID, .keep_all = T) %>% 
    select(ID, lake:period)
  
  # Join results
  df.DCA <- site %>% 
    right_join(res.DCA, by = "ID") %>% 
    mutate(DCA1 = case_when(lake == 'Fogo' ~ DCA1*-1, .default = DCA1))

  results_list[[i]] <- df.DCA 
}

# Create a data frame from the results_list from all 5 lakes
DCA.res <- Reduce(full_join,results_list) %>% 
  arrange(lake, desc(year_CE))

# 2. Integrate Meteo Data at similar time resolution of Sediment Core record Data ####

library 
library(tidyverse)

# Functions to resample climate data to the same time resolution the diatom data set
source("Functions/CEE_functions.R")

## Meteorological Data from Ponta Delgada ####
df.temp <- read_csv("Data/PontaDelgadaTemp.csv")

### Calculate PD Temperature anomaly for the period 1901-2000 ####

df.temp.base <- df.temp %>% filter(YEAR >= 1901 & YEAR <= 2000) 
T.mean <- mean(df.temp.base$Tmean)
df.temp <-  df.temp %>% mutate(Anomaly = Tmean-T.mean)

# DCA.res is calculated in 02_DCAs.R
df.env <- DCA.res %>% 
  filter(year_CE >= 1873) # Temperature time series starts at 1873

# Calculations 

result.T_PD <- resample_meteo_at_core(df.env, df.meteo = df.temp, 
                                      top_col = "Top.ce", bot_col = "Bot.ce", id_col = "ID")

colnames(result.T_PD) <- c("ID", "Tanomaly", "Tanomaly.sd")

### Ponta Delgada PRECIPITATION ####
df.pre <- read_csv("Data/PontaDelgada_precipitation.csv")

#Calculate Precipitation anomaly for the period 1900-1999

df.pre.base <- df.pre %>% filter(YEAR >= 1901 & YEAR <= 2000) 
P.mean <- mean(df.pre.base$P_tot)
df.pre <-  df.pre %>% mutate(Anomaly = P_tot-P.mean)

# Calculations

result.P_PD <- resample_meteo_at_core(df.env, df.meteo = df.pre, 
                                      top_col = "Top.ce", bot_col = "Bot.ce", id_col = "ID")

colnames(result.P_PD) <- c("ID", "Panomaly", "Panomaly.sd")

### NHSAT ####

df.NH <- read_csv("Data/NHAST.csv") %>% 
  rename(YEAR = Year)

df.env <- DCA.res  %>% 
  filter(year_CE >= 1850) # Temperature time series starts at 1850

# Calculations 

result.NH <- resample_meteo_at_core(df.env, df.meteo = df.NH, 
                                    top_col = "Top.ce", bot_col = "Bot.ce", id_col = "ID")

colnames(result.NH) <- c("ID", "NHanomaly", "NHanomaly.sd")

# 3. Join and save the Meteo data set with DCA results ####
df.DCA_meteo <- DCA.res %>% 
  left_join(result.T_PD, by= "ID") %>% 
  left_join(result.P_PD, by= "ID") %>% 
  left_join(result.NH, by= "ID")


saveRDS(df.DCA_meteo, file = "Data/DCA_meteo.RDS" )

