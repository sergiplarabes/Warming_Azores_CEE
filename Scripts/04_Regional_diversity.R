# Data calculation for Figure 6 
rm(list = ls())
# Calculate regional diversity (gamma), mean regional diversity (alpha)
# and Beta-diversity (variability in the spatial distribution of diatom species)
# Following Engel, et al. Ecosphere 2021 Vol. 12 Issue 9 Pages e03745

## call source code. Functions from Engel et al. 2021
source("Functions/Beta_C.R")
source("Functions/CEE_functions.R")

#Upload R Packages
library(tidyverse)
library(foreach)

#Upload data and transform to wide format
DF.dt.counts <-readRDS("Data/Diatoms_SaoMiguel_Data.RDS") %>% 
  select(-c(rel_abund)) %>% 
  pivot_wider(values_from = counts, names_from = taxon_code,
              values_fill = 0)

# Find samples with low counts
# sort(DF.dt.counts %>% column_to_rownames("ID") %>%  select(-c(lake:period)) %>% rowSums())
# Remove sample AZ06_28.25 with low counts, which would increase the accuracy of the diversity estimation
# Data selection
df.dtS <- DF.dt.counts %>% 
  #rename(ID = ID.1) %>% 
  filter(ID != "AZ06_28.25") %>% # Remove samples with low diatom counts
  column_to_rownames("ID") %>% 
  filter(Age.ce >= 1830) %>% 
  filter(Age.ce <= 2011) %>%
  #create a 10 yeras time-interval
  mutate(period2 = cut_interval(Bot.ce,  17, dig.lab = 4)) %>% 
  relocate(period2, .after = Age.int )

## Coverage Based standardization ###

# Calculate the minimum Coverage all samples in São Miguel data set (1895-2010)
# Calculate the target Coverage. The largest possible coverage that can be used to calculate 
# beta_C (from Engel et al., 2021) for the data set without exceeding twice the size of each sample. 
cov <-  df.dtS %>% select(-c(lake:period)) %>% 
  C_target() %>% 
  round(5)


## Diversity for São Miguel data set, but excluding the shortest record from Lagoa Furnas ####
#  start the time series at 1830
# 30-years time intervals (bins)
# Select years and lakes
df.dt1 <- df.dtS %>% 
  filter(lake != "Furnas") %>% # Furnas is the shortest record
  filter(Age.ce >= 1830) %>% 
  filter(Age.ce <= 2011) %>% 
  droplevels()

# Table of available samples per period and lake
# with(df.dt1,table(period,lake))

# Preparing parameters for the Loop
# Time interval available in the data set
lev<-levels(df.dt1$period)
# Set the Number of Polling times
R = 999
# Find the minimum number of samples that could be selected per period and lake 
samples <- min(with(df.dt1,table(period,lake)))
# Set seed to replicate results
set.seed(123)

# Calculation for each period R times
result <- Diversity_period(df= df.dt1, R = R, cov = cov)

# Create a data frame with the results
beta_6p <-result %>% 
  pivot_longer(cols = !period, names_to = "Diversity") %>% 
  mutate(set = "4lakes_6periods") #identified

## São Miguel Including Furnas between 1969 and 2011 ####
# and 10-years time intervals

df.dt1 <- df.dtS %>% 
  filter(Age.ce > 1969) %>% 
  filter(Age.ce <= 2011) %>% 
  droplevels() %>% 
  select(-period) %>% 
  rename(period = period2)

# Table of available samples per period and lake
# with(df.dt1,table(period2,lake))

# Preparing parameters for the Loop
# Time interval available in the data set
lev<-levels(df.dt1$period2)
# Set the Number of Polling times
R = 999
# Find the minimum number of samples that could be selected per periode and lake 
samples <- min(with(df.dt1,table(period,lake)))

# Set seed to replicate results
set.seed(123)

# Calculation for each period R times
result <- Diversity_period(df= df.dt1, R = R, cov = cov)

# Create a data frame with the results
beta_4p <-result %>% 
  pivot_longer(cols = !period, names_to = "Diversity") %>% 
  mutate(set = "5lakes_4periods") #identified

## Join results for the two periods (10 and 10 year ones)
Beta_C_results <- rbind(beta_6p, beta_4p)

saveRDS(Beta_C_results, "Data/Beta_coverage_1000_paper.RDS")


# Regional Evenness and Dominance ####
rm(list = ls())
## Evenness and Dominance at regional scale (Island-wide) ####

#Loading libraries
library(iNEXT)
library(tidyverse)
library(foreach)

## call source code. 
source("Functions/Beta_C.R") # Functions from Engel et al., 2021
source("Functions/CEE_functions.R")
#Upload data and transform to wide format
DF.dt.counts <-readRDS("Data/Diatoms_SaoMiguel_Data.RDS") %>% 
  select(-c(rel_abund)) %>% 
  pivot_wider(values_from = counts, names_from = taxon_code,
              values_fill = 0)

# Data selection
df.dtS <- DF.dt.counts %>% 
  filter(ID != "AZ06_28.25") %>% # remove samples with low counts
  column_to_rownames("ID") %>% 
  filter(Age.ce >= 1830) %>% 
  filter(Age.ce <= 2011) %>%
  #create a 10 years time-interval
  mutate(period2 = cut_interval(Bot.ce,  17, dig.lab = 4))  %>% 
  relocate(period2, .after = Age.int )

# beta_C (from Engel et al., 2021) for the data set without exceeding twice the size of each sample. 
cov <-  df.dtS %>% select(-c(lake:period)) %>% 
  C_target() %>% 
  round(5)

### Dominance Metrics from 1830 in period of 30 years ####

df.dt1 <- df.dtS %>% 
  filter(lake != "Furnas") %>% 
  droplevels()

#with(df.dt1,table(period,lake))
# Set the Number of Polling times
R = 99

set.seed(123)

# Apply the function to the data set
result <- Evenness_period(df = df.dt1, R = R, cov = cov)

df.res_6 <- result %>% 
  pivot_longer(cols = !period, names_to = "Diversity") %>% 
  mutate(set = "4lakes_6periods")


### Beta diversity from 1969 in period of 10 years ####

df.dt1 <- df.dtS %>% 
  filter(Age.ce > 1969) %>% 
  filter(Age.ce <= 2011) %>% 
  droplevels() %>% 
  select(-period) %>% 
  rename(period = period2)

# Table with available samples
# with(df.dt1,table(period,lake))

## Subloop to calculate gamma for each period ####
lev <- levels(df.dt1$period)

# Set the Number of Polling times
R = 99
set.seed(123)

# apply the function for the second period
result <- Evenness_period(df = df.dt1, R = R, cov = cov)

df.res_4 <- result %>% 
  pivot_longer(cols = !period, names_to = "Diversity") %>% 
  mutate(set = "5lakes_4periods")

# join both time period results
result.join <- rbind(df.res_6, df.res_4)

saveRDS(result.join, file = "Data/Dominance_J_6_4period_99runs.Rds")

