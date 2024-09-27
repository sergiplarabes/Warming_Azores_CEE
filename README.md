# Warming_Azores_CEE
R code and data used in the Paper published in CEE

## Freshwater Communities Response to Warming

Repository for **Global warming triggers abrupt regime shifts in 
island lake ecosystems in the Azores Archipelago** project

### São Miguel data set (5 lakes)

Diatoms_SaoMiguel_Data.RDS 

### Scripts used for the paper, including figures
Scripts are numerated to follow the used workflow

**01_DCAs.R** 1) Calculate Detrended Correspond Analysis first axis and 2) Climate data 
(NHSAT and Air Temperature and Precipitation from Ponta Delgada weather station) at the same time intervals
than sediment core records

**02_HGAM_DCA_trends.R** Hierarchical Generalised Additive model fitted trends of DCA axis 1 values for the 5 studied lakes sedimentary records in São Miguel Island and **Figure 3** in the paper.

**03_HGAM_DCA_NHSAT.R** An island-scale Hierarchical Generalised Additive based model of NHSAT anomaly fitted to DCA for the 5 studied lakes sedimentary records in São Miguel Island, and plotting **Figure 5** in the paper. **Supplementary Figure 2** showing lake scale results. Code for HGAM model of São Miguel air temperature anomaly instrumental record fitted to DCA for the 5 studied lakes **Supplementary Figure 3**

**04_Diversity_time_intervals.R** 1) Assessing regional diversity (gamma), mean regional diversity (alpha) and Beta-diversity (variability in the spatial distribution of diatom species across São Miguel island studied lakes for each time interval studied in the present work 2) Assessing Evenness and Dominance at regional scale (Island-wide) for each time interval. To calculate beta, alpha and gamma diversity the sample coverage functions in Engel et al., 2021 have been applied. These functions are in the Functions folder (see beta_C.R). Engel, T., et al., 2021. Using coverage-based rarefaction to infer non-random species distributions. Ecosphere 12:e03745.	https://doi.org/10.1002/ecs2.3745

**05_Figure_6.R** Code for **Figure 6** Showing regional diversity changes across the full studied period (1830-2010) of 4 studied lakes for 30 years’ time intervals and changes in regional diversity metrics across the most recent times (1968-2010) of 5 studied lakes 10 years’ time intervals.

**06_Alpha_diversity.R** Assessing changes in alpha diversity across time for each sediment record and **Supplementary Figure 4** Changes across time in lake (local) biodiversity metrics

**07_Stratigraphic_Figures.R** Code for **Figure 4** Shifts in the dominance (relative abundance) of key diatom functional groups over time.**Sup. Figure 1** Stratigraphic plots showing changes in the most abundant species (relative abundances higher than 5%)
for each lake record. 

**08_Figure_7.R** Code for **Figure 7** Diatom species losses/gains between consecutive time intervals

**09_HGAM_DCA_PD-Temp.R** An island-scale Hierarchical generalised additive based model of Ponta Delgada Temperaure anomaly fitted to DCA for the 5 studied lakes sedimentary records in São Miguel Island, and plotting **Supplementary Figure 3** showing lake scale results.

**10_Supp_Figure_5.R** HGAM model with Ponta Delgada air temperature and Precipitation instrumental records **Supplementary Figure 7** 

## Folders

### Data
This folder contains the diatoms sediment record data used in the paper from 5 lakes, North Hemisphere Surface air temperature (NHSAT), Air temperature and Precipitation instrumental record from Ponta Delgada (São Miguel Island, Açores), and results of all the analysis.

### Functions
This folder contains the Beta_C functions from Engel et al., 2021 and additional functions developed to tidy the paper code.

### Plots

This folder store all the plots of the present project

### Scripts

This Folder contain all the scripts explained above







