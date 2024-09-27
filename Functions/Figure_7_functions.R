
# Functions for Figure 7 Species losses and gains using Codyn library

# The following function is resampling the dataset at a regional scale to have 
#a measure of variability.  Is resampling n times selecting two samples per each 
#lake and period (time-interval). Samples are standardised using rarefaction by 
#sample size. The output data frame "rep" = replicate #number, "value" is the 
#percentage of any species (name) in a given replicate for the selected period 
# and “Age.ce.m” is the mean Age CE. for the  selected period.
codyn_input_data <- function(df, R = 100) {
  
  # Preparing parameters for the Loop
  df$period <- as.factor(df$period)  # Ensure period is a factor
  lev <- levels(df$period)           # Polling times
  
  # Find the minimum number of samples selected for each lake 
  samples <- min(with(df, table(period, lake)))
  
  # Minimum sample size to standardize
  Sb <- min(rowSums(df[, 13:ncol(df)]))  # Standardize based on species columns (assumed from column 13 onward)
  
  # Loop to create the regional matrix
  matrix <- foreach(i = 1:R, .combine = "rbind") %do% { 
    results_A <- foreach(j = 1:length(lev), .combine = "rbind") %do% {  
      mat <- df %>% 
        filter(period == lev[j]) %>%  # Select the period (time interval)
        group_by(lake)                # Group by lake
      mat1 <- sample_n(mat, samples)   # Select 'samples' number of samples per lake
      
      # Create the environmental and species matrices
      env <- select(mat1, lake:period)   # Environmental data
      spe <- mat1 %>% ungroup() %>%      # Species data
        select(-c(lake:period))
      
      # Randomly select 'Sb' individuals from each sample
      mat.r <- rrarefy(spe, Sb)          
      spe.1 <- colSums(mat.r)            # Sum of species after rarefaction
      
      period <- lev[j]
      rep <- i
      a <- c(rep, period, spe.1)         # Combine rep, period, and species data
      
      return(a)
    }
  }
  
  # Convert to a data frame
  DF <- data.frame(matrix) %>%
    # Separate the top and bottom age for each period
    separate_wider_delim(V2, "-", names = c("AgeT.p", "AgeB.p"), cols_remove = F) %>% 
    mutate(V2 = as.factor(V2)) %>% 
    mutate_if(is.character, as.numeric) %>% 
    # Calculate the central age for each period
    mutate(Age.ce.m = (AgeT.p + AgeB.p) / 2) %>% 
    relocate(Age.ce.m, .before = AgeT.p) %>% 
    rename(rep = V1, period = V2)
  
  #### Create environmental matrix ####
  DF.env <- DF[, 1:5]  # First 5 columns contain environmental data
  
  #### Create species matrix ####
  # Transform species data to percentages
  spe.1 <- decostand(DF[,-(1:5)], method = "total") * 100
  
  # Join with environmental data
  spe.2 <- bind_cols(DF.env, spe.1)
  
  # Transform to long format
  df.mat <- spe.2 %>% 
    pivot_longer(cols = !c(rep:period))  # Transform species data into long format
  
  return(df.mat)
}

# This function claculate the turnover metrrics using Codyn library and format 
# the results to plot

calculate_turnover_metrics <- function(df.in) {
  
  # Calculate turnover, disappearance, and appearance
  df.turn <- turnover(df = df.in,  
                      time.var = "Age.ce.m", 
                      species.var = "name",
                      abundance.var = "value", 
                      replicate.var = "rep")
  
  df.dis <- turnover(df = df.in,  
                     time.var = "Age.ce.m", 
                     species.var = "name",
                     abundance.var = "value", 
                     replicate.var = "rep",
                     metric = "disappearance")
  
  df.app <- turnover(df = df.in,  
                     time.var = "Age.ce.m", 
                     species.var = "name",
                     abundance.var = "value", 
                     replicate.var = "rep",
                     metric = "appearance")
  
  # Summarize appearance data
  df.a <- df.app %>%
    group_by(Age.ce.m) %>%
    summarise(sd = sd(appearance, na.rm = TRUE),
              mean = mean(appearance, na.rm = TRUE)) %>% 
    mutate(metric = "Appearance")
  
  # Summarize disappearance data
  df.d <- df.dis %>%
    group_by(Age.ce.m) %>%
    summarise(sd = sd(disappearance, na.rm = TRUE),
              mean = mean(disappearance, na.rm = TRUE)) %>% 
    mutate(metric = "Disappearance")
  
  # Summarize turnover data
  df.t <- df.turn %>%
    group_by(Age.ce.m) %>%
    summarise(sd = sd(total, na.rm = TRUE),
              mean = mean(total, na.rm = TRUE)) %>% 
    mutate(metric = "Turnover")
  
  # Combine all results into one data frame
  df.res <- rbind(df.a, df.d, df.t)
  
  return(df.res)
}

