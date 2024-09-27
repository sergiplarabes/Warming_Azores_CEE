# 1. Function to calculate meteo data ta the core sample resolution ####

# Define the function with flexible column names
resample_meteo_at_core <- function(df.env, df.meteo, top_col, bot_col, id_col) {
  
  # Check if the specified columns exist in df.env
  if (!all(c(top_col, bot_col, id_col) %in% colnames(df.env))) {
    stop("The columns for Top.ce, Bot.ce, and ID must exist in the dataframe df.env.")
  }
  
  # Select and rename the necessary columns to standard names for internal use
  k <- df.env %>%
    select(Top.ce = !!sym(top_col), Bot.ce = !!sym(bot_col), ID = !!sym(id_col))
  
  # Extract the year and anomaly vectors from df.temp
  f <- df.meteo$YEAR
  var <- df.meteo$Anomaly
  
  # Perform the calculation
  res.meteo <- data.frame(foreach(i = 1:nrow(k), .combine = "rbind") %do% {
    # Search for the vector position with the minimum difference related to core age (TOP)
    b <- which.min(abs(f - as.numeric(k[i, "Top.ce"])))
    
    # Search for the position of the minimum difference related to core bottom age
    t <- which.min(abs(f - as.numeric(k[i, "Bot.ce"])))
    
    # Calculate mean and SD of var (Anomaly) between t and b
    value <- mean(var[t:b])    # average value of the Anomaly for the new sample interval
    sdvalue <- sd(var[t:b])    # standard deviation
    
    # Return the results, including the ID column
    cbind(k[i, "ID"], value, sdvalue)
  })
  
  return(res.meteo)
}


# 2. Calculate Regional diversity across time periods ####
Diversity_period <- function(df, R, cov) {
  # Ensure the period variable is a factor
  df$period <- as.factor(df$period)
  
  # Levels of period (time interval available)
  lev <- levels(df$period)
  
  # Find the minimum number of samples that could be selected per period and lake
  samples <- min(with(df, table(period, lake)))
  
  # Create an empty list to store results (more efficient than a matrix)
  results_list <- list()
  
  # Loop through each period
  for (j in 1:length(lev)) {
    # Filter the data frame for the current period
    mat <- df %>% filter(period == lev[j]) %>%  
      # Group by lake to select samples
      group_by(lake)
    
    # Nested loop to calculate gamma, alpha, and beta diversity for each time interval
    results_A <- foreach(i = 1:R, .combine = "rbind") %do% {
      # Randomly sample 'samples' rows for each lake
      mat1 <- sample_n(mat, samples)
      
      # Split data into environmental (lake and period) and species (remaining columns)
      env <- select(mat1, lake:period)
      spe <- mat1 %>% ungroup() %>% select(-c(lake:period))
      
      # Remove species with zero counts
      spe <- spe[, colSums(spe) != 0]
      
      # Calculate expected sample size corresponding to the sample coverage
      # invChat Find the sample size N from a sample coverage (cov) see /Functions/beta_C.R
      N <- invChat(colSums(spe), cov)
      
      # Calculate gamma diversity see D0.hat function in /Functions/beta_C.R
      gamma <- D0.hat(colSums(spe), N)
      
      # Calculate alpha diversity (mean species richness)
      alpha <- mean(apply(spe, 1, D0.hat, m = N))
      
      # Calculate beta diversity (multiplicative beta-diversity)
      beta <- gamma / alpha
      
      # Store the current period and the calculated metrics
      period <- lev[j]
      
      # Return results
      cbind(period, gamma, alpha, beta)
    }
    
    # Append the results for this iteration to the list
    results_list[[j]] <- results_A
  }
  
  # Combine all results into a single data frame
  results_f <- do.call(rbind, results_list)
  
  # Convert results matrix to data frame
  results_df <- as.data.frame(results_f, stringsAsFactors = FALSE)
  
  # Assign column names
  colnames(results_df) <- c("period", "gamma", "alpha", "beta")
  
  # Ensure 'Period' is a factor
  results_df$period <- factor(results_df$period, levels = lev)
  
  # Convert Gamma, Alpha, and Beta to numeric
  results_df$gamma <- as.numeric(results_df$gamma)
  results_df$alpha <- as.numeric(results_df$alpha)
  results_df$beta <- as.numeric(results_df$beta)
  
  return(results_df)
}

# 3.Function to calculate Evenness from differents periods of time #### 

Evenness_period <- function(df, R, cov) {
  # Ensure the period variable is a factor
  df$period <- as.factor(df$period)
  
  # Levels of period (time interval available)
  lev <- levels(df$period)
  
  # Find the minimum number of samples that could be selected per period and lake
  samples <- min(with(df, table(period, lake)))
  
  # Create an empty list to store results
  results_list <- list()
  
  # Loop through each period
  for (j in 1:length(lev)) {
    # Filter the data frame for the current period
    mat <- df %>% filter(period == lev[j]) %>%  
      # Group by lake to select samples
      group_by(lake)
    
    # Nested loop to calculate alpha, beta, and other metrics for each time interval
    results_A <- foreach(i = 1:R, .combine = "rbind") %do% {
      # Randomly sample 'samples' rows for each lake
      mat1 <- sample_n(mat, samples)
      
      # Split data into environmental (lake and period) and species (remaining columns)
      env <- select(mat1, lake:period)
      spe <- mat1 %>% ungroup() %>% select(-c(lake:period))
      
      # Remove species with zero counts
      spe <- spe[, colSums(spe) != 0]
      
      # Species abundance vector
      a <- colSums(spe)
      b <- data.frame(a)
      
      # Estimate diversity using coverage-based estimation (iNEXT)
      div.f <- estimateD(a, datatype = "abundance", base = "coverage", 
                         level = cov, conf = 0.95)
      
      # Extract diversity metrics
      S <- div.f[1, "qD"]  # richness
      H <- div.f[2, "qD"]  # exponential of Shannon
      
      # Pielou's Evenness
      J <- log(H) / log(S)  # log(Exp(H))/log(S)
      
      # Dominance (percentage of the 2 most abundant species on each sample)
      c <- b %>% mutate(rel_abund = (a / sum(a)) * 100) %>% arrange(desc(a))
      BP2 <- c[1,2] + c[2,2]
      
      # Store the current period and the calculated metrics
      period <- lev[j]
      
      # Return results
      cbind(period, J, BP2)
    }
    
    # Append results for the current period to the list
    results_list[[j]] <- results_A
  }
  
  # Combine all results into a single data frame
  results_f <- do.call(rbind, results_list)
  
  # Convert results matrix to a data frame
  results_df <- as.data.frame(results_f, stringsAsFactors = FALSE)
  
  # Assign column names
  colnames(results_df) <- c("period", "evenness", "dominance")
  
  # Ensure 'Period' is a factor
  results_df$period <- factor(results_df$period, levels = lev)
  
  # Convert numeric columns to numeric type
  results_df$evenness <- as.numeric(results_df$evenness)
  results_df$dominance <- as.numeric(results_df$dominance)
  
  return(results_df)
}

