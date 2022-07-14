# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load necessary libraries
library(mice)
library(foreign)

# load complete merged data to obtain survey number levels
# nothing to do with this data, we only need the factor levels for the loop
cleaned_full_df <- readRDS("cleaned_full_df.rds")

# calculate average missingness in data
(sum(is.na(cleaned_full_df[cleaned_full_df$phq == "Yes",]))/prod(dim(cleaned_full_df[cleaned_full_df$phq == "Yes",])))*100
# it is 13.8%, we will impute 15 copies


  full_df <- cleaned_full_df
  
  # MICE IMPUTATION
  # save variables as vectors that are not needed for imputation
  SEQN <- full_df$SEQN
  full_df$SEQN <- NULL
  SDMVPSU <- full_df$SDMVPSU
  full_df$SDMVPSU <- NULL
  SDMVSTRA <- full_df$SDMVSTRA
  full_df$SDMVSTRA <- NULL
  
  # imputation: 1 copy, 5 iterations, predictive mean matching algorithm
  imputation_object <- mice(full_df, method = "rf", m = 15, maxit = 5, seed = 35670)
  
  # investigate convergence visually
  # imputation_object$method #those variables with no missing have "" as method - they are still used for imputation
  # plot(imputation_object) #plots look OK
  
  
  for (j in 1:15) {
    
    # extract imputed datasets
    imputed <- complete(imputation_object, j)
    
    # re-merge ID 
    imputed_df <- as.data.frame(cbind(SEQN, SDMVPSU, SDMVSTRA, imputed))
    
    filename <- paste0("imputed_",j,".rds")
    
    # save imputed data
    saveRDS(imputed_df, filename)
    
  }





