# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load necessary libraries
library(mice)

# load complete merged data to obtain survey number levels
# nothing to do with this data, we only need the factor levels for the loop
cleaned_full_df <- readRDS("cleaned_full_df.rds")
cleaned_full_df_used <- cleaned_full_df[cleaned_full_df$survey_nr %in% c("2005_2006","2007_2008","2009_2010",
                                                                         "2011_2012","2013_2014","2015_2016",
                                                                         "2017_2018"),]

# in the missing_true vector, TRUE means missing values across ALL depression indicators
missing_true <- rowSums(is.na(cleaned_full_df[,(ncol(cleaned_full_df)-8):ncol(cleaned_full_df)])) == 9
# in the nonmissing_true vector, TRUE means not completely missing values across depression indicators (so there can be missing values, but not for all indicators)
nonmissing_true <- !missing_true

# observations that have some depression data
dep_nonmissing <- cleaned_full_df[nonmissing_true,]

# calculate average missingness in data
(sum(is.na(dep_nonmissing))/prod(dim(dep_nonmissing)))*100
# it is 0.6%, we will only impute one copy

# load data per survey (loop)
for (i in levels(cleaned_full_df$survey_nr)) {
  
  full_df <- readRDS(paste0("full_",i,".rds"))
  
  # MICE IMPUTATION
  # save variables as vectors that are not needed for imputation
  SEQN <- full_df$SEQN
  full_df$SEQN <- NULL
  SDMVPSU <- full_df$SDMVPSU
  full_df$SDMVPSU <- NULL
  SDMVSTRA <- full_df$SDMVSTRA
  full_df$SDMVSTRA <- NULL
  
  # imputation: 1 copy, 5 iterations, predictive mean matching algorithm
  imputation_object <- mice(full_df, method = "rf", m = 1, maxit = 5, seed = 35670)
  
  # investigate convergence visually
  # imputation_object$method #those variables with no missing have "" as method - they are still used for imputation
  # plot(imputation_object) #plots look OK
  
  
  for (j in 1) {
    
    # extract imputed datasets
    imputed <- complete(imputation_object, j)
    
    # removal of not needed variables
    imputed$ever_lipid_meds <- NULL
    
    # re-merge ID 
    imputed_df <- as.data.frame(cbind(SEQN, SDMVPSU, SDMVSTRA, imputed))
    
    filename <- paste0("imputed_",i,"_",j,".rds")
    
    # save imputed data
    saveRDS(imputed_df, filename)
    
  }
}



