
# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load necessary libraries
library(foreign)
library(dplyr)

# LOAD AND MERGE ALL NECESSARY FILES PER SURVEY FOR PRESCRIPTION DRUGS


# dowload drug classes
drugclasses_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/RXQ_DRUG.XPT")
## MEDICATIONS
download.file(drugclasses_file, tf <- tempfile(), mode="wb")
drugclasses <- foreign::read.xport(tf)
keep_vars <- c("RXDDRGID", "RXDDCN1A")
drugclasses <- drugclasses[,keep_vars]

######################################
########## NHANES 2007-2008 ##########
######################################

years <- "2007-2008"
letter <- "E"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2007_2008 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2007_2008 <- meds_2007_2008[,keep_vars]

######################################
########## NHANES 2009-2010 ##########
######################################
years <- "2009-2010"
letter <- "F"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2009_2010 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2009_2010 <- meds_2009_2010[,keep_vars]

######################################
########## NHANES 2011-2012 ##########
######################################
years <- "2011-2012"
letter <- "G"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2011_2012 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2011_2012 <- meds_2011_2012[,keep_vars]

######################################
########## NHANES 2013-2014 ##########
######################################
years <- "2013-2014"
letter <- "H"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2013_2014 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2013_2014 <- meds_2013_2014[,keep_vars]

######################################
########## NHANES 2015-2016 ##########
######################################
years <- "2015-2016"
letter <- "I"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2015_2016 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2015_2016 <- meds_2015_2016[,keep_vars]

######################################
########## NHANES 2017-2018 ##########
######################################
years <- "2017-2018"
letter <- "J"

med_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/RXQ_RX_",letter,".XPT")

## MEDICATIONS
download.file(med_file, tf <- tempfile(), mode="wb")
meds_2017_2018 <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXDUSE", "RXDDRUG", "RXDDRGID")
meds_2017_2018 <- meds_2017_2018[,keep_vars]

# merging files
meds_df <- rbind(meds_2007_2008,
                 meds_2009_2010,
                 meds_2011_2012,
                 meds_2013_2014,
                 meds_2015_2016,
                 meds_2017_2018)

# adding drug class
merged <- left_join(meds_df, drugclasses, by = "RXDDRGID")

saveRDS(meds_df, "meds_df.rds")


