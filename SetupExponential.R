## Setup work on Warner Point exponential models: ##
##  set wd and load libraries                   ## 


# Define directory paths
# R code
  DirCode <- "Z:\\asb\\BlackCanyon2015\\WarnerPoint\\Exponential_Model\\Code\\"
# Data 
  DirData <- "Z:\\asb\\BlackCanyon2015\\WarnerPoint\\Exponential_Model\\Data\\"
# Output
  DirOut <- "Z:\\asb\\BlackCanyon2015\\WarnerPoint\\Exponential_Model\\Output\\"
  
#   # R code
#   DirCode <- "C:\\Users\\bubba\\Desktop\\USGS 0926\\WarnerPt\\Exponential_model\\Code\\"
#   # Data (USGS gage data)
#   DirData <- "C:\\Users\\bubba\\Desktop\\USGS 0926\\WarnerPt\\Exponential_model\\Data\\"
#   # Output
#   DirOut  <- "C:\\Users\\bubba\\Desktop\\USGS 0926\\WarnerPt\\Exponential_model\\Output\\"

# set working directory to data location
  setwd(DirData)

# load libraries
  library(plyr)
  library(dplyr)
#   library(gridExtra)
  library(reshape2)
  library(ggplot2)


#### Read in and prepare data ####
## 1. Veg survey data
  rawveg <- read.csv("WP_VegData_withCovertypeInundur.csv")

  # Limit to veg columns only
#   veg <- filter(rawveg, year==1990)  #to do 1990 only
  veg <- rawveg[, c(2:3, 23:129)]  #year and plot ID plus veg data columns only

  # Calculate total number of occurrences of each spp
  vegtot <- apply(X=veg[, 3:109], MARGIN=2, FUN=sum)
  
  # Create a dataframe of vegtot data for later flexible filtering
  vegtot.df <- as.data.frame(vegtot)
  vegtot.df$species <- rownames(vegtot.df)
  rownames(vegtot.df) <- NULL
  vegtot.df <- rename(vegtot.df, n.occ=vegtot)
  
  rm(vegtot)

#   sum(vegtot >= 50)  #29 spp with at least 50 occurrences out of 665 plot observations
#   common.spp <- as.data.frame(vegtot[vegtot>=50]) 
#   common.spp$species <- row.names(common.spp)
#   row.names(common.spp) <- NULL
#   colnames(common.spp)[1] <- "n.occ"
  
#   lesscommon.spp <- as.data.frame(vegtot[vegtot<50 & vegtot>=20]) 
#   lesscommon.spp$species <- row.names(lesscommon.spp)
#   row.names(lesscommon.spp) <- NULL
#   colnames(lesscommon.spp)[1] <- "n.occ"


## 2. Inundation durations
##    For now, use previously-calculated inundur at 15 standard decay rates
  flowhist.sm  <- read.csv("Inundurations_lambdas_allyrs_small.csv")
  flowhist.lg  <- read.csv("Inundurations_lambdas_allyrs_large.csv")
  flowhist.all <- read.csv("Inundurations_lambdas_allyrs_all.csv")

  
## 3. Plan traits
  traits <- read.csv("BLCA_WP_traitdata.csv")

  # Combine traits and vegtot information
  traits <- merge(traits, vegtot.df, by="species", all=TRUE)
  rm(vegtot.df) 