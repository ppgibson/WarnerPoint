#### PREPARATION ####
# First, run <Setup_Exponential.R> to set directories and libraries and read in data
  source.with.encoding("Z:\\asb\\BlackCanyon2015\\WarnerPoint\\Exponential_Model\\WarnerPt_Analysis_RProj\\SetupExponential.R", 
                        encoding="UTF-8")

# Extra libraries
  library(gridExtra)
  library(scales)

# Source model-calculation custom functions
  source.with.encoding(paste(DirCode, "function_calcmodels_transformations.R", sep=""), 
                       encoding="UTF-8")

  source.with.encoding(paste(DirCode, "function_picklambda.R", sep=""), 
                       encoding="UTF-8")

# Generate a vector of the (pre-determined) lambda values 
  # !Depends on which flow history data is being used! Change the 
  #  source of \flowhist\ df as necessary. 
  flowhist.exp <- read.csv("Inundurations_lambdas_allyrs_all.csv") #Inundation durations calculated with exponential weighting.
  flowhist.lin <- read.csv("Inundurations_linear_allyrs.csv")      #Inundation durations calculated with linear weighting.
  flowhist.sea <- read.csv("Inundurations_seasonal_allyrs_IncCurSeas.csv")  #Inundation durations calculated with seasonal exponential weigthting, current season is included.
#   flowhist.sea.nocur <- read.csv("Inundurations_seasonal_allyrs_NoCurSeas.csv")
  flowhist <- flowhist.sea   #!!Choose one (or read in/replace it with something else). 
#   flowhist <- flowhist.sea.nocur

  lambdas <- as.numeric(substr(colnames(flowhist), 9, 21))[5:ncol(flowhist)]
  
# Set the default model type: presence/absence
  model.type <- "p/a"  #set pres-abs as the standard model type (for labelling plots)

# Generate a df of selected species set; filter \traits\ by desired characteristic 
  spp.subset <- filter(traits, n.occ>=50)
  spp.subset <- filter(traits, n.occ<50 & n.occ>=20)
  # Or, just count number of spp with desired traits
  sum(traits$n.occ >= 50)
  sum(traits$annual.tend=="Yes", na.rm=TRUE)  

## GAINS AND LOSSES ##
  # Source the gain/loss setup
    source.with.encoding(paste(DirCode, "Setup_GainLoss.R", sep=""), 
                       encoding="UTF-8")
  
  # Store change data as \veg\
    veg.orig <- veg     #First, store a clean copy of veg...
  #   ##
#     veg <- veg.gains  #...then, pick a veg dataset to use, gains or losses.
#     veg <- veg.loss
    veg <- veg.orig
  #   ##

    # !! for losses only:
    veg[veg==-1] <- 1  #need to edit -1 "loss" values to +1, "presence of a loss"
  
  # Select gain/loss spp to use  !!Pick just one!!
    # Option 1: GAINS
    traits.gain <- merge(traits, gain.counts, by="species", all=TRUE)
    gain.spp <- traits.gain[(traits.gain$n.gain>=20), ]  
    spp.subset <- gain.spp
    model.type <- "gain"

    # Option 2: LOSS
    traits.loss <- merge(traits, loss.counts, by="species", all=TRUE)
    loss.spp <- traits.loss[(traits.loss$n.nochange>=20 & 
                             traits.loss$n.loss>=20), ]
    spp.subset <- loss.spp
    model.type <- "loss"

## END GAINS AND LOSSES ##


#### TEST CODE ####
# calcmodels (fit one sp/lm combination)
  calcmod.test <- calcmodels(sp.cur="CARLAN", lm.cur=lambdas[32], 
                             trans="base", weighting="seas", 
                             mod.type=model.type, exclude.130=FALSE)
    calcmod.test$fitplot 
    calcmod.test$param

# picklambda (try all lm values for one sp, find the best one)
  picklm.test <- picklambda(species="CARLAN", transformation="base", weights="seas")  #!note that changing the 'weights' argument only changes how the plot is labeled - in order to change underlying data, you need to change the data stored as \flowhist\.
    picklm.test$lmplot
    picklm.test$spdata


## Next, go to FitModels_MultTrans.R or FitModels_OneTrans.R 
## to loop through multiple species, produce plots and 
## model fit stats. 
####################################