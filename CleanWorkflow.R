#### PREPARATION ####
# First, run <Setup_Exponential.R> to set directories and libraries and read in data
  source.with.encoding("Z:\\asb\\BlackCanyon2015\\WarnerPoint\\Exponential_Model\\Code\\Setup_Exponential.R", 
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
  #  source flowhist df as necessary. 
  flowhist <- flowhist.all   #!!Or, change this to flowhist.sm, or something else. 
    rm(flowhist.all)
    rm(flowhist.sm)
    rm(flowhist.lg)
  lambdas <- as.numeric(substr(colnames(flowhist), 9, 21))[5:ncol(flowhist)]
  
# Set the default model type: presence/absence
  model.type <- "p/a"  #set pres-abs as the standard model type (for labelling plots)

# Generate a df of selected species set; filter \traits\ by desired characteristic 
  spp.subset <- filter(traits, n.occ>=50)
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
    veg <- veg.loss
#     veg <- veg.orig
  #   ##

    # !! for losses only:
    veg[veg==-1] <- 1  #need to edit -1 "loss" values to +1, "presence of a loss"
  
  # Select gain/loss spp to use  !!Pick just one!!
    # Option 1: GAINS
    traits.gain <- merge(traits, gain.counts, by="species", all=TRUE)
    gain.spp <- traits.gain[(traits.gain$n.gain>=20), ]  
                              traits.gain$n.samp>=50 &
                              traits.gain$pct.gain>=0.05), ] 
                              traits.gain$pct.gain <0.05), ]  #list of eligible spp for fitting a model on gains 
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
  calcmod.test <- calcmodels(sp.cur="ANITEC", lm.cur=lambdas[5], trans="sqrt", mod.type=model.type, exclude.130=FALSE)
    calcmod.test$fitplot 
    calcmod.test$param

# picklambda (try all lm values for one sp, find the best one)
  picklm.test <- picklambda(species="ANITEC", transformation="sqrt")
    picklm.test$lmplot
    picklm.test$spdata


## Next, go to FitModels_MultTrans.R or FitModels_OneTrans.R 
## to loop through multiple species, produce plots and 
## model fit stats. 
####################################