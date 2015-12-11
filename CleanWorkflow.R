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


#### LOOP THROUGH SPECIES ####

# Create empty data frame to be filled - same column structure as lmloop
  bestmodels <- data.frame(species=character(), lambda=character(), n.occ=double(), model=character(), 
                           spr.01=double(), sb0=double(), sb1=double(), schistat=double(), spval=double(), 
                           sr2.nag=double(), sr2.mf=double(), 
                           gu=double(), gt=double(), gpmax=double(), gchistat=double(), gpval=double(), 
                           gr2.nag=double(), gr2.mf=double(), 
                           ochistat=double(), or2.nag=double())
  
  bestmodels[1:nrow(spp.subset), 1:20] <- NA
  bestmodels$lambda  <- as.character(bestmodels$lambda)
  bestmodels$species <- as.character(bestmodels$species)
  bestmodels$model   <- as.character(bestmodels$model)


  # Call "picklambda" function for all spp  
  # !! to print pdfs, start the pdf device before running the loop
    for (i in 1:nrow(spp.subset)) {
      print(paste(i, spp.subset$species[i]))
      out.cur <- picklambda(species=as.character(spp.subset$species[i]))
      
      # print(out.cur$lmplot) #not needed when printing both plots together with grid.arrange
      
      bestmodels[i, ] <- out.cur$spdata
      
      #Re-run the models with the newly selected lambda in order to generate a plot of the model fit
      newspmodel <- calcmodels(sp.cur=bestmodels[i, 1], 
                               lm.cur=bestmodels[i, 2], 
                               mod.type = model.type, 
                               exclude.130=FALSE)
      
      # Print the plot (or, comment this out to make the code run a bit faster)
      # print(newspmodel$fitplot)  #this one isn't needed when printing both plots together with grid.arrange
      print(grid.arrange(newspmodel$fitplot, out.cur$lmplot, ncol=2))
    }

# To print a PDF: 
  pdf(file=paste(DirOut, "MFPlots_PA_Dyn_Exp_AllLm_50occ.pdf", sep=""), 
      width=14, height=6)

  #(run the for loop)

  dev.off()

# To print a data file:
  write.csv(bestmodels, 
            paste(DirOut, "MFStats_PA_Dyn_Exp_AllLm_50occ.csv", sep=""), 
            row.names=FALSE)

# Some basic diagnostics:
  # How many models have R2.nag of at least 0.1?
    sum(bestmodels$or2.nag>=0.1)
  # Sig vs Gauss
    table(bestmodels$model)
    table(bestmodels$model[bestmodels$or2.nag>0.1])
  # What are the best fit models?
    tail(select(arrange(bestmodels, or2.nag), species, or2.nag))
  # What are the lambda values of the good (i.e., R2>0.1) models?
    table(as.numeric(bestmodels$lambda))
    table(as.numeric(bestmodels$lambda)[bestmodels$or2.nag>=0.1])
    log10(as.numeric(bestmodels$lambda[bestmodels$or2.nag>=0.1]))
  # Histogram of selected lambdas
    ggplot(data=bestmodels[bestmodels$or2.nag>0.1, ], 
           aes(x=factor(round(log10(as.numeric(lambda)),1)))) +
      geom_histogram() +
      xlab("log10(lambda)")
  # Plot of R2 vs selected lambda
   p <- ggplot(data=bestmodels, 
           aes(x=factor(round(log10(as.numeric(lambda)),1), ordered=TRUE)) ) +
      geom_point(aes(y=as.numeric(or2.nag))) +
      xlab("log10(lambda)") + 
      ggtitle("Model fit (R2) vs selected lambda: Dynamic, PA, >=50")
    p + geom_segment(aes(x=factor(-3), xend=factor(-4.7), y=0.1, yend=0.1))
