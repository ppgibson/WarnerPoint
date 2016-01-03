## This code directly follows CleanWorkflow.R. 
## For a specified set of species (\spp.subset\ dataframe, established 
## in CleanWorkflow.R), run the picklambda function for each transformation 
## of inundur values (base, sqrt, log), combine model stats
## in a single dataframe (\bestmodels\) and print a six-panel plot (model
## fit plot and lambda vs X2 plot for each transformation).

## ! This version of the code runs the models/produces output for multiple 
## transformations of the inundur values.  To run only a single transformation 
## use <FitModels_OneTrans.R>.


#### LOOP THROUGH SPECIES AND TRANSFORMATIONS ####
# Possible transformation variables (note more could be added later...)
  trans.options <- c("base", "sqrt", "log")
# Weighting of inundation durations (**choose one)
  weighting.cur <- "lin"

# Create empty data frame to be filled - same column structure as lmloop
  bestmodels <- data.frame(species=character(), lambda=character(), n.occ=double(), model=character(), 
                           spr.01=double(), sb0=double(), sb1=double(), schistat=double(), spval=double(), 
                           sr2.nag=double(), sr2.mf=double(), 
                           gu=double(), gt=double(), gpmax=double(), gchistat=double(), gpval=double(), 
                           gr2.nag=double(), gr2.mf=double(), 
                           ochistat=double(), or2.nag=double(), 
                           transformation=character())
  
  bestmodels[1:(3*nrow(spp.subset)), 1:21] <- NA
  bestmodels$lambda  <- as.character(bestmodels$lambda)
  bestmodels$species <- as.character(bestmodels$species)
  bestmodels$model   <- as.character(bestmodels$model)
  bestmodels$transformation <- as.character(bestmodels$transformation)


# Call "picklambda" function for all spp, using current transformation  
  # !! to print pdfs, start the pdf device before running the loop
  for (i in 1:nrow(spp.subset)) {
    print(paste(i, spp.subset$species[i])) #output to keep track of how the model run is progressing.
    
    # Clean list in which to store plots (list must be re-generated before each run of the j-loop)
    gvec <- vector("list", length=3)
    
    for (j in 1:3) {  #once for each transformation
      trans.cur <- trans.options[j]
      
      # Call picklambda function
      out.cur <- picklambda(species=as.character(spp.subset$species[i]), 
                            transformation=trans.cur, 
                            weights=weighting.cur)
      
      # Store output in correct row of the compiling df
      bestmodels[j+((i-1)*3), 1:20] <- out.cur$spdata
      bestmodels[j+((i-1)*3), 21]   <- trans.cur
      
      #Re-run the models with the newly selected lambda in order to generate a plot of the model fit
      newspmodel <- calcmodels(sp.cur=bestmodels[j+((i-1)*3), 1], 
                               lm.cur=bestmodels[j+((i-1)*3), 2],
                               trans=trans.cur, 
                               weighting=weighting.cur, 
                               mod.type=model.type, 
                               exclude.130=FALSE)
      
      # Arrange the model plot and lambda plot side by side
      twopanel <- arrangeGrob(newspmodel$fitplot, out.cur$lmplot, ncol=2)

      # Save the two-pane plot to plot list
      gvec[[j]] <- twopanel
    }
    
  # Arrange all six panels, two for each transformation, on a single pdf page (one page for each sp) 
  print(grid.arrange(gvec[[1]], gvec[[2]], gvec[[3]], ncol=1))
  }


# To print a PDF: 
  pdf(file=paste(DirOut, "MFPlots_PA_Dyn_Lin_AllTran_50occ.pdf", sep=""), 
      width=14, height=18)

  #(run the for loop)

  dev.off()


# To print a data file:
  write.csv(bestmodels, 
            paste(DirOut, "MFStats_PA_Dyn_Lin_AllTran_50occ.csv", sep=""), 
            row.names=FALSE)


# Some basic diagnostics:
  # How many models have R2.nag of at least 0.1?
    sum(bestmodels$or2.nag>=0.1)
    nrow(bestmodels) #...out of how many possible?
  # Sig vs Gauss
    table(bestmodels$model)
    table(bestmodels$model[bestmodels$or2.nag>0.1])
  # What are the best fit models?
    tail(select(arrange(bestmodels, or2.nag), species, or2.nag))
  # What are the lambda values of the good (i.e., R2>0.1) models?
    table(log10(as.numeric(bestmodels$lambda)))
    table(log10(as.numeric(bestmodels$lambda)[bestmodels$or2.nag>=0.1]))
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

## Compare results among the different transformations
# Read in different versions of \bestmodels\
  best.50 <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Exp_AllTran_50occ.csv", sep=""))
  bestmodels <- best.50
  best.20 <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Exp_AllTran_20occ.csv", sep=""))
  bestmodels <- best.20
  best.gain <- read.csv(paste(DirOut, "MFStats_Gain_Dyn_Exp_AllTran_20occ.csv", sep=""))
  bestmodels <- best.gain
  best.loss <- read.csv(paste(DirOut, "MFStats_Loss_Dyn_Exp_AllTran_20occ.csv", sep=""))
  bestmodels <- best.gain

# Format the df
  best.bysp <- group_by(bestmodels, species)
  best.bysp <- select(best.bysp, species, lambda, n.occ, model, ochistat, or2.nag, transformation)
  best.bysp <- best.bysp[!is.na(best.bysp$species), ]

# Summarize by species
  # or2
  best.bysp$or2.nag <- as.numeric(best.bysp$or2.nag)
  bestr2 <- summarize(best.bysp, 
                         max.or2 = max(or2.nag))
  bestr2 <- merge(best.bysp, bestr2, 
                by.x=c("species", "or2.nag"), 
                by.y=c("species", "max.or2"), 
                all.x=FALSE, all.y=TRUE)
  table(bestr2$transformation)  #number of species having its best or2 model fit in each transformation.
  or2.wide <- dcast(best.bysp, species ~ transformation, value.var="or2.nag")

  # chisq (which produces same results as or2)
  best.bysp$ochistat <- as.numeric(best.bysp$ochistat)
  bestchi <- summarize(best.bysp, 
                         max.chi = max(ochistat))
  bestchi <- merge(best.bysp, bestchi, 
                by.x=c("species", "ochistat"), 
                by.y=c("species", "max.chi"), 
                all.x=FALSE, all.y=TRUE)
  table(bestchi$transformation)

###################################################