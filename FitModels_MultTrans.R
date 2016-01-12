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
  weighting.cur <- "seas"

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
  pdf(file=paste(DirOut, "MFPlots_PA_Dyn_Seas_AllTran_20occ_IncCur.pdf", sep=""), 
      width=14, height=18)

  #(run the for loop)

  dev.off()


# To print a data file:
  write.csv(bestmodels, 
            paste(DirOut, "MFStats_Loss_Dyn_Seas_AllTran_20occ_IncCur.csv", sep=""), 
            row.names=FALSE)

## For code to compare/evaluate models, go to < ModelEvaluations.R >
####################################################################