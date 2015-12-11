## This code directly follows CleanWorkflow.R. 
## For a specified set of species (\spp.subset\ dataframe, established 
## in CleanWorkflow.R), run the picklambda function, combine model stats
## in a single dataframe (\bestmodels\) and print a two panel plot (model
## fit plot and lambda vs X2 plot).

## ! This version of the code runs the models/produces output for only a single 
## transformation of the inundur values.  To run multiple transformations at once,
## use <FitModels_multTrans.R>

#### LOOP THROUGH SPECIES ####
# Set the transformation to use when running the models
  trans.cur <- "base"  #One of {base, sqrt, log}

# Create empty data frame to be filled - same column structure as lmloop
  bestmodels <- data.frame(species=character(), lambda=character(), n.occ=double(), model=character(), 
                           spr.01=double(), sb0=double(), sb1=double(), schistat=double(), spval=double(), 
                           sr2.nag=double(), sr2.mf=double(), 
                           gu=double(), gt=double(), gpmax=double(), gchistat=double(), gpval=double(), 
                           gr2.nag=double(), gr2.mf=double(), 
                           ochistat=double(), or2.nag=double(),
                           transformation=character())
  
  bestmodels[1:nrow(spp.subset), 1:20] <- NA
  bestmodels$lambda  <- as.character(bestmodels$lambda)
  bestmodels$species <- as.character(bestmodels$species)
  bestmodels$model   <- as.character(bestmodels$model)
  bestmodels$transformation <- as.character(bestmodels$transformation)


  # Call "picklambda" function for all spp  
  # !! to print pdfs, start the pdf device before running the loop
    for (i in 1:nrow(spp.subset)) {
      print(paste(i, spp.subset$species[i]))
      out.cur <- picklambda(species=as.character(spp.subset$species[i]),
                            transformation=trans.cur)
      
      # print(out.cur$lmplot) #not needed when printing both plots together with grid.arrange
      
      bestmodels[i, ] <- out.cur$spdata
      
      #Re-run the models with the newly selected lambda in order to generate a plot of the model fit
      newspmodel <- calcmodels(sp.cur=bestmodels[i, 1], 
                               lm.cur=bestmodels[i, 2], 
                               trans=trans.cur,
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
