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

#   test <- data.frame(species=character(), transf=character())
#   test[1:(3*nrow(spp.subset)), 1:2] <- NA
#   test$species <- as.character(test$species)
#   test$transf  <- as.character(test$transf)
# # Set the transformation to use when running the models
#   trans.cur <- "sqrt"
# 
# for(i in 1:nrow(spp.subset)) {
#   print(spp.subset$species[i])
#   
#   for (j in 1:3){   #do the whole process three times, once for each transformation
#     trans.cur <- trans.options[j]
#     print(trans.cur)
#     
#     test[j+((i-1)*3), 1] <- as.character(spp.subset$species[i])
#     test[j+((i-1)*3), 2] <- trans.cur
#   }
#   
# } 
#   for(i in 1:nrow(spp.subset)) {
#     test[i+((j-1)*nrow(spp.subset)), 1] <- as.character(spp.subset$species[i])
#     test[i+((j-1)*nrow(spp.subset)), 2] <- trans.cur
#   }
  
  # Call "picklambda" function for all spp, using current transformation  
  # !! to print pdfs, start the pdf device before running the loop
  for (i in 1:nrow(spp.subset)) {
    print(paste(i, spp.subset$species[i]))
    
    # Clean list to save plots in (must be re-generated before each run of the j-loop)
    gvec<-vector("list", length=3)
    
    for (j in 1:3) {
      trans.cur <- trans.options[j]
#       print(trans.cur)
      
      out.cur <- picklambda(species=as.character(spp.subset$species[i]), 
                            transformation=trans.cur)
      
      # print(out.cur$lmplot) #not needed when printing both plots together with grid.arrange
      
      bestmodels[j+((i-1)*3), 1:20] <- out.cur$spdata
      bestmodels[j+((i-1)*3), 21]   <- trans.cur
      
#       test[j+((i-1)*3), 1] <- as.character(spp.subset$species[i])
#       test[j+((i-1)*3), 2] <- trans.cur
  
      
      #Re-run the models with the newly selected lambda in order to generate a plot of the model fit
      newspmodel <- calcmodels(sp.cur=bestmodels[j+((i-1)*3), 1], 
                               lm.cur=bestmodels[j+((i-1)*3), 2],
                               trans=trans.cur, 
                               mod.type = model.type, 
                               exclude.130=FALSE)
      
      # Print the plot (or, comment this out to make the code run a bit faster)
      # print(newspmodel$fitplot)  #this one isn't needed when printing both plots together with grid.arrange
#       print(grid.arrange(newspmodel$fitplot, out.cur$lmplot, ncol=2))
      twopanel <- arrangeGrob(newspmodel$fitplot, out.cur$lmplot, ncol=2)

      # Save plot to plot list
      gvec[[j]] <- twopanel
    }
    
  print(grid.arrange(gvec[[1]], gvec[[2]], gvec[[3]], ncol=1))
  }

# To print a PDF: 
  pdf(file=paste(DirOut, "MFPlots_PA_Dyn_Exp_Sqrt_AllLm_20occ.pdf", sep=""), 
      width=14, height=6)

  pdf(file=paste(DirOut, "MFPlots_PA_Dyn_Exp_AllTran_20occ.pdf", sep=""), 
      width=14, height=18)

  #(run the for loop)

  dev.off()

# To print a data file:
  write.csv(bestmodels, 
            paste(DirOut, "MFStats_PA_Dyn_Exp_AllTran_20occ.csv", sep=""), 
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

# Merge 'bestmodels' output from various transformations
test <- cbind(best.base.50[, c(1:4, 19, 20)], best.sqrt.50[, c(1:4, 19, 20)])