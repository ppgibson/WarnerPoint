## picklambda: a function to find the best lambda value for a species
# 
# Inputs:
#   1. species
#   2. (indirectly) vector of lambda values, "lambdas"
#
# Process: 
#   1. Call the function \calcmodels\ once for each value in 
#      "lambdas" (always for the given species)
#   2. Identify the which lambda value produces the model with
#      the best fit (as defined by highest value of or2.nag, the
#      Nagelkerke max-rescaled R2)
#
# Output is a list containing
#   [1] spdata = a vector all the model parameters/stats (for the
#                best-fit value of lambda)
#   [2] lmplot = a ggplot object of lambda value vs. model fit (as
#                the chi-square value), color of plots indicates 
#                model shape (Gaus vs sig), title indicates best lambda.
#   [3] lmdata = a df of model parameters/stats for all lambda values
#

#weights must be one of {exp[default], lin, seas}
picklambda <- function(species, transformation, weights) {
  # Environment
    localenv <- environment()
    
  # Empty data frame to be filled
    lmloop <- data.frame(species=character(), lambda=character(), n.occ=double(), model=character(), 
                         spr.01=double(), sb0=double(), sb1=double(), schistat=double(), spval=double(), 
                            sr2.nag=double(), sr2.mf=double(), 
                         gu=double(), gt=double(), gpmax=double(), gchistat=double(), gpval=double(), 
                            gr2.nag=double(), gr2.mf=double(), 
                         ochistat=double(), or2.nag=double())
        
    lmloop[1:length(lambdas), 1:ncol(lmloop)] <- NA  
      lmloop$lambda  <- as.character(lmloop$lambda)
      lmloop$species <- as.character(lmloop$species)
      lmloop$model   <- as.character(lmloop$model)
  
  
  # Loop through the different lambda values (for a single species)
    for (i in 1:length(lambdas)) {
#       print(paste(i, colnames(flowhist)[i+3]))
      out.cur <- calcmodels(sp.cur=species, lm.cur=lambdas[i], trans=transformation, weighting=weights, exclude.130=FALSE)
#         print(out.cur$fitplot)
      
      lmloop[i, ] <- out.cur$param
    }

  
  # Extract data for the best lambda value
    max.index  <- which(lmloop$ochistat==max(as.numeric(lmloop$ochistat)))
    spdata.cur <- lmloop[max.index, ]
    dur.cur <- traits$duration[traits$species==species]

  # Formatted 'lambda' information for display in plots (default is exp plots, with an option for lin plots)
    lm.disp <- paste("1e", round(log10(as.numeric(spdata.cur[2])), 1), sep="")
    weight.strength.type <- "lambda"
    # For linear plots
      if(weights=="lin") {
        lm.disp <- as.numeric(spdata.cur[2])
        weight.strength.type <- "rec.lngth"
      }

  # Plot model success measure (X2 or R2) vs. lambda: which lambda produces the best model fit?
    lambdaplot <- ggplot(data=lmloop, environment=localenv, aes(x=as.numeric(lambda))) +  #for now, exclude the rows for lm=0.5 and lm=1
      geom_point(aes(y=as.numeric(ochistat), color=model)) +
      geom_point(aes(x=as.numeric(lambda)[max.index], 
                     y=as.numeric(ochistat)[max.index]), color="green", size=4, shape=1) +  #make the point for the best-fit model bigger so it will stand out.
      ylab("lik. ratio test statistic (X2)") +
      xlab(weight.strength.type) +
      scale_colour_manual(values=c("Gaussian"="blue", "Sigmoid"="red"), labels=c("Gaussian"="Gaussian", "Sigmoid"="Sigmoid") ) +
#       coord_cartesian(xlim=c(0, 0.1)) +
#       scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x) ) +  #add labels=comma to get sci notation to disp as plain decimals
      ggtitle(paste(lmloop[1,1], "
      ", weights, " wts, best=", lm.disp, 
                    "  duration=", dur.cur, "  trans=", transformation, sep="")) 

    # Add log-scale x-axis for exponential weights
      if (weights!="lin") {
        lambdaplot <- lambdaplot + scale_x_continuous(trans=log10_trans())
      }

   
  # Return plot and data
    output <- list(spdata = spdata.cur,
                   lmplot = lambdaplot,
                   lmdata = lmloop)
}