## calcmodels: a function to fit a sigmoid Gaussian logistic 
## models for a given species and lambda value
# 
# Inputs:
#   1. sp.cur (the species)
#   2. lm.cur (the lambda value, numeric)
#   3. mod.type (p/a for presence absence, or gains or loss)
#   3. exclude.130=FALSE (set to TRUE in order to remove dry plot P130)
#
# Process: 
#   1. Fit sigmoid and Gaussian models predicting species presence 
#      as a function of plot inundation duration, using the inundur
#      calculated with the specified lambda value.
#   2. Identifies which model shape should be used, using a parsimony
#      criterion, as described by Jongman et al: does the Gaussian
#      model improve model fit significantly, relative to sig fit, 
#      enough to justify retaining the extra term in the model? 
#      --> note that if smp size is too small, Gauss model does not converge;
#          result is a not-fitted function in the plot, and official sigmoid shape.
#
# Output is a list containing
#   [1] sigfit  = glm object for the sigmoid model
#   [2] gausfit = glm object for the Gaussian model 
#   [3] param   = a vector of model parameters/stats for both models
#   [4] fitplot = a ggplot object, plot of the species presences/
#                 absences, with overlain lines showing the sig and 
#                 Gaus models. 
#

#'trans' must be one of {base, sqrt, log}
#'weighting' must be one of {exp, lin} 
# use 'max.q' to establish what plots should be excluded; set to 12000 to include all plots;
#   11000 will exclude the top plot (P130);
#    8000 will exclude the top five plots;
#    other values can be used also, of course.  

calcmodels <- function(sp.cur, lm.cur, trans, weighting="exp", mod.type="p/a", max.q=12000) {  
  # Establish local environment, otherwise ggplot won't recognize variables defined within function
    localenv <- environment()
  
  # Extract correct column index for flow data
    flow.col.ind <- which(as.numeric(substr(colnames(flowhist), 9, 21)) %in% lm.cur)  #ignore the warnings
    flow.col <- colnames(flowhist)[flow.col.ind]
    veg.col  <- which(colnames(veg)==sp.cur)
    
    # How many times does the current sp occur?
    occ.cur <- traits$n.occ[traits$species==sp.cur]
  
  # Formatted 'lambda' for display in plots (default is exp plots, with an option for lin plots)
    lm.disp <- paste("1e", round(log10(as.numeric(lm.cur)), 1), sep="")
    weight.strength.type <- "lambda"
    # For linear plots 
      if(weighting=="lin") {
        lm.disp <- as.numeric(lm.cur)
        weight.strength.type <- "rec.lngth"
      }
  
  # Combine current flow and veg data
    dat.cur <- merge(veg[, c("year", "plot", sp.cur)], flowhist[, c("year", "plot", flow.col, "q.inund")], 
                     by=c("year", "plot"), all=TRUE)
    colnames(dat.cur)[3:4] <- c("presence", "base.inundur")
    dat.cur <- filter(dat.cur, !is.na(presence))  #eliminate NA rows (should apply only to gain/loss models)

  # Remove data for any plots excluded by q.inund critera
  # (i.e., remove dry outlier plots if desired)
    dat.cur <- filter(dat.cur, q.inund < max.q)
 
  # Calculate transformations 
    dat.cur <- mutate(dat.cur, 
                      sqrt.inundur=((base.inundur)^0.5), 
                      log.inundur=log10(base.inundur))
      # Note that the log trans requires dealing with data points where inundur = 0 (resulting in unusable log.inundur=-Inf)
      # Current resolution: set these zero values to one tenth of the next-smallest inundur value,
      # which is mathematically equivalent to the next-lowest log value minus 1:
      inf.ind <- which(dat.cur$log.inundur==-Inf)  #index of the rows where inundur is 0/log.inund is -inf
      dat.cur$log.inundur[inf.ind] <- min(dat.cur$log.inundur[dat.cur$log.inundur!=-Inf]) - 1
      rm(inf.ind)
  
  # Colname for desired transformation
    hydro.col <- paste(trans, ".inundur", sep="")
 
  # Base plot: occ vs inundur
    base <- ggplot(data=dat.cur, environment=localenv, aes_string(x=hydro.col)) +
      geom_point(aes(y=presence)) +
      ggtitle(paste(sp.cur, 
                    ", lambda=", lm.cur, 
                    ", n.occ=", occ.cur, sep=""))

  
  # Now the models
    nullfit.cur <- glm(presence ~ 1, 
                       data=dat.cur, family="binomial")
    sigfit.cur  <- glm(presence ~ get(hydro.col),
                       data=dat.cur, family="binomial")
    gausfit.cur <- glm(presence ~ get(hydro.col) + I(get(hydro.col)^2), 
                       data=dat.cur, family="binomial")
  
  # Gaussian or sigmoid? 
    # Compare deviances
      delta.dev <- sigfit.cur$deviance - gausfit.cur$deviance
    # vs. Chi-sq critical value with num.df = k, the number of additional parameters in quad vs simple mod.
      chi.crit <- qchisq(p=0.95, df=1) #which is always going to be 3.841, since df is always same
    # is the change in deviance larger than the critical value?
    #  If so, then the gaussian model is significantly better.  If not, then
    #  there is not enough evidence to conclude the gaussian model is better than simple logit.
      retaingaus <- delta.dev > chi.crit  #TRUE for gaussian, FALSE for sigmoid

#   retaingaus <- FALSE #********************TEMP ADDITION, FIX LATER

  
  # Generate new x values for plotting (y vals will depend on Gauss vs sig)
    xvals <- seq(from=min(dat.cur[, hydro.col]), to=max(dat.cur[, hydro.col]), length.out=100)
    modplot.data <- data.frame(x=xvals)  
    colnames(modplot.data)[1] <- as.character(hydro.col) #colname must match hydro.col name in order for predict function to work


  # [S]igmoid model outputs: Prob of occ (at various ID); param b0 b1; stats X2, p, R2
    # Parameters
      sb0 <- coef(sigfit.cur)[1] #the "intercept"
      sb1 <- coef(sigfit.cur)[2]
    # Probability of occupancy
      spr.0001 <- (exp(sb0 + sb1*log10(0.0001))) / (1 + (exp(sb0 + sb1*log10(0.0001))))
      spr.01   <- (exp(sb0 + sb1*log10(0.01)))   / (1 + (exp(sb0 + sb1*log10(0.01))))
      spr.8    <- (exp(sb0 + sb1*log10(0.8)))    / (1 + (exp(sb0 + sb1*log10(0.8))))
    # Model statistics
      # Likelihood ratio test statistic, distributed as Chisq with 1 df
        schistat <- sigfit.cur$null.deviance - sigfit.cur$deviance  #test statistic is difference between null mod deviance and model deviance.
        spval <- 1 - (pchisq(schistat, df=1))
      # Nagelkerke max-rescaled R2
      # (http://yatani.jp/teaching/doku.php?id=hcistats:logisticregression)
        sr2.nag  <- (1 - exp(-2 * (logLik(sigfit.cur) - logLik(nullfit.cur)) / nrow(dat.cur) )) / 
                    (1 - exp(2 * logLik(nullfit.cur)/nrow(dat.cur) ))
      # McFadden's pseudo-R2
      # (http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/)
        sr2.mf <- 1 - (logLik(sigfit.cur) / logLik(nullfit.cur) )
    # Predicted values for plotting
      yvals.sig <- predict(sigfit.cur, newdata=modplot.data, type="response")
  
  # [G]aussian model outputs: u, t, pmax, X2, p, R2
    # Parameters - model world
      gb0 <- coef(gausfit.cur)[1] #the "intercept"
      gb1 <- coef(gausfit.cur)[2]
      gb2 <- coef(gausfit.cur)[3]
    # Parameters - real world (or rather, log10(real.world))
      gu    <- (-1*gb1) / (2*gb2)
      gt    <- 1 / sqrt(-2*gb2)
      gpmax <- (exp(gb0 + gb1*gu + gb2*(gu)^2)) / (1 + (exp(gb0 + gb1*gu + gb2*(gu)^2)))
    # Model statistics
      # Likelihood ratio test statistic, distributed as Chisq with 2 df
        gchistat <- gausfit.cur$null.deviance - gausfit.cur$deviance  #test statistic is difference between null mod deviance and model deviance.
        gpval <- 1 - (pchisq(gchistat, df=2))
      # Nagelkerke max-rescaled R2
      # (http://yatani.jp/teaching/doku.php?id=hcistats:logisticregression)
        gr2.nag  <- (1 - exp(-2 * (logLik(gausfit.cur) - logLik(nullfit.cur)) / nrow(dat.cur) )) / 
                    (1 - exp(2 * logLik(nullfit.cur)/nrow(dat.cur) ))
      # McFadden's pseudo-R2
      # (http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/)
        gr2.mf <- 1 - (logLik(gausfit.cur) / logLik(nullfit.cur) )
        if((as.numeric(gr2.mf))>0.99) {  #if the gaussian R2=1 it means the gaus model failed to converge, use sig instead. But for some reason=1 doesn't work, so use > 0.99.
          retaingaus <- FALSE
        }
    # Predicted values for plotting
      yvals.gaus <- predict(gausfit.cur, newdata=modplot.data, type="response")



  # Final model shape  
    model.shape <- "Sigmoid"  #Sigmoid is the default;
      if(retaingaus==TRUE) {    #...but set to Gaussian if retaingaus is TRUE
        model.shape <- "Gaussian"
      }

  # Determine the overall X2 and R2 values to use for model comparison
    # Default is sigmoid model values
      ochistat <- schistat
      or2.nag  <- sr2.nag
    # ...But switch to Gaussian model values if retaingaus=TRUE
      if(retaingaus==TRUE) {
        ochistat <- gchistat
        or2.nag  <- gr2.nag
      }

  
  # Combine all model outputs into one vector
    modstats <- c(sp.cur, lm.cur, occ.cur, model.shape, #1-4
                  spr.01, sb0, sb1, schistat, spval, sr2.nag, sr2.mf, #5-11
                  gu, gt, gpmax, gchistat, gpval, gr2.nag, gr2.mf, #12-18
                  ochistat, or2.nag)  #19-20

    
  # Plot with fitted functions
    # DF for smooth model plot
    modplot.data <- cbind(modplot.data, yvals.sig, yvals.gaus)


    logplot <- base + 
      ylab("prob(presence)") + 
#       coord_cartesian(xlim=c(-6, 0.5)) +   #to focus the plot on the main region, without the -15 point
      ggtitle(paste(mod.type, "  ", sp.cur, "   ", weighting, " wts 
     ", weight.strength.type, "=", lm.disp, 
                    ", n.occ=", occ.cur,
                    ", mod=", model.shape, ", X2=", round(ochistat, 2), 
                    ", R2=", round(or2.nag,2), sep=""))  
  

#     if(retaingaus==TRUE) {
       logplot <- logplot + geom_line(data=modplot.data, 
                                      aes(x=xvals, y=yvals.gaus), color="blue")  
#       }

  
#     if(retaingaus==FALSE) {
        logplot <- logplot + geom_line(data=modplot.data,
                                       aes(x=xvals, y=yvals.sig), color="red")  
#       }  

                    
    
  # Combine all relevant information into a single list
    output <- list(sigfit  = sigfit.cur,
                   gausfit = gausfit.cur,
                   param = modstats,
                   fitplot = logplot)
  
    return(output)
  }
