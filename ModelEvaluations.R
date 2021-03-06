# Uncleaned/poorly organized code for 
# -model evaluation
# -comparing results among different transformations
# -comparing results among different weighting schemes
# -looking for relationships between lambda and successional traits

#### MODEL EVALUATION ####
## Some basic diagnostics:
  # How many models have R2.nag of at least 0.1?
    sum(bestmodels$or2.nag>=0.1 & bestmodels$transformation=="log")
    nrow(bestmodels) #...out of how many possible?
  # Sig vs Gauss
    table(bestmodels$model)
    table(bestmodels$model[bestmodels$or2.nag>0.1])
  # What are the best fit models?
    tail(select(arrange(bestmodels, or2.nag), species, or2.nag, transformation))
  # What are the lambda values of the good (i.e., R2>0.1) models?
    table((as.numeric(bestmodels$lambda)))
    table((as.numeric(bestmodels$lambda)[bestmodels$or2.nag>=0.1]))
    log10(as.numeric(bestmodels$lambda[bestmodels$or2.nag>=0.1]))
  # Histogram of selected lambdas
    ggplot(data=bestmodels[bestmodels$or2.nag>0.1, ], 
           aes(x=factor(round(log10(as.numeric(lambda)),1)))) +
      geom_histogram() +
      xlab("log10(lambda)")

    ggplot(data=bestmodels, 
           aes(x=as.numeric(lambda))) +
      geom_histogram() +
      xlab("log10(lambda)")

  # Plot of R2 vs selected lambda
   p <- ggplot(data=bestmodels, 
           aes(x=factor(round(log10(as.numeric(lambda)),1), ordered=TRUE)) ) +
      geom_point(aes(y=as.numeric(or2.nag))) +
      xlab("log10(lambda)") + 
      ggtitle("Model fit (R2) vs selected lambda: Dynamic, PA, >=50")
    p + geom_segment(aes(x=factor(-3), xend=factor(-4.7), y=0.1, yend=0.1))


#### COMPARE TRANSFORMATIONS ####
# Read in different versions of \bestmodels\
  best.50 <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_PA_Dyn_Lin_AllTran_50occ.csv", sep=""))
  bestmodels <- best.50
  best.20 <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_PA_Dyn_Lin_AllTran_20occ.csv", sep=""))
  bestmodels <- best.20
  best.gain <- read.csv(paste(DirOut, "MFStats_Gain_Dyn_Seas_AllTran_20occ_IncCur.csv", sep=""))
  bestmodels <- best.gain
  best.loss <- read.csv(paste(DirOut, "MFStats_Loss_Dyn_Seas_AllTran_20occ_IncCur.csv", sep=""))
  bestmodels <- best.loss

# Format the df
  best.bysp <- group_by(bestmodels, species)
  best.bysp <- select(best.bysp, species, lambda, n.occ, model, ochistat, or2.nag, threshold)
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
  table(bestr2$threshold)  #number of species having its best or2 model fit in each transformation.
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

  # Avg R2 across all spp at each threshold/trans
  bestmodels$or2.nag <- as.numeric(bestmodels$or2.nag)
  bythr <- split(bestmodels$or2.nag, f=list(bestmodels$threshold))
  sapply(X=bythr, FUN=mean)

#### COMPARE SEASONAL WITH AND WITHOUT CURRENT SEASON ####
# Read in different versions of \bestmodels\
  best.inccur.50 <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Seas_AllTran_50occ_IncCur.csv", sep=""))
  best.nocur.50  <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Seas_AllTran_50occ_NoCur.csv", sep=""))
  best.inccur.20 <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Seas_AllTran_20occ_IncCur.csv", sep=""))
  best.nocur.20  <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Seas_AllTran_20occ_NoCur.csv", sep=""))

# Straight-across equivalent versions of bestmodels
  # Combine the 50-occ and 20-occ spp
  best.inccur <- rbind(best.inccur.50, best.inccur.20)
  best.nocur  <- rbind(best.nocur.50, best.nocur.20)
  # Merge into a single df comparing or2
  cursea <- cbind(best.inccur[, c("species", "lambda", "n.occ", "or2.nag")], best.nocur[, 20])
  colnames(cursea)[4:5] <- c("r2.inccur", "r2.nocur")
  cursea <- mutate(cursea, r2.diff = r2.inccur - r2.nocur)
  sum(cursea$r2.diff > 0)  #for 79 0f 153 spp, including cur sea has better fit
  sum(cursea$r2.diff < 0)  #for 74 of 153 spp, excluding cur sea has better fit
  # So, there is definitely no clear preference for one or the other.  
  # What if we combine with trait data?
  #  (use \succ.tr\ from ExploratoryAnalyses.R)
  cursea.tr <- merge(cursea, succ.tr, by="species", all.x=TRUE, all.y=FALSE)
  ggplot(data=cursea.tr[cursea.tr$r2.diff > -0.02, ]) + 
    geom_point(aes(x=succ.ind, y=r2.diff, color=duration), size=3) +
    geom_hline(aes(xintercept=0), lty=2)
    # Try using other variables as x-axis
  ggplot(data=cursea.tr) + 
#   ggplot(data=cursea.tr[cursea.tr$r2.diff > -0.02, ]) + 
    geom_point(aes(x=duration, y=r2.diff, color=duration), size=3) +
    geom_hline(aes(xintercept=0), lty=2)

# Compare magnitudes of differences
  tapply(X=cursea.tr$r2.diff, INDEX=(cursea.tr$r2.diff>0), FUN=mean)
  t.test(abs(cursea.tr$r2.diff[cursea.tr$r2.diff>0]), abs(cursea.tr$r2.diff[cursea.tr$r2.diff<0]))


#### COMPARE WEIGHTING SCHEMES ####
# Read in different versions of \bestmodels\
  # Exponential
  best.50.exp.log <- read.csv(paste(DirOut, "ExpWeighting_AllTrans_ManyLambdas\\MFStats_PA_Dyn_Exp_AllTran_50occ.csv", sep=""))
  best.20.exp.log <- read.csv(paste(DirOut, "ExpWeighting_AllTrans_ManyLambdas\\MFStats_PA_Dyn_Exp_AllTran_20occ.csv", sep=""))
  best.exp <- rbind(best.50.exp, best.20.exp)
  best.exp$wt <- "exp"
  best.gain.exp <- read.csv(paste(DirOut, "ExpWeighting_AllTrans_ManyLambdas\\MFStats_Gain_Dyn_Exp_AllTran_20occ.csv", sep=""))
  best.loss.exp <- read.csv(paste(DirOut, "ExpWeighting_AllTrans_ManyLambdas\\MFStats_Loss_Dyn_Exp_AllTran_20occ.csv", sep=""))
    best.loss.exp <- filter(best.loss.exp, !is.na(species))  #not sure why R reads an extra 23 rows
  # Linear
  best.50.lin <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_PA_Dyn_Lin_AllTran_50occ.csv", sep=""))
  best.20.lin <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_PA_Dyn_Lin_AllTran_20occ.csv", sep=""))
  best.lin <- rbind(best.50.lin, best.20.lin)
  best.lin$wt <- "lin"
  best.gain.lin <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_Gain_Dyn_Lin_AllTran_20occ.csv", sep=""))
  best.loss.lin <- read.csv(paste(DirOut, "LinearWeighting_AllTrans\\MFStats_Loss_Dyn_Lin_AllTran_20occ.csv", sep=""))
  # Seasonal exp
  best.50.seas <- read.csv(paste(DirOut, "SeasonalWeighting_AllTrans\\MFStats_PA_Dyn_Seas_AllTran_50occ_IncCur.csv", sep=""))
  best.20.seas <- read.csv(paste(DirOut, "SeasonalWeighting_AllTrans\\MFStats_PA_Dyn_Seas_AllTran_20occ_IncCur.csv", sep=""))
  best.seas.cur <- rbind(best.50.seas, best.20.seas)
  best.seas.cur$wt <- "sea.cur"
  
  best.50.nocur <- read.csv(paste(DirOut, "SeasonalWeighting_AllTrans\\MFStats_PA_Dyn_Seas_AllTran_50occ_NoCur.csv", sep=""))
  best.20.nocur <- read.csv(paste(DirOut, "SeasonalWeighting_AllTrans\\MFStats_PA_Dyn_Seas_AllTran_20occ_NoCur.csv", sep=""))
  best.seas.nocur <- rbind(best.50.nocur, best.20.nocur)
  best.seas.nocur$wt <- "sea.nocur"

# Combine relevant data
  # Compare R2 across all models
  allmod <- rbind(best.exp, best.lin, best.seas.cur, best.seas.nocur)
  table(list)

  # Compare two df side by side
  comptrans <- merge(best.loss.seas[, c("species", "or2.nag", "transformation")],
                     best.loss.exp[, c("species", "or2.nag", "transformation")], 
                     by=c("species", "transformation"), all=TRUE)
  comptrans <- rename(comptrans, seas=or2.nag.x, exp=or2.nag.y)
  comptrans <- mutate(comptrans, seas.diff = as.numeric(seas) - as.numeric(exp))
  comptrans <- mutate(comptrans, seas.better = (seas.diff>0))

  # For how many spp/trans does linear produce the better fin?
#   sum(comptrans$lin.diff > 0)  #59
#   sum(comptrans$lin.diff < 0)  #28
#   table(comptrans$lin.better)
  table(list(comptrans$transformation, comptrans$seas.better))
  ggplot(data=comptrans) + 
    geom_point(aes(x=transformation, y=seas.diff)) +
    geom_hline(aes(y=0), lty=2)

#### OVERALL STATS ####
  #Overall which is best (for each sp), regardless of transformation?
  # Convert to full long form
#   comptrans.l <- melt(comptrans, id.vars=c("species", "transformation"), variable.name="weights", value.name="or2.nag")
  comp.l.bysp <- group_by(allmod, species)
  compwts <- filter(comp.l.bysp, or2.nag==max(or2.nag))
  table(compwts$wt)  #weights only
  table(compwts$trans) #trans only
  table(list(compwts$wt, compwts$trans)) #wts*trans

  # For a given weight, which transformation produces peak fits
  temp <- filter(allmod, wt=="lin")
  compwts <- filter(group_by(temp, species), or2.nag==max(or2.nag))
  table(compwts$trans)

# Compare different weights
 compwts <- rbind(best.exp, best.lin, best.seas.cur, best.seas.nocur)
  compwts.orig <- compwts
  compwts <- filter(allmod, trans=="sqrt")
  comp.l.bysp <- group_by(compwts, species)
  compare <- filter(comp.l.bysp, or2.nag==max(or2.nag))
  table(compare$wt)  #weights only
  table(compare$trans) #trans only
  table(list(compare$wt, compare$transformation)) #wts*trans

# Calculate mean r2.nag by different groups
#   compwts.temp <- filter(compwts, wt=="exp")
  compwts.temp <- allmod 
  compwts.list <- split(compwts.temp, f=list(compwts.temp$trans))
  sapply(X=compwts.list, function(x) mean(x$or2.nag))
  matrix(sapply(X=compwts.list, function(x) mean(x$or2.nag)), nrow=2)

#### SUCCESSIONAL TRAITS ####
## Compare 'lambda' with successional traits (use P/A, 50 occ for now)
  n.years.50 <- best.50[best.50$transformation=="log", c("species", "lambda")]
  n.years.20 <- best.20[best.20$transformation=="log", c("species", "lambda")]
  n.years <- rbind(n.years.50, n.years.20)
  lin.traits <- merge(succ.tr, n.years, by="species", all.x=FALSE, all.y=TRUE) #use succ.tr from ExploratoryAnalyses.R

  # Seasonal vs traits, use sqrt trans
  seas.traits <- filter(best.seas, transformation=="sqrt")
  seas.traits <- merge(seas.traits, succ.tr, by="species", all.x=TRUE, all.y=FALSE)
  # best fit lambdas vs traits, use filter by trans and wt
  seas.traits <- filter(best.lin.sqrt, threshold==8000)
  seas.traits <- merge(seas.traits, succ.tr, by="species", all.x=TRUE, all.y=FALSE)


ggplot(data=seas.traits) + geom_point(aes(x=succ.ind, y=as.numeric(lambda), color=rhizomes), size=3)
ggplot(data=seas.traits) + geom_point(aes(x=rhizomes, y=as.numeric(lambda), color=rhizomes), size=3) + 
  coord_cartesian(xlim=c(0,20))

###################################
## Comparing weights/transformations when outlier plots are excluded.
# exp log
  best.50.exp.log <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Exp_LogTran_Outliers_50occ.csv", sep=""))
  best.20.exp.log <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Exp_LogTran_Outliers_20occ.csv", sep=""))
  best.exp.log <- rbind(best.50.exp.log, best.20.exp.log)
  best.exp.log$wt    <- "exp"
  best.exp.log$trans <- "log"

# exp sqrt
  best.50.exp.sqrt <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Exp_SqrtTran_Outliers_50occ.csv", sep=""))
  best.20.exp.sqrt <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Exp_SqrtTran_Outliers_20occ.csv", sep=""))
  best.exp.sqrt <- rbind(best.50.exp.sqrt, best.20.exp.sqrt)
  best.exp.sqrt$wt    <- "exp"
  best.exp.sqrt$trans <- "sqrt"

# lin log
  best.50.lin.log <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Lin_LogTran_Outliers_50occ.csv", sep=""))
  best.20.lin.log <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Lin_LogTran_Outliers_20occ.csv", sep=""))
  best.lin.log <- rbind(best.50.lin.log, best.20.lin.log)
  best.lin.log$wt    <- "lin"
  best.lin.log$trans <- "log"

# lin sqrt
  best.50.lin.sqrt <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Lin_SqrtTran_Outliers_50occ.csv", sep=""))
  best.20.lin.sqrt <- read.csv(paste(DirOut, "OutlierDryPlots\\MFStats_PA_Dyn_Lin_SqrtTran_Outliers_20occ.csv", sep=""))
  best.lin.sqrt <- rbind(best.50.lin.sqrt, best.20.lin.sqrt)
  best.lin.sqrt$wt    <- "lin"
  best.lin.sqrt$trans <- "sqrt"

# all together 
  allmod <- rbind(best.exp.log, best.exp.sqrt,
                  best.lin.log, best.lin.sqrt)
  allmod <- filter(allmod, threshold==8000)  #use only outlier-excluded models
  colnames(allmod)
  allmod <- allmod[, c(1:3, 20:23)]