#### 1. TOTAL COVER ####

## 1b. Has there been a change in total cover over time?
  cover <- rawveg[, c("year", "plot", "elevation", "q.inund", "VEGETATIVE.COVER")]
  cover <- rename(cover, cover=VEGETATIVE.COVER)
  
  # Add a percentile rank column based on q.inund to gauge which plots are wettest/driest
    # First get a separte df with only one record per plot
    plot.qs <- filter(cover, year==1990)
    plot.qs <- mutate(plot.qs, q.rank=rank(q.inund), q.rank.pct=round(100*(rank(q.inund)/length(q.inund)), 0) ) 
    # Now merge back in with the main data
    cover <- merge(cover, plot.qs[, c("plot", "q.rank", "q.rank.pct")], by="plot", all=TRUE)
    rm(plot.qs)

  # Replace 'negative' cover values with NAs
    cover$cover[cover$cover<0] <- NA 
  
# Across the whole study area
  # As an average: (which is equivalent to sum of each plot coverage)
  by.yr <- group_by(cover, year)
  annual.means <- summarize(by.yr, mean.plotcover = mean(VEGETATIVE.COVER, na.rm=TRUE))
  
  # Plot distribution by year
  ggplot(data=cover) +
#     geom_point(aes(x=factor(year), y=cover), position=position_jitter(w=0.2)) +
    geom_boxplot(aes(x=factor(year), y=cover)) +
    ylab("Percent cover") +
    xlab("Sample year") +
    ggtitle("Average percent cover (across all plots) by sample year")
  
  # Anova
  summary(lm(cover~factor(year), data=cover))+
  anova(lm(cover~factor(year), data=cover))
  TukeyHSD(aov(cover~factor(year), data=cover))
  
  
  # Is there more of a trend in lower plots?
  ggplot(data=cover) +
    geom_point(aes(x=q.inund, y=cover)) + 
    facet_grid(. ~ year)  #this plot does show the general pattern of higher cover in 2006 and 2013.
  
# Individual plots
  # single plot
  ggplot(data=cover[cover$plot=="P105", ]) + 
    geom_point(aes(x=factor(year), y=cover))
  
  # first nine plots
  ggplot(data=cover[grep(pattern="P20", x=cover$plot), ]) + 
    geom_point(aes(x=factor(year), y=cover)) + 
    facet_wrap(~ plot)
  
  # Look for plots with a significant trend over time
  trends.cov <- matrix(data=NA, nrow=length(unique(cover$plot)), ncol=7)
  trends.cov <- as.data.frame(trends.cov)
  colnames(trends.cov) <- c("plot", "coef", "r2", "pval", "n.pos", "n.neg", "avg.change")
  
    # Fit a lm for each plot for the five yrs of data: looking for trends in pct cover
    for(i in 1:length(unique(cover$plot))) {
      plot.cur <- cover$plot[i]
      #     print(plot.cur, max.levels=0)
      
      lmfit.cur <- lm(cover~year, data=cover[cover$plot==plot.cur, ])
      pval.cur  <- summary(lmfit.cur)$coefficients[2,4]
            
      n.pos <- sum(diff(cover$cover[cover$plot==plot.cur]) > 0)
      n.neg <- sum(diff(cover$cover[cover$plot==plot.cur]) < 0)
      avg.change <- mean(diff(cover$cover[cover$plot==plot.cur]))
      
      out.cur <- c(as.character(plot.cur), 
                   lmfit.cur$coefficients[2], 
                   ((summary(lmfit.cur))$r.squared), 
                   pval.cur, 
                   n.pos, n.neg, avg.change)

      trends.cov[i, ] <- out.cur
    }

  # Convert to numeric
    trends.cov[, 2:6] <- as.numeric(unlist(trends.cov[, 2:6]))

  # How many plots have a positive linear trend?
    sum(trends.cov$coef>0)  #76 of 133, 57%
    sum(trends.cov$coef<0)  #56 of 133 (42%) have a negative trend (one plot has no trend)
  
  # How many plots have a 'significant' linear trend?
    sum(trends.cov$pval<0.1)  #1 of 133 plots, <1%...but of course it's hard to get "significance" with 5 data pts.
      hist(trends.cov$pval)
    sum(trends.cov$r2>0.5)     #23 of 133 plots have an r2 > 50%
      sum(trends.cov$r2>0.5 & trends.cov$coef>0)  #...of which 12 have a positive trend (and 14 have a neg trend)

  # Largest R2 values
    filter(trends.cov, r2>0.6) -> temp
    ggplot(data=cover[cover$plot %in% temp$plot, ]) + 
      geom_point(aes(x=year, y=cover)) + 
      geom_smooth(method=lm, aes(x=year, y=cover), linetype=2, se=FALSE, color="black", size=0.5) + 
      geom_text(aes(x=1994, y=15, label=paste("inund.q = ", round(q.inund, 0), sep=""))) + 
      geom_text(aes(x=1994, y=3,  label=paste("pct.rank = ", q.rank.pct, sep=""))) + 
      facet_wrap(~ plot)

  # Largest b1 coefficient (pos or neg)
    arrange(trends.cov, abs(coef)) -> test
    tail(test$plot, 9) -> str.plots
    ggplot(data=cover[cover$plot %in% str.plots, ]) + 
      geom_point(aes(x=year, y=cover)) + 
#       geom_smooth(method=lm, aes(x=year, y=cover), linetype=2, se=FALSE) + 
      facet_wrap(~ plot)
    
  # What about a monotonic trend, rather than just linear
    # How many plots have only positive (or only negative) changes in covertype?
    sum(trends.cov$n.pos==0)  #none
    sum(trends.cov$n.neg==0)  #none
  
    # How about just one negative (or just one positive) change
    sum(trends.cov$n.neg==1)  #43 of 133 plots
    sum(trends.cov$n.pos==1)  #42 of 133 plots
    
    sum(trends.cov$n.pos==2)  #73 of 133 plots
      #...so, very evenly distributed, definitely no evidence for an overall increase.

  # Again, look for associations with elevation/inundur
    # Merge in plot data
      trends.cov <- merge(trends.cov, cover[cover$year==1990, c("plot", "elevation", "q.inund")])
      # Index describing whether trends (such as it may be) is pos or neg
      trends.cov$trend.dir <- trends.cov$coef>0
    # Strength of trend (R2) vs q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=r2)) + geom_point()
      ggplot(data=trends.cov, aes(x=elevation, y=r2)) + geom_point()
    # Direction and strength of trend vs q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=coef)) + geom_point()
      ggplot(data=trends.cov, aes(x=elevation, y=coef)) + geom_point()
    # Direction of trend vs q.inund, for plots with high r2
      ggplot(data=trends.cov[trends.cov$r2>=0.5, ], aes(x=q.inund, y=coef)) + 
        geom_point(aes(fill=r2), size=3, shape=21) + 
        scale_fill_gradientn(colors=rainbow(4)) +
        ylab("Slope coefficient of plot total cover vs. year")
      ggplot(data=trends.cov[trends.cov$r2>=0.5, ], aes(x=trend.dir, y=q.inund)) + 
        geom_point(aes(fill=abs(coef)), size=3, shape=21) + 
        scale_fill_gradientn(colors=rainbow(4), name="abs(Slope)") + 
        scale_x_discrete(labels=c("Negative", "Positive")) +
        xlab("Direction of plot trend in total cover")
    # Num pos/neg changes in cover as a function of q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=n.pos)) + geom_point()
      ggplot(data=trends.cov, aes(x=factor(n.pos), y=q.inund)) + geom_point()
    # Avg pct change (n=4 changes per plot) vs q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=as.numeric(avg.change))) + geom_point() + 
        coord_cartesian(xlim=c(0, 6000))

# Look at changes in pct cover year to year
changes.cov <- as.data.frame(matrix(data=NA, nrow=length(unique(cover$plot)), ncol=5))
colnames(changes.cov) <- c("plot", "d1994", "d2001", "d2006", "d2013")
  
    # Fit a lm for each plot for the five yrs of data: looking for trends in pct cover
    for(i in 1:length(unique(cover$plot))) {
      plot.cur <- cover$plot[i]
      
      diffs.cur <- diff(cover$cover[cover$plot==plot.cur])
      
      changes.cov[i, 2:5] <- diffs.cur
      changes.cov[i, 1] <- as.character(plot.cur)
     }

  # Create a long form df with q.inund
  changes.cov <- merge(changes.cov, rawveg[rawveg$year==1990, c("plot", "q.inund")],
                       by="plot", all=TRUE)
  changes.l <- melt(data=changes.cov, id.vars=c("plot", "q.inund"), variable.name="year", value.name="cover.change")
  
  # Now plot CHANGE vs q.inund, year by year
  ggplot(data=changes.l) +
    geom_point(aes(x=q.inund, y=cover.change)) + 
    geom_hline(aes(yintercept=0), linetype="dashed") + 
    facet_wrap(~year) + 
    ggtitle("CHANGES in percent cover vs inundating discharge")

  # What if we just look at actual pct cover rather than change in pct cover?
  ggplot(data=cover[cover$q.inund<6000, ]) +
    geom_point(aes(x=q.inund, y=cover)) + 
    #   geom_hline(aes(yintercept=0), linetype="dashed") + 
    facet_wrap(~year) + 
    ggtitle("Percent cover vs inundating discharge")

  # Need to assess flow history: use \raw.flowdata\ from <Exp_InundationDurations.R>
  recent.flows <- filter(raw.flowdata, wyear>=1986)
  months <- as.numeric(substr(recent.flows$date, 6, 7))
  recent.flows <- cbind(recent.flows, months)
  recent.flows$wyear[recent.flows$months %in% c(7,8,9)] <- 1 + recent.flows$wyear[recent.flows$months %in% c(7,8,9)]

  by.yr <- group_by(recent.flows, wyear)
  test <- summarize(by.yr,
            total.q = sum(discharge),
            peak.q = max(discharge))
  test$survey <- FALSE
  test$survey[test$wyear %in% c(1990, 1994, 2001, 2006, 2013)] <- TRUE

    # Quickly - calculate mean total annual flow for the four yrs preceeding each survey
    for (i in c(1990, 1994, 2001, 2006, 2013)) {
      dat.cur <- filter(test, wyear<=i & wyear>=(i-3))
      avg.cur <- mean(dat.cur$total.q)
      print(paste(min(dat.cur$wyear), max(dat.cur$wyear), "4 yr maf =", avg.cur))
    }
 ggplot(data=test) +
  geom_line(aes(x=wyear, y=peak.q)) +
  geom_point(aes(x=wyear, y=peak.q, color=survey), size=3) +
  ggtitle("Peak discharge during the 12 months preceding July survey")

# How many plots have a positive/negative change in cover 1994->2000 and 2006->2013?
  yr23 <- filter(cover, year %in% c(2006, 2013))
  yr23$year[yr23$year==2006] <- "y2006"
  yr23$year[yr23$year==2013] <- "y2013"
  yr23.w <- dcast(data=yr23, plot + elevation + q.inund ~ year, value.var="cover")
  yr23.w <- mutate(yr23.w, d2013 = y2013-y2006)
  sum(yr23.w$d2013>0)
  sum(yr23.w$d2013==0)
  sum(yr23.w$d2013<0)


ggplot(data=cover[cover$plot=="P324", ]) + 
  geom_point(aes(x=year, y=cover)) + 
  geom_smooth(method=lm, aes(x=year, y=cover), linetype=2, se=FALSE, color="black", size=0.5) + 
  geom_text(aes(x=1994, y=75, label=paste("inund.q = ", round(q.inund, 0), sep=""))) + 
  geom_text(aes(x=1994, y=63,  label=paste("pct.rank = ", q.rank.pct, sep=""))) + 
  facet_wrap(~ plot)

#### 2. SPECIES FREQUENCY ####

# Calculate frequency of occurrence for each species in each year
  occ.w <- aggregate(veg[, 3:109], by=list(veg$year), FUN=sum)  #all occ counts are out of 133 poss plots
  occ.w <- rename(occ.w, year=Group.1)

  occ   <- melt(occ.w, id.vars="year", variable.name="species", value.name="n.occ")


# When we get x,y data, do some maps!


# Graph occ vs time for selected spp
  # One sp
  ggplot(data=occ[occ$species=="CARSPE", ]) + 
    geom_point(aes(x=factor(year), y=n.occ))
  
  # First 9 spp
  temp <- filter(occ, species %in% common.spp$species[10:18])
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species)


# Look for linear/monotonic trends
  # Empty df to be filled
  trends.occ <- matrix(data=NA, nrow=length(unique(occ$species)), ncol=6)
  trends.occ <- as.data.frame(trends.occ)
  colnames(trends.occ) <- c("species", "coef", "r2", "pval", "n.pos", "n.neg")
  
  # Fit a lm for each plot for the five yrs of data: looking for trends in pct cover
    for(i in 1:length(unique(occ$species))) {
      sp.cur <- unique(occ$species)[i]
      #     print(plot.cur, max.levels=0)
      
      lmfit.cur <- lm(n.occ~year, data=occ[occ$species==sp.cur, ])
      pval.cur  <- round(summary(lmfit.cur)$coefficients[2,4], 3)
            
      n.pos <- sum(diff(occ$n.occ[occ$species==sp.cur]) > 0)
      n.neg <- sum(diff(occ$n.occ[occ$species==sp.cur]) < 0)
      
      out.cur <- c(as.character(sp.cur), 
                   round(lmfit.cur$coefficients[2], 3), 
                   round(((summary(lmfit.cur))$r.squared), 3), 
                   pval.cur, 
                   n.pos, n.neg)

      trends.occ[i, ] <- out.cur
    }


  # Convert to numeric
    trends.occ[, 2:6] <- as.numeric(unlist(trends.occ[, 2:6]))
    trends.occ <- mutate(trends.occ, n.zero=(5-n.pos-n.neg))

  # How many sp have a positive trend? (according to lm)
    sum(trends.occ$coef>0)  #49 of 107 (46%) of species increase in frequency
    sum(trends.occ$coef<0)  #58 of 107 (54%) of species decrease in frequency
  
  # How many species have a 'significant' linear trend?
    # P value
    sum(trends.occ$pval<0.05)  #11 of 107 plots, ~10%...but of course it's hard to get "significance" with 5 data pts.
      hist(trends.occ$pval)
      
      filter(trends.occ, pval<0.05)->sig.spp
      ggplot(data=occ[occ$species %in% sig.spp$species, ]) + 
        geom_point(aes(x=year, y=n.occ)) + 
        geom_smooth(method=lm, aes(x=year, y=n.occ), linetype=2, se=FALSE) + 
        facet_wrap(~ species) +
        ggtitle("Spp with lm pval < 0.05")
    
    # R squared
    hist(trends.occ$r2)
    sum (trends.occ$r2>0.75)   #39 of 107 spp have an r2 > 50%; 14 are > 0.75.

    filter(trends.occ, r2>0.75) -> r2.spp
    ggplot(data=occ[occ$species %in% r2.spp$species, ]) + 
      geom_point(aes(x=year, y=n.occ)) + 
      geom_smooth(method=lm, aes(x=year, y=n.occ), linetype=2, se=FALSE) + 
      facet_wrap(~ species) + 
      ggtitle("Spp with highest lm R2")
  
    # Largest b1 coefficient (pos or neg)
    hist(trends.occ$coef)
    arrange(trends.occ, abs(coef)) -> test
    tail(test$species, 9) -> str.species
    ggplot(data=occ[occ$species %in% str.species, ]) + 
      geom_point(aes(x=year, y=n.occ)) + 
#       geom_smooth(method=lm, aes(x=year, y=n.occ), linetype=2, se=FALSE) + 
      facet_wrap(~ species)
    
  # What about a monotonic trend, rather than just linear
    # How many plots have only positive (or only negative) changes in covertype?
    sum(trends.occ$n.pos==0)  #16 spp (with which, n.neg=3, 2, or 1)
    sum(trends.occ$n.neg==0)  # 9 spp
    # Rearrange df, most pos change to most neg change
    trends.occ <- arrange(trends.occ, n.pos, -n.neg)
      # But, many of these spp have very few occurrences:
      by.spp <- group_by(occ, species)
      tot.occ <- summarize(by.spp, freq=sum(n.occ, na.rm=TRUE))
      trends.occ <- merge(trends.occ, tot.occ, by="species", all=TRUE)

  # Plot the spp with most inc/dec
  temp <- filter(occ, species %in% trends.occ$species[trends.occ$n.neg==3 & trends.occ$freq>=50])
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species) +
    ggtitle("3 decreases and >= 50 occ")

## Overall: somewhat subjectively chosen trend spp
##  (limit to at least 20 occ)
  inc.spp <- c("APOCAN", "ASCSPE", "ELYREP", "HIPHYE", "PHAARU", "SALEXI")
  dec.spp <- c("AGRGIG", "CONCAN", "EPICIL", "EUTOCC", "MUHRAC", "NEGACE", "VERBRA")

  temp <- filter(occ, species %in% dec.spp)
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species) +
    ggtitle("Decreasing trend")

  # What about traits for these spp?
  filter(succ.tr, species%in% dec.spp)
  

    # How about just one negative (or just one positive) change
    sum(trends.occ$n.neg==1)  #45 of 107 spp (these categories are not mutually exclusive)
    sum(trends.occ$n.pos==1)  #49 of 107 spp
    
    sum(trends.occ$n.pos==2)  #30 of 107 spp
    sum(trends.occ$n.neg==2)  #33 of 107 spp

#### 2B. UPDATE BASED ON JF CMTS FROM 2016-01-30 ####
## Get an objective/standardized list of increasers and decreasers
# Combine useful data in one place
  # Merge in n.occ data, and all traits data
    trends.occ <- merge(trends.occ, traits, by="species", all=TRUE)
  # and succ.ind info  #succ.tr is from next section of script
    trends.occ <- merge(trends.occ, succ.tr[, c("species", "succ.ind")], by="species", all.x=TRUE)
  # And wetness index data (from later in script)
    trends.occ <- merge(trends.occ, sp.ind[, c(1, 4)], by="species", all=TRUE)
  # Rename n.occ column to avoid later confusion with yearly counts
    trends.occ <- rename(trends.occ, n.occ.total=n.occ)

# First criterion: at least 20 total occ
  common.spp <- traits$species[traits$n.occ>=20]  #51 spp

# Second criterion: trend over time is statistically significant
  sum(trends.occ$pval<=0.05)  #only 11 of 107 spp
  sum(trends.occ$pval<=0.10)  #brings it up to 23 spp
  sig.spp <- trends.occ$species[trends.occ$pval<=0.1]

# Third criterion: slope above some threshold
  # Distribution of slope values across all spp
  summary(abs(trends.occ$coef))
  ggplot(data=trends.occ, aes(x=1, y=abs(coef))) + geom_boxplot()
  # Or across only the 23 'significant' spp
  summary(abs(trends.occ$coef[trends.occ$pval<=0.1]))
  ggplot(data=trends.occ[trends.occ$pval<=0.1, ], aes(x=1, y=abs(coef))) + geom_point()
  
# Combine the first two criteria
  # How many of the 'sig' spp also have at least 20 occ? 
    trends.sig <- filter(trends.occ, pval<=0.1 & n.occ.total>=20)  #14 spp
    #   trends.extrasig <- filter(trends.sig, pval<=0.05) #7 spp, so half of the 14 are highly sig.
    trends.sig <- arrange(trends.sig, desc(coef))  #arrange in order of slope value

# Add in third criterion?
# --> All of the 14 spp have coefficients exceeding 0.5, with the exception of STAPAL. 
#     I am going to retain this sp for now - so no slope criterion for now. 

  # Add trends data to occ data for plotting
    occ.withtr <- merge(occ, trends.occ, by="species", all=TRUE)

# By these criteria, the increasing species:
  trends.sig.pos <- filter(trends.sig, coef>0)  #n=9
    # look at relevant columns only
    trends.sig.pos[, c(1:4, 8, 10:11, 13:15)]
  inc.spp <- trends.sig.pos$species
  median(trends.sig.pos$succ.ind)
  # Plots for each increasing species 
  ggplot(data=occ.withtr[(occ.withtr$species %in% trends.sig.pos$species), ]) +
    geom_point(aes(x=year, y=n.occ)) + 
      geom_text(aes(x=1994, y=90, label=paste("coef=", coef, sep=""))) + 
      geom_text(aes(x=1997, y=80, label=paste("r2=", r2, "; p=", pval, sep=""))) + 
    facet_wrap(~species) 

# The decreasing spp:
  trends.sig.neg <- filter(trends.sig, coef<0)  #n=5
    # look at relevant columns only
    trends.sig.neg[, c(1:4, 8, 10:11, 13:15)]
  dec.spp <- trends.sig.neg$species
  mean(trends.sig.neg$succ.ind)
  # Plots for each increasing species 
  ggplot(data=occ.withtr[(occ.withtr$species %in% trends.sig.neg$species), ]) +
    geom_point(aes(x=year, y=n.occ)) + 
      geom_text(aes(x=1994, y=90, label=paste("coef=", coef, sep=""))) + 
      geom_text(aes(x=1997, y=80, label=paste("r2=", r2, "; p=", pval, sep=""))) + 
    facet_wrap(~species) 

# Species that were previously (subjectively) identified as decreasing,
# but don't appear in the new objective list:
  dec.spp.lost <- c("CONCAN", "MUHRAC", "EUTOCC")

  ggplot(data=occ.withtr[(occ.withtr$species %in% dec.spp.lost), ]) +
    geom_point(aes(x=year, y=n.occ)) + 
    geom_text(aes(x=2006, y=60, label=paste("coef=", coef, sep=""))) + 
    geom_text(aes(x=2006, y=50, label=paste("r2=", r2, "; p=", pval, sep=""))) + 
    facet_wrap(~species) 
  
  filter(trends.occ, species %in% dec.spp.lost)


## Average successional index for a plot sample ##
# Long form veg data
  veg.long <- read.csv("WP_VegData_withCovertypeInundur_long.csv")
  # Remove unneeded columns
  veg.long <- select(veg.long, -c(dur.class, covertype, num.spp.rip))
  # Add succ.ind data
  veg.long <- merge(veg.long, succ.tr[, c("species", "succ.ind")], by="species", all=TRUE)
  # Reorder
  veg.long <- arrange(veg.long, year, plot, species)

# Calculate mean succ.ind for a plot sample
  by.plotsmp <- group_by(veg.long, year, plot, elevation, q.inund, inundur)
  plotsmp.dat <- summarize(by.plotsmp,
                           num.spp = max(num.spp.all),
                           mean.succ.ind = mean(succ.ind, na.rm=TRUE)) #ignore spp with no index, still, calculate a value for that plot smp
  rm(by.plotsmp)

# Succ.ind as a func of inundur for each year
  # Which inundur to use? Linear or exp weighting? Lambda?
  # Read in the inundur data
    inundurs.exp <- read.csv("Inundurations_lambdas_allyrs_all.csv")
    inundurs.lin <- read.csv("Inundurations_linear_allyrs.csv")
  # Combine plot veg data and inunduration data
    # 50 yr inundur with no weighting; weak exp weighting (1e-4); and strong exp weighting (1e-1.5)
    veg.inund <- merge(plotsmp.dat, inundurs.exp[, c(1,2,5, 16, 41)], 
                       by=c("year", "plot"), all=TRUE)  
    colnames(veg.inund)[8:10] <- c("no.wgt", "exp.e4", "exp.e1.5")
    # Weak linear weighting (30 yr; this was most popular); and strong linear (5 yr)
    veg.inund <- merge(veg.inund, inundurs.lin[, c("year", "plot", "inundur.30", "inundur.5")],
                  by=c("year", "plot"), all=TRUE)
    veg.inund <- rename(veg.inund, lin.30 = inundur.30, lin.5 = inundur.5)

  # Now a series of plots, one for each inundur column
    for (i in (c(5, 8:12))) {
      plot.cur <- ggplot(data=veg.inund, aes(x=veg.inund[, i], y=mean.succ.ind)) + 
        geom_point(aes(fill=num.spp), shape=21, size=3, color="black") + 
        scale_fill_gradientn(colors=rainbow(7)) + 
        ggtitle(colnames(veg.inund)[i]) + 
        facet_wrap(~ year)
      print(plot.cur)
    }  

  # What are all those red/orange plots with so few spp, esp in 2001?
  onesp.plots <- filter(veg.long, num.spp.all==1) #12 plotsmp
  twosp.plots <- filter(veg.long, num.spp.all==2) #19 plotsmp

# Succ ind. vs plot cover
  # Bring in cover data (\cover\ df created in part A)
  veg.inund <- merge(veg.inund, cover[, c("year", "plot", "cover")],
                     by=c("year", "plot"), all=TRUE)  
  veg.inund$cover[veg.inund$cover<0] <- NA
  
  # And plot it
  ggplot(data=veg.inund, aes(x=cover, y=mean.succ.ind)) + 
    geom_point(aes(fill=num.spp), shape=21, size=3, color="black") + 
    scale_fill_gradientn(colors=rainbow(7)) + 
    ggtitle("Successional index vs Cover (color=spp richness)") + 
    facet_wrap(~ year)

  # The 3rd plot: cover ~ inundur
  ggplot(data=veg.inund, aes(y=cover, x=exp.e4)) + 
    geom_point(aes(fill=num.spp), shape=21, size=3, color="black") + 
    scale_fill_gradientn(colors=rainbow(7)) + 
    ggtitle("Cover vs Inundur (color=spp richness)") + 
    facet_wrap(~ year)
  
  # Explore the low-cover/high-ind outlier plots
  out.pl <- filter(veg.inund, year>1990, cover<15 & num.spp <=7 & mean.succ.ind>4)
  out.spp<- merge(veg.long, out.pl[, c(1,2,5,7,13)],
                  b7=c("year", "plot", "inundur"), all=FALSE)
  sum(out.pl$inundur %in% wet.out$inundur)  

# Succ ind vs spp richness
  ggplot(data=veg.inund, aes(x=num.spp, y=mean.succ.ind)) + 
    geom_point((aes(fill=exp.e4)), shape=21, size=3, color="black") +  #aes(fill=num.spp), 
    scale_fill_gradientn(colors=rainbow(7), name="Inundur(exp.1e-4)") +   #colors=rainbow(7)
    ggtitle("Successional index vs Species richness (color=inundur)") + 
    facet_wrap(~ year)

  # Investigate the uppper-right blue pts
  wet.out <- filter(veg.inund, year>1990 & num.spp<=5 & exp.e4>0.5 & num.spp>0) 
# And, though it's been done before, spp.richness vs inundur (exp.weak)
  ggplot(data=veg.inund, aes(x=exp.e4, y=num.spp)) + 
    geom_point((aes(fill=mean.succ.ind)), shape=21, size=3, color="black") +  #aes(fill=num.spp), 
    scale_fill_gradientn(colors=rainbow(7), name="Mean.succ.ind") + 
    ggtitle("Spp richness vs inundation duration (color=mean succ.ind)") + 
    facet_wrap(~ year)

# Distribution of succ.ind (plot sample mean) in each year
  ggplot(data=veg.inund, aes(x=factor(year), y=mean.succ.ind)) + 
  geom_boxplot()


## Observed wetness index for each species: which end of the inundur gradient 
## does the spp tend to occur at, in our dataset?
## --> a spp wetness index
  # Read in the inundur data [again]
    inundurs.exp <- read.csv("Inundurations_lambdas_allyrs_all.csv")
  # Aribtrarily pick a lm to use: 1e-4, col 16
    veg.long <- merge(veg.long, inundurs.exp[, c(1,2,16)],
                      by=c("year", "plot"), all=TRUE)
    veg.long <- rename(veg.long, inundur.exp=inundur.0.0001, inundur.17yr=inundur)
  # Calculate mean inundur across all occurrences, by species
    by.sp <- group_by(veg.long, species)
    sp.ind<- summarize(by.sp, 
                       mean.exp = mean(inundur.exp),
                       mean.17yr= mean(inundur.17yr))
  # Reformulate as a percentile
    # First remove 'none'
    sp.ind <- sp.ind[sp.ind$species!="NONE", ]
    sp.ind <- mutate(sp.ind, wet.ind=round(100*(rank(mean.exp)/length(mean.exp)), 0) )
    # ! in this case, wetter species have a higher index - so 100 is for the wettest sp, 1 for the driest. 


## Tendency for a given sp to be lost from a plot in which it has occurred
# Loss/gain matrices
  veg.loss <- read.csv("vegdata_loss.csv")
  veg.gain <- read.csv("vegdata_gains.csv")

# Losses
  # Convert to long form
  veg.loss.l <- melt(data=veg.loss, 
                     id.vars=c("yr.2", "plot"), 
                     variable.name="species", value.name="change")

  # Count losses and remains by sp
  by.sp <- group_by(veg.loss.l, species)
  loss.sum <- summarize(by.sp,
                        n.loss = sum(change==-1, na.rm=TRUE),
                        n.stay = sum(change== 0, na.rm=TRUE),
                        n.poss = sum(!is.na(change)), 
                        pct.loss = n.loss/n.poss)
  # Merge in succ.ind for these sp
  loss.sum <- merge(loss.sum, succ.tr[, c(1,2,4,5,8)], by="species", all=TRUE)
  # is pct loss related to succ.ind?
  ggplot(data=loss.sum[loss.sum$n.poss>=20, ], aes(x=succ.ind, y=pct.loss)) +
    geom_point(aes(color=n.poss), size=3)

# Gains
  # Convert to long form
  veg.gain.l <- melt(data=veg.gain, 
                     id.vars=c("yr.2", "plot"), 
                     variable.name="species", value.name="change")

  # Count losses and remains by sp
  by.sp <- group_by(veg.gain.l, species)
  gain.sum <- summarize(by.sp,
                        n.gain = sum(change==-1, na.rm=TRUE),
                        n.stay = sum(is.na(change)),  #num. of plots in which sp was present before
                        pct.gain = n.gain/(n.gain + n.stay)) #what prop of sp occurrences were gains?
  # Merge in succ.ind for these sp
  gain.sum <- merge(gain.sum, succ.tr[, c(1,2,4,5,8)], by="species", all=TRUE)
  # is pct gain related to succ.ind?
  ggplot(data=gain.sum, aes(x=succ.ind, y=pct.gain)) +
    geom_point()

#### 3. SUCCESSIONAL TRENDS ####
# Read in trait data
  traits <- read.csv("BLCA_WP_traitdata.csv")

  # But, many of these spp have very few occurrences:
  by.spp <- group_by(occ, species)
  tot.occ <- summarize(by.spp, freq=sum(n.occ, na.rm=TRUE))
  # Merge n.occ with traits
  traits <- merge(traits, tot.occ, by="species", all=TRUE)


# Which are the most late-successional spp?
  #   Perennial, rhizomatous, tall [JF suggests >8dm vs <8 dm]
  late.spp  <- filter(traits, duration=="P" & rhizomes=="Yes" & height>=8)
# vs. the most early-successional:
  early.spp <- filter(traits, duration=="A" & rhizomes=="No" & height<8)

  # Plot the late spp (with enough occurrences)
  temp <- filter(occ, species %in% late.spp$species[late.spp$freq>11])
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species) +
  ggtitle("Late-successional species")

  # Now the early spp (not many of them)
  temp <- filter(occ, species %in% early.spp$species[early.spp$freq>6])
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species) +
  ggtitle("Early-successional species")

# Create an index of successionality (hackishly)
  # for this, let's eliminate all rows with NAs
  succ.tr <- traits[(!is.na(traits$duration) & !is.na(traits$rhizomes) & !is.na(traits$height)), ]

  succ.tr$succ.ind <- 0
  succ.tr$succ.ind[succ.tr$duration!="P" & succ.tr$duration!="A"] <- 1  #1 succession pt for being between A and P
  succ.tr$succ.ind[succ.tr$duration=="P"] <- 2                          #2 succession pts for being fully perenn
  succ.tr$succ.ind[succ.tr$rhizomes=="Yes"] <- 2 + succ.tr$succ.ind[succ.tr$rhizomes=="Yes"]    #2 succ pts for being rhizomatous
  # height: [0, 5) --> 0 pts
  #         [5, 8) --> 1 pt
  #         [8, 12] -> 2 pts
  #         (12, ...)-> 3 pts
  succ.tr$succ.ind[succ.tr$height>=5 & succ.tr$height<8]   <- 1 + succ.tr$succ.ind[succ.tr$height>=5 & succ.tr$height<8]
  succ.tr$succ.ind[succ.tr$height>=8] <- 2 + succ.tr$succ.ind[succ.tr$height>=8]  #& succ.tr$height<=12
#   succ.tr$succ.ind[succ.tr$height>12] <- 3 + succ.tr$succ.ind[succ.tr$height>12]

  # Look at data for trend spp:
  filter(succ.tr[, c(1:5, 7:8)], species %in% dec.spp)

  # Now plot some spp
  temp <- filter(occ, species %in% succ.tr$species[succ.tr$succ.ind>=6 & succ.tr$freq>=20])
  ggplot(data=temp) + 
    geom_point(aes(x=factor(year), y=n.occ)) + 
    facet_wrap(~ species) +
  ggtitle("Successional index: 6 or 7")

  # Plot spp trend metric vs successionality
  trends.occ <- merge(trends.occ, succ.tr[, c("species", "succ.ind")], 
                      by="species", all=TRUE)
  trends.occ <- merge(trends.occ, tot.occ, by="species", all=TRUE)
  ggplot(data=trends.occ[trends.occ$freq>=20, ]) +
    geom_point(aes(x=succ.ind, y=coef)) + 
  ggtitle("Linear slope vs successional index (at least 20 occ)")

#### PLOT-SPECIFIC SPECIES CHANGES ####
gains.data <- read.csv("vegdata_gains.csv")
gains.byyr <- aggregate(gains.data[, 3:109], by=list(gains.data$yr.2), FUN=sum, na.rm=TRUE)
colnames(gains.byyr)[1] <- "yr.2"

  # Convert to long form
  gains.l <- melt(gains.byyr, id.vars="yr.2", variable.name="species", value.name="n.gains")

  # Combine with occ counts
  gains <- merge(gains.l, occ, by.x=c("yr.2", "species"), by.y=c("year", "species"), all.x=TRUE, all.y=FALSE)
  gains <- mutate(gains, pct.change = n.gains/n.occ)

  # wide form
  changes <- dcast(gains[, c(1,2,5)], species ~ yr.2, value.var="pct.change")
    # Total n.occ of each sp
      vegtot <- as.data.frame(apply(X=veg[, 3:109], MARGIN=2, FUN=sum))
      vegtot$species <- rownames(vegtot)
      colnames(vegtot)[1] <- "n.occ"
      rownames(vegtot) <- NULL
  changes <- merge(vegtot, changes, by="species", all=TRUE)

  changes.common <- filter(changes, n.occ > 10)
# *** COME BACK TO THIS ***#

#### PHALARIS TRENDS ####
# Sum total n.spp present in each plot sample  
# Be careful!  If a num.spp column has already been added, resulting sums will be 2X appropriate size.
# Probably better to merge in a more reliable count, rather than to recalculate it.  
  n.spp <- apply(X=veg[, 3:109], MARGIN=1, FUN=sum)
  veg <- cbind(veg, n.spp)
  veg <- veg[ c(1:2, 110, 3:109)]
  rm(n.spp)

# Add [correct] n.spp to rawveg
  rawveg <- merge(rawveg, plotsmp.dat[, c("year", "plot", "num.spp")],
                       by=c("year", "plot"), all=TRUE)
  
# Select only PHAARU plots
  pha.smp <- filter(veg, PHAARU==1)
  table(pha.smp$year)   #should correspond to trend in PHAARU frequency of occ

# How many plots have PHAARU present in all years?
  by.plot <- group_by(veg, plot)
#   by.plot.3 <- filter(by.plot, year>2000)
  nyr.ph  <- summarize(by.plot, 
                       n.yr = sum(PHAARU))

  sum(nyr.ph$n.yr==5) #34 plots; meaning that 17 plots with PHAARU present in 1990 must have lost it for at least one yr.  
  fiveyr.plots <- nyr.ph$plot[nyr.ph$n.yr==5]
  threeyr.plots <- nyr.ph$plot[nyr.ph$n.yr==3]

  pha.allyr <- filter(pha.smp, plot %in% fiveyr.plots)
  # Mean num.spp in each year (within the 34 always-PHAARU plots)
    tapply(pha.allyr$n.spp, pha.allyr$year, mean)

# Plot of n.spp dist over time
  ggplot(data=pha.smp[pha.smp$plot %in% fiveyr.plots, ]) +
      geom_boxplot(aes(x=factor(year), y=n.spp)) + 
#     geom_point(aes(x=factor(year), y=n.spp)) + 
    ggtitle("Number of spp in plots in which PHAARU was always present (n=34)")

# Vs. (control) plot of n.spp dist over time, ALL PLOTS
  ggplot(data=veg[veg$PHAARU==0, ]) +
      geom_boxplot(aes(x=factor(year), y=n.spp)) + 
#     geom_point(aes(x=factor(year), y=n.spp)) + 
    ggtitle("Number of spp across non-PHAARU plots")

# Does PHAARU tend to appear at certain elevations? (inundating discharges)
  ggplot(data=rawveg) + 
    geom_point(aes(x=q.inund, y=PHAARU))

  # Year-by-year
  ggplot(data=rawveg) + 
    geom_point(aes(x=q.inund, y=factor(PHAARU))) +
  facet_wrap(~year)

# Overall relationship between n.spp and q.inund
  rawveg <- cbind(n.spp, rawveg)

ggplot(data=rawveg) +
  geom_point(aes(x=q.inund, y=num.spp, color=factor(PHAARU))) +
  facet_wrap(~year) +
  ggtitle("Number of spp present vs. plot inundating discharge: 
          plots with and without PHAARU")

# Average n.spp  by year
  tapply(X=pha.smp$n.spp, INDEX=pha.smp$year, FUN=mean)
  aov(n.spp~factor(year), data=pha.smp)->pha.aov
  summary(pha.aov)
  TukeyHSD(pha.aov)

ggplot(data=veg.inund, aes(x=cover, y=num.spp)) + 
  geom_point(aes(fill=exp.e4), shape=21, size=3) +
  scale_fill_gradientn(colors=rainbow(7)) +
  facet_wrap(~year)
scale_fill_gradientn(colors=rainbow(7), name="Mean.succ.ind") + 
  

#### SPP RICHNESS, Q.INUND, PCT COVER ####
plotcov <- select(rawveg, year, plot, q.inund, num.spp.all, VEGETATIVE.COVER)   
plotcov <- rename(plotcov, n.spp=num.spp.all, cover=VEGETATIVE.COVER)

library(plot3D)
points3D(x=plotcov$n.spp, y=plotcov$cover, z=plotcov$q.inund)
# surf3D(x=plotcov$n.spp, y=plotcov$cover, z=plotcov$q.inund) #doesn't work

#q.inund:n.spp
ggplot(data=plotcov) +
  geom_point(aes(x=q.inund, y=n.spp)) +
  facet_wrap(~year)

#q.inund:cover
ggplot(data=plotcov) +
  geom_point(aes(x=q.inund, y=cover)) +
  facet_wrap(~year)

#n.spp:cover
ggplot(data=plotcov) +
  geom_point(aes(x=n.spp, y=cover)) +
  facet_wrap(~year)

ggplot(data=rawveg) + 
  geom_point(aes(y=num.spp.all, x=VEGETATIVE.COVER, color=as.factor(PHAARU))) + 
  facet_wrap(~year)

#### TRAITS AND EXPONENTIAL DECAY ####
# Successional traits vs selected lambda
  lms <- select(bestmodels, species, lambda, n.occ)
  bestmodels.2 <- read.csv(paste(DirOut, "MFStats_PA_Dyn_Exp_SmLm_50occ.csv", sep=""))
  lms2 <- select(bestmodels.2, species, lambda, n.occ)
  lms <- rbind(lms, lms2)
  rm(lms2)

  mod.spp <- merge(succ.tr, lms, by="species", all=FALSE)
  mod.spp <- mutate(mod.spp, lm.exp=log10(as.numeric(lambda)))

ggplot(data=mod.spp, aes(x=lm.exp, y=annual.tend)) + geom_point()

lglmspp <- c("ARTLUD", "FESARU", "LACSER", "OENVIL", "SOLVEL", "TRADUB")
zerolmspp <- c("AGRGIG", "CARLAN", "CARNEB", "ELEMAC", "EPICIL", "EQUARV", "NEGACE", "CHASER")
filter(mod.spp, species %in% zerolmspp)
