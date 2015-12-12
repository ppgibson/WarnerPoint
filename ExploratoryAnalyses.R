#### 1. TOTAL COVER ####

## 1b. Has there been a change in total cover over time?
  cover <- rawveg[, c("year", "plot", "elevation", "q.inund", "VEGETATIVE.COVER")]
  cover <- rename(cover, cover=VEGETATIVE.COVER)
  
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
    sum(trends.cov$pval<0.05)  #1 of 133 plots, <1%...but of course it's hard to get "significance" with 5 data pts.
      hist(trends.cov$pval)
    sum(trends.cov$r2>0.5)     #23 of 133 plots have an r2 > 50%
      sum(trends.cov$r2>0.5 & trends.cov$coef>0)  #...of which 12 have a positive trend (and 14 have a neg trend)

  # Largest R2 values  
    filter(trends.cov, r2>0.6) -> temp
    ggplot(data=cover[cover$plot %in% temp$plot, ]) + 
      geom_point(aes(x=year, y=cover)) + 
      geom_smooth(method=lm, aes(x=year, y=cover), linetype=2, se=FALSE) + 
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
    # Strength of trend (R2) vs q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=r2)) + geom_point()
      ggplot(data=trends.cov, aes(x=elevation, y=r2)) + geom_point()
    # Direction and strength of trend vs q.inund
      ggplot(data=trends.cov, aes(x=q.inund, y=coef)) + geom_point()
      ggplot(data=trends.cov, aes(x=elevation, y=coef)) + geom_point()
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
      pval.cur  <- summary(lmfit.cur)$coefficients[2,4]
            
      n.pos <- sum(diff(occ$n.occ[occ$species==sp.cur]) > 0)
      n.neg <- sum(diff(occ$n.occ[occ$species==sp.cur]) < 0)
      
      out.cur <- c(as.character(sp.cur), 
                   lmfit.cur$coefficients[2], 
                   ((summary(lmfit.cur))$r.squared), 
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
  succ.tr$succ.ind[succ.tr$height>=8 & succ.tr$height<=12] <- 2 + succ.tr$succ.ind[succ.tr$height>=8 & succ.tr$height<=12]
  succ.tr$succ.ind[succ.tr$height>12] <- 3 + succ.tr$succ.ind[succ.tr$height>12]

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
  n.spp <- apply(X=veg[, 3:109], MARGIN=1, FUN=sum)
  veg <- cbind(veg, n.spp)
  veg <- veg[ c(1:2, 110, 3:109)]
  rm(n.spp)

# Select only PHAARU plots
  pha.smp <- filter(veg, PHAARU==1)
  table(pha.smp$year)   #should correspond to trend in PHAARU frequency of occ

# Plot of n.spp dist over time
  ggplot(data=pha.smp) +
    #   geom_boxplot(aes(x=factor(year), y=n.spp)) + 
    geom_point(aes(x=factor(year), y=n.spp)) + 
    ggtitle("Number of spp in plots with PHAARU")

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
  geom_point(aes(x=q.inund, y=n.spp, color=factor(PHAARU))) +
  facet_wrap(~year) +
  ggtitle("Number of spp present vs. plot inundating discharge: 
          plots with and without PHAARU")

# Average n.spp  by year
  tapply(X=pha.smp$n.spp, INDEX=pha.smp$year, FUN=mean)
  aov(n.spp~factor(year), data=pha.smp)->pha.aov
  summary(pha.aov)
  TukeyHSD(pha.aov)

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
