# Raw veg data = \veg\

# Pull out a separate df for a single sp
test <- dcast(data=veg[c(1:2, 22)], plot ~ year, value.var="CARLAN")

# ...or, a separate df for a single plot
plot.cur <- "P101"

veg.change <- veg
veg.change[, 3:109] <- NA  #fill all the data columns with NA

temp <- veg[veg$plot==plot.cur, 3:109]
temp2 <- temp

for(i in 2:5) {
#   print(i)
#   temp[i, ] - temp[(i-1), ] -> temp2[i, ]
  temp[i, ] - temp[(i-1), ] -> veg.change[veg.change$plot==plot.cur, 3:109][i, ]
}
temp2[-1, ]->temp2  #get rid of 1990 data, can't use with no changes

d.yrs <- c("d1994", "d2001", "d2006", "d2013")
cbind(year=d.yrs, plot=plot.cur, temp2) -> tester

veg.change[veg.change$plot==plot.cur, 1:5][2, ]

tapply(X=veg[, ], INDEX=veg$plot, FUN=function(x) print(x$plot))


veg.byplot <- group_by(veg, plot)
testfunc

testfunc <- function(x) {
  print(nrow(x))
}

###############

veg.change <- veg
veg.change[, 3:109] <- NA  #fill all the data columns with NA


for(j in unique(veg$plot)) {
#   print(j)
  
  plot.cur <- j
  
  temp <- veg[veg$plot==plot.cur, 3:109]
  
  for(i in 2:5) {
  temp[i, ] - temp[(i-1), ] -> veg.change[veg.change$plot==plot.cur, 3:109][i, ]
  }
}

# Now get rid of the 1990 rows
veg.change <- filter(veg.change, year!=1990)
apply(veg.change, 2, function(x) sum(x==1))

# Now convert to long form
change. l <- melt(veg.change, id.vars=c("year", "plot"), variable.name="species", value.name="change.dir")
change.l$change.dir <- factor(change.l$change.dir)

# Count numbers of each change direction
  change.counts <- ddply(.data=change.l,
                         .variables="species", 
                         .fun=summarize,
                         n.gain=sum(change.dir==1),
                         n.loss=sum(change.dir==-1),
                         n.nochange=sum(change.dir==0))

  # Add total n.occ to \change.counts\
    n.occ <- apply(veg[, 3:109], 2, sum)
    vegtot <- as.data.frame(n.occ)
    vegtot$species <- row.names(vegtot)
    change.counts <- merge(change.counts, vegtot, by="species", all=TRUE)

# Add yr 1 and yr2 columns to \change.l\
  change.l <- rename(change.l, yr.2=year)
  surv.yrs <- c(1990, 1994, 2001, 2006, 2013)

  y2.ind <- match(change.l$yr.2, surv.yrs)
  yr.1 <- surv.yrs[y2.ind-1]

  change.l <- cbind(yr.1, change.l)

# Get long form of orig veg pres/abs data
  veg.l <- melt(veg, id.vars=c("year", "plot"), variable.name="species", value.name="yr1.pres")
  veg.l <- filter(veg.l, year!=2013)

# Combine change and presence into one long-form df
  change.l <- merge(veg.l, change.l, 
        by.x=c("year", "plot", "species"),
        by.y=c("yr.1", "plot", "species"), 
        all=TRUE)

# Create separte dfs for gains and losses
  # Gains
  gains <- change.l
  gains$change.dir[gains$yr1.pres==1] <- NA  
  veg.gains <- dcast(data=gains, yr.2+plot ~ species, value.var="change.dir") #put back in wide form, now with ineligible plot samples as NAs
  gain.counts <- ddply(.data=gains,
                           .variables="species", 
                           .fun=summarize,
                           n.gain=sum(change.dir==1, na.rm=TRUE),
                           n.nochange=sum(change.dir==0, na.rm=TRUE),
                           n.na=sum(is.na(change.dir)) )
  
    # Calculate percent of eligible plot samples with gains
    gain.counts <- mutate(gain.counts, n.samp=(n.nochange+n.gain), pct.gain=n.gain/(n.nochange+n.gain))
    gain.spp <- gain.counts$species[gain.counts$n.samp>=50 & gain.counts$pct.gain>=0.05]  #list of eligile spp for fitting a model on gains 
    gain.spp <- sort(gain.spp)

  # Losses
  loss <- change.l
  loss$change.dir[loss$yr1.pres==0] <- NA
  veg.loss <- dcast(data=loss, yr.2+plot ~ species, value.var="change.dir") #put back in wide form, now with ineligible plot samples as NAs
  loss.counts <- ddply(.data=loss,
                           .variables="species", 
                           .fun=summarize,
                           n.loss=sum(change.dir==-1, na.rm=TRUE),
                           n.nochange=sum(change.dir==0, na.rm=TRUE),
                           n.na=sum(is.na(change.dir)) )
  
    # Calculate percent of eligible plot samples with gains, and total # of eligible plot samples
    loss.counts <- mutate(loss.counts, n.samp=(n.nochange+n.loss), pct.loss=n.loss/(n.nochange+n.loss))
    loss.spp <- loss.counts$species[loss.counts$n.samp>=50 & loss.counts$pct.loss>=0.05]
    loss.spp <- sort(loss.spp)  #alphabetize
    
# Write some output to use in future scripts
  write.csv(veg.gains, paste(DirData, "vegdata_gains.csv", sep=""), row.names=FALSE)
  write.csv(veg.loss,  paste(DirData, "vegdata_loss.csv",  sep=""), row.names=FALSE)