###########################################################
## CALCULATE LINEARLY-WEIGHTED INUNDATION DURATIONS ##
###########################################################

## Based entirely on empirical values, not interpolation.


#### READ IN AND PREPARE DATA ####
# 1. Time series discharge data, all possible years (50 yrs before 1st survey to yr of last survey)
  raw.flowdata <- read.csv("Gunn_DailyMeanCFS_WY1939-2013.csv")

  # Add an index distinguish growing season months
  months.all     <- as.numeric(substr(raw.flowdata$date, 6, 7))
  months.all.ind <- (months.all > 4 & months.all < 10)
  raw.flowdata  <- cbind(raw.flowdata, months.all.ind)
  
# 2. Inundating discharge data
  inundatingQs.df <- read.table("WP_InundatingDischarges.txt", 
                                col.names=c("plot", "q.inund", "elevation", "unknown"))

  # Create a new/cleaner version of inundatingQs.df, which will be used to collect 
  # the calculated inundation durations
  plotdata <- select(inundatingQs.df, -unknown)  #Get rid of unknown column
  plotdata$plot <- paste("P", plotdata$plot, sep="")  #Add Ps to plot numbers
  plotdata <- plotdata[plotdata$plot!="P129", ]  #Get rid of P129
  plotdata <- plotdata[plotdata$plot!="P999", ]  #...and get rid of P999


  
#### EXAMINE FLOW HISTORY ####
## Plot flow history for the period in question
  surv.yrs <- c(1990, 1994, 2000, 2006, 2013)  #the set of survey years

  for(i in surv.yrs) {
    print(i)
    
    flowhist.plot <- ggplot(data=raw.flowdata[raw.flowdata$wyear>(i-10) & raw.flowdata$wyear < (i+1), ]) +  #show 5 yrs prior to and 1 yr after survye
      geom_point(aes(x=as.POSIXct(date), y=discharge, color=months.all.ind)) +
      geom_hline(aes(yintercept=300)) +
      ggtitle(i)
    
    print(flowhist.plot) 
  }

  rm(months.all)
  rm(months.all.ind)
  rm(i)


#### INUNDUR FUNCTION ####
# Select some lambda values  #larger values = faster decay! = heavier weighting of recent values

# Function to calculate inundation durations for a specified 
# start yr/length of record/decay rate:
# (GA suggests calculating inundur once, then match plot q.inund - 
#  but so far I can't see any way to do this that would involve
#  less calculation than the current loop through plots.)
inundur.lin <- function(year, length) {     #default values
    # Extract out only desired years of data for given sample year
      start.year  <- year - (length - 1) 
      startdate.ind <- which(raw.flowdata$date == paste(start.year, "-05-01", sep=""))  #starting at beg of growing season - to make numbers line up correctly. 
      enddate.ind <- which(raw.flowdata$date == paste(year, "-07-17", sep=""))  #Use July 17 as the last date, in all years.
      flow.data <- raw.flowdata[startdate.ind:enddate.ind, ]
#       flow.data <- flow.data[(flow.data$wyear >= start.year), ]
      # Print selected date, 1st date of flow record, and last date of flow record
#       print(paste(year, length, flow.data$date[1], flow.data$date[nrow(flow.data)]))

    # Create an index to limit flow data to growing season 
    # (index will be applied after calculation of decay)
      months <- as.numeric(substr(flow.data$date[!(flow.data$wyear==start.year & flow.data$months.all.ind==FALSE)], 6, 7))  #limit data to only growing season dates during the earliest year.
      months.index <- (months>4 & months <10)
      rm(months)
  
    # Calculate flow duration curve for the extracted years of data
      # Calculate decreasing weights (speed of decay as a function of desired length of record, n.days)
        n.days <- nrow(flow.data[!(flow.data$wyear==start.year & flow.data$months.all.ind==FALSE), ])
#         daily.wt <- 1/n.days #amount by which the weight decreases each day
#         decay.lin <- 1 - (daily.wt*(0:(n.days-1)))  #from zero rather than from one so that first flow value has zero decay.
#         wts <- rev(decay.lin*n.days) #reverse so that strongest weight is for end of flow sequence!(i.e., most recent flows)  #not really necessary, but keeps #s from getting so small
        wts <- 1:(n.days)  #the above three lines of code mimic how it's done in the exponential weighting, but it's mathematically equivalent to this much simpler approach.      # Now, reduce wts to only include growing season.
      sum.wts <- sum(wts*months.index)  #only wts from the growing season days should be included in the total
              
      # Add a blank column to the existing inundurations df
      inundurations$inundur <- NA #blank column to fill
    
    # Loop through the plots
      for(i in 1:nrow(inundurations)) {
        ex.days.cur <- flow.data$discharge >= inundurations$q.inund[i]  #days when flow EQUALS OR EXCEEDS given inundating discharge.
        inundurations$inundur[i] <- sum(wts*ex.days.cur*months.index) / sum.wts
      }
    
    # Give the new inunduration column (newcol) a better name
      colname <- paste("inundur.", length, sep="")
#       colname <- paste("inundur.e", log10(lambda), sep="")
      colnames(inundurations)[ncol(inundurations)] <- colname
#       colname <- paste("inundur.", year, sep="")
#       colnames(inundurations)[ncol(inundurations)] <- colname
    
    return(inundurations)
}


#### CALL THE FUNCTION TO CALCULATE INUNDUR ####
# The set of years and lambdas (rec.lengths)
  surv.yrs <- c(1990, 1994, 2001, 2006, 2013)
  rec.lengths <- c(1,2,3,4,5,7,10,12,15,17,20,25,30,35,40,45,50)

# Clean df to fill
  inundur.allyrs <- matrix(nrow=0, ncol=(4+length(rec.lengths)))  #4 data columns (including yr, to be assigned below), plus one column for each lambda value to be computed.

## Double-for loop to calculate inundurs with each lambda value, in 
## each survey year.  

  for(k in surv.yrs) {
    print(k) 
    
    # Need a clean version of inundurations each time, in order to add columns during the for loop.
      inundurations <- plotdata  
    
    # Run the inundur.exp function, which calculates an inundur value for each plot, 
    # once for each specified value of lambda
      for(i in 1:length(rec.lengths)) {
        inundurations <- inundur.lin(year=k, length=rec.lengths[i])
      }
    
    # Add a year column
      inundurations$year <- k
    
    # Merge current year's results with the compilation df
      inundur.allyrs <- rbind(inundur.allyrs, inundurations)
  }

# Fix up inundur.allyrs
  # Move yr column to the front
    inundur.allyrs <- inundur.allyrs[, c(ncol(inundur.allyrs), 1:(ncol(inundur.allyrs) - 1))] #move [year] column to the front
  # Add a column for 50 year record, no weighting -> paste it in from flowhist.exp
    flowhist.exp <- flowhist.exp <- read.csv("Inundurations_lambdas_allyrs_all.csv") #Inundation durations calculated with exponential weighting.
    inundur.allyrs <- cbind(inundur.allyrs, inundur.inf=flowhist.exp[, "inundur.0"])
    rm(flowhist.exp)

## Examine distribution of inundur values
  for (i in 1:(ncol(inundur.allyrs)-3)) {
    print(i)
    print(colnames(inundur.allyrs)[i+3])
    hist(inundur.allyrs[, (i+3)], main=paste("AllYrs", colnames(inundur.allyrs)[i+3]))
  }
  
  # (inundur.allyrs$year==1990)
  
  # How many inundur=0 for each rec.length?
  apply(X=inundur.allyrs[, 5:21], MARGIN=2, FUN=function(x) sum(x==0))

#### WRITE OUTPUT FILE ####
  write.csv(inundur.allyrs, "Inundurations_linear_allyrs.csv", row.names=FALSE)


#### REFORMAT OUPUT ####
# Convert \inundurations\ to long form - one record for each plot in 
# each year (i.e., each plot sample).
  durations <- melt(data = inundurations, 
                    id.vars = c("plot", "q.inund", "elevation"),
                    variable.name = "year", 
                    value.name = "inundur", 
                    na.rm=FALSE)

  durations$year <- substr(durations$year, 9, 12) #covert [year] column to plain numbers
  durations$year <- as.numeric(durations$year)

#### END SCRIPT (FOR NOW) ####