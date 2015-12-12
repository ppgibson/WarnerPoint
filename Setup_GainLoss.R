## Prep gains/losses data

## Read in data
  veg.gains <- read.csv("vegdata_gains.csv")
    veg.gains <- rename(veg.gains, year=yr.2)  #have to keep column names consistent for functions.
  veg.loss  <- read.csv("vegdata_loss.csv")
    veg.loss  <- rename(veg.loss, year=yr.2)
  
# Gain counts
  # Count relevant events
  n.gain     <- apply(X=veg.gains[, 3:109], MARGIN=2, FUN=sum, na.rm=TRUE)
  n.nochange <- apply(X=veg.gains[, 3:109], MARGIN=2, FUN=function(x) sum(x==0, na.rm=TRUE))
  n.na       <- apply(X=veg.gains[, 3:109], MARGIN=2, FUN=function(x) sum(is.na(x)))
  
  # Combine them into a df
  gain.counts <- data.frame(cbind(n.gain, n.nochange, n.na))
  gain.counts$species <- row.names(gain.counts)
  row.names(gain.counts) <- NULL
  
  # Calculate percent of eligible plot samples with gains
  gain.counts <- mutate(gain.counts, n.samp=(n.nochange+n.gain), pct.gain=n.gain/(n.nochange+n.gain))
  
  # Delete extraneous variables
  # (n.gains might be useful, keep that one)
  rm(n.nochange)
  rm(n.na)

# Loss counts
  # Count relevant events
  n.loss     <- apply(X=veg.loss[, 3:109], MARGIN=2, FUN=function(x) sum(x==-1, na.rm=TRUE))
  n.nochange <- apply(X=veg.loss[, 3:109], MARGIN=2, FUN=function(x) sum(x==0, na.rm=TRUE))
  n.na       <- apply(X=veg.loss[, 3:109], MARGIN=2, FUN=function(x) sum(is.na(x)))
  
  # Combine them into a df
  loss.counts <- data.frame(cbind(n.loss, n.nochange, n.na))
  loss.counts$species <- row.names(loss.counts)
  row.names(loss.counts) <- NULL
  
  # Calculate percent of eligible plot samples with loss
  loss.counts <- mutate(loss.counts, n.samp=(n.nochange+n.loss), pct.loss=n.loss/(n.nochange+n.loss))

  
  # Delete extraneous variables
  rm(n.nochange)
  rm(n.na)
