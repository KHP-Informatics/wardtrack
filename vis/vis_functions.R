#visualisation and simulated data functions
library( ggplot2 )
library( lattice )
library( plyr )
library( grid )
library( reshape2 )

simulate_rater <- function( days, p.improve, p.worse, rater.number ) {
  #Cheap model for one rater. Assumes on any given day, the actual rating {-1,0,+1} == {"worse","same","better"}
  #is generated as the sum of two binomial processes - one generating +1 with probability p.improve (or 0 with probability
  #1-p.improve) and another generating -1 with probability p.worse (and 0 with probability 1-p.worse)
  
  # - days : vector of days to produce ratings for e.g. {1,7,14,21 ....}
  # - p.improve : probability [0,1] that this rater will vote "better"
  # - p.worse   : ditto that this rater will vote worse
  # - rater.number : id of the rater (just so it can be added to the returned data.frame)
  
  #random offset from days to represent variation 
  #rather so any raters are not (necessarily) synchronous in the days they will rate a patient.
  offset <- round( runif(length(days), min = 1, max = 7) )
  
  #vector of actual days of ratings
  this.rating <- days + offset        
  
  #2 binomial generators for independently voting "better / same" and "worse / same"   
  this.improve <- rbinom( length( this.rating), 1,  p.improve  )
  this.worse   <- -1 * rbinom( length( this.rating), 1,  p.worse )
  
  #combine to get "worse", "same", "better" on each day 
  this.total   <- this.improve + this.worse
  
  return(
    data.frame( rater = rep( rater.number, length( this.total) ),
                day = this.rating,
                rating = this.total)
  )
}

define_duration <- function( D, startDay, endDay ) {
  #For convenience ...
  #returns a bunch of indices into D which satisfy startDay <= D$day <= endDay
  minDay <- min( D$day )
  maxDay <- max( D$day )
  
  start.day <- max( minDay, startDay )    #pick appropriate start day
  end.day   <- min( maxDay, endDay )      #pick appropriate end day
  
  return( which( D$day >= start.day & D$day <= end.day ) )
  
}

define_bins <- function( D, delta ) {
  #takes a data.frame organised as :
  # col 1 = rater id
  # col 2 = day of rating
  # col 3 = rating (-1,0,1)
  
  #time windows (delta) is the duration to compute a summary statistic over (in days)

  #coded using indexes (rather than data frame labels) for portability if data.frame 
  #column names (e.g. D$day ) are not available 
  t.start   <- min( D[, 2] )      #first recorded rating day (may not be 1)
  t.end     <- max( D[, 2] )      #last recorded day of rating
  t.span    <- t.end - t.start    #duration of recording
  
  bins.breaks <- seq( t.start, t.end-1, by = delta ) #generate a sequence of bin starts (e.g. "left" side of bins)
  
  #bins.breaks[i] is the start day of a window - so, if bins.breaks[3] = 11 and bins.breaks[4] = 16, then
  #the 3rd window starts on day 11 and ends on day 15 (with delta = 5)
  
  #now, collate data into bins 
  #Define storage :
  bins.n          <- length(bins.breaks) - 1
  rating.worse    <- as.vector( rep(0, bins.n))        #for this bin, number of -1
  rating.same     <- as.vector( rep(0, bins.n))        #for this bin, number of 0
  rating.better   <- as.vector( rep(0, bins.n))        #for this bin, number of +1
  number.raters   <- as.vector( rep(0, bins.n))        #numeric number of raters in this bin
  number.ratings  <- as.vector( rep(0, bins.n))        #number of ratings (may be > number of raters - see below)
  rater.ids       <- test <- vector("list", bins.n)    #ragged array to store actual rater IDs for this bin
  window.end      <- as.vector( rep(0, bins.n))
  window.start    <- as.vector( rep(0, bins.n))
  
  for( i in 2:length(bins.breaks) ) {
    this.d                 <- D[ which( D[, 2 ] >= bins.breaks[i-1] & D[ , 2 ] < bins.breaks[i] ), ]  #select window i
    rater.ids[[i-1]]       <- as.numeric( this.d[,1] )    #store rater ids
    rating.worse[i-1]      <- length( which( this.d[,3] == -1 ) )
    rating.same[i-1]       <- length( which( this.d[,3] == 0 ) )
    rating.better[i-1]     <- length( which( this.d[,3] == 1 ) )
    number.raters[i-1]     <- length( unique( this.d[,1] ) )   #this captures the idea that one rater can provide more thatn
    #one rating - this can be checked by looking at the list rater.ids
    number.ratings[i-1]    <- length( this.d[,1] )             #number of ratings, but may not be unique raters (i.e.) one
    #rater may have rated > 1 times in this window
    window.end[i-1]   <- bins.breaks[i] - 1
    window.start[i-1] <- bins.breaks[i-1]
  }        
  
  df <- data.frame( w.start = window.start, w.end = window.end,
                    N.ratings = number.ratings, N.raters = number.raters,
                    worse = rating.worse, same = rating.same, better = rating.better )
  return( list( w.df = df, ids = rater.ids ) )
}

mean_vec <- function( d, n ) {
  #takes a triple e.g. (3,1,0) representing 3 votes for worse, 1 for same, and 0 for better
  #and assumes constant time window.  Computes the average "movement" as the resultant vector 
  #(-0.5, 0, +0.5) (i.e. one unit "down", one unit "no movement" and one unit "up" as -0.5, 0 and 0.5)
  #n is the number of raters
  #So, if n = 5, and all raters agree on "better" : sum( (0,0,5) * (-0.5, 0, 0.5) ) / 5 = 0.5
  #If n = 4, two raters vote "better" and two vote "worse" : sum( (2,0,2) * (-0.5, 0, 0.5) ) / 4 = 0.0
  
  #try/catch - there may be windows where there were 0 ratings.  This needs to be caught.
  
  if( n < 1 ) {
    return(NA)
  } else {
    return( sum( d * c(-0.5,0,0.5) ) / n )
  }
}

cumSkipNA <- function(x, FUNC)
{ #richard scriven's helper function : http://stackoverflow.com/users/3063910/richard-scriven
  d <- deparse(substitute(FUNC))
  funs <- c("max", "min", "prod", "sum")
  stopifnot(is.vector(x), is.numeric(x), d %in% funs)
  FUNC <- match.fun(paste0("cum", d))
  x[!is.na(x)] <- FUNC(x[!is.na(x)])
  x
}

scale.range <- function(x, newMin, newMax){ 
  (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin 
}

progress_cumVectors <- function( w.df ) {
  #takes output in define_bins format (w.df) and computes the cumulative sum of changes
  #defined by mean_vec (which is the average "vector" of improvement - see code for function)
  
  this.prog <- cbind( w.df$worse, w.df$same, w.df$better  )
  
  mean.vecs <- as.vector( rep(0, dim(this.prog)[1]))
  for( i in 1:dim(this.prog)[1] ) {
    mean.vecs[i] <- mean_vec( this.prog[ i, ], w.df$N.ratings[i] )
  }
  
  upper.mean.vecs <- max( mean.vecs, na.rm = TRUE )
  lower.mean.vecs <- min( mean.vecs, na.rm = TRUE )
  range.mean.vecs <- (upper.mean.vecs - lower.mean.vecs) / 2
  max.range.mean.vecs  <- (upper.mean.vecs - lower.mean.vecs) / 2

  min.range.mean.vecs  <- 0
  scaled.n.mean.vecs  <- scale.range( w.df$N.raters, max.range.mean.vecs, min.range.mean.vecs )
  
  cum.vecs <- cumSkipNA(mean.vecs, sum)
  upper.cum.vecs  <- max( cum.vecs, na.rm = TRUE )
  lower.cum.vecs  <- min( cum.vecs, na.rm = TRUE )
  max.range.cum.vecs  <- (upper.cum.vecs - lower.cum.vecs) / 2

  min.range.cum.vecs  <- 0
  scaled.n.cum.vecs   <- scale.range( w.df$N.raters, max.range.cum.vecs, min.range.cum.vecs )
  
  df <- data.frame( t = w.df$w.end, t.start = w.df$w.start,
                    avrg.vec = mean.vecs, avrg.rater.lim = scaled.n.mean.vecs,
                    cum.vec  = cum.vecs,  cum.rater.lim = scaled.n.cum.vecs, N.ratings = w.df$N.ratings )
  
  return( df  )
}

########### visualisation / graphing functions

plot.indivRater <- function( D ) {
  #format of dataframe D by column : 
  # rater = unique rater ID
  # day   = day of rating
  # rating = {-1,0,1} representing {worse, same, better}
  # cumul  = for a given rater, the cumulative rating they have given over time
  
  p <- ggplot(D, aes(x=day, y=rating, colour=rater)) + geom_point( size = 3)
  p + facet_grid( rater ~. )
}

plot.indivRaterCum <- function( D ) {
  #plots cumulative rating for each rater
  #see plot.indivRater for dataframe format 
  
  p <- ggplot(D, aes(x=day, y=cumul, colour=rater)) + geom_line() + geom_point( size = 3 )
  p + facet_grid( rater ~. )
}

plot.rawAgreement <- function( w.df ) {
  #plots bar chart showing number of "votes" in each time window of w.df.
  #format for W is dataframe as returned by define_bins (e.g. w.df)
  #By columns : 
  #  w.start    - start day of window (size delta - see define_bins)
  #  w.ed       - end day of window (ditto)
  #  N.ratings  - number of ratings in the window [w.start, w.end]
  #  N.raters   - number of unique raters (i.e. people) doing the ratings in this window
  #  worse      - number of people "voting" worse in this time window
  #  same       - as above, but for voting same
  #  better     - as above, but for voting better
  
  temp.df <- data.frame( t = w.df$w.end, N = w.df$N.ratings, worse = w.df$worse,
                         same = w.df$same, better = w.df$better )
  
  melt.df <- melt(temp.df, id.vars = c("t", "N"),
                  variable.name = "rating", 
                  value.name = "count") 
  
  #find rating == worse, and make a negative number
  melt.df$count[ melt.df$rating == "worse" ] <- melt.df$count[ melt.df$rating == "worse" ] * -1
  
  #define a named pallete for {worse, same, better}
  cpallete <- c(worse = "#D55E00", same = "#000000", better = "#009E73")  #colour-blind friendly pallette (red, grey, green)
  
  temp.df$same <- as.factor( temp.df$same )

  ggplot(temp.df, aes(x=t) ) +
    geom_bar(  aes(y = -worse), stat="identity", fill = cpallete["worse"]) +
    geom_bar(  aes(y = better), stat="identity", fill = cpallete["better"]) +
    geom_point(aes(y = 0, size = same), shape=15, fill = cpallete["same"], alpha = 0.4) +
    scale_size_manual(values=c( as.numeric( levels( temp.df$same ) ) * 3 ), guide=FALSE ) +
    geom_hline(yintercept = 0.0) +
    ylab("Votes\n") + xlab("\nTime Window")

}

plot.agreeCumVectors <- function( r.df, delta.w, balloon = FALSE ) {
#plot of progress (cumulative sum of vectors)
  
#Expects data in the r.df dataframe format (see progress_cumVectors)
#also, delta.w must be the window size passed to progress_cumVectors to make the segment/vectors
#the correct length

#NOTE : this plot shows trend in relative measurements, but the MAGNITUDE of change 
#cannot be inferred because it represents the progress vectors for each window (e.g. 5 day)
#"lined up" (much in the same way a diagram showing how to construct the resultant vector would look)

#So, a change from 0 on the y-axis to 1.5 over the entire duration does not mean
#the "change" in the symptom is absolutely 1.5.

#The ribbon (or balloons) gives a qualitative indication of the amount of data available - 
# the wider the ribbon, the less data was available for that window.
#(which can be taken as a measure of uncertainty in the estimate)
#Conversely, where there is no ribbon, this represents windows where the MOST data was available.
  
#Currently, the ribbon plot gives inconsistent results (because of problems with alignment on x-axis) - balloon is favoured.

#Balloons are the opposite : more data, bigger balloon.
  
  r.df$qual.change <- c(rep("Same",dim(r.df)[1]))
  r.df$qual.change[ r.df$avrg.vec > 0 ] <- "Improved"
  r.df$qual.change[ r.df$avrg.vec < 0 ] <- "Worsened"
  r.df$qual.change <- factor( r.df$qual.change )
  
  r.df$N.ratings <- factor( w.df$N.ratings )
  
  cpallete <- c(Worsened = "#D55E00", 
                Same = "#000000",
                Improved = "#009E73")  #colour-blind friendly pallette (red, grey, green)
  
  if( balloon == FALSE ) {
  #ribbon for N.ratings
  ggplot(r.df, aes(x=t-delta.w, y=cum.vec)) +
    geom_ribbon(aes(x = t-(delta.w / 2), ymin=cum.vec-cum.rater.lim, ymax=cum.vec+cum.rater.lim), fill = "gray",
                alpha=0.8) +
    geom_segment(aes(x = t-delta.w, xend = t, y = cum.vec - avrg.vec, yend = cum.vec, colour = qual.change ),
                 arrow = arrow(length = unit(0.2,"cm")), size = 0.75) +
    scale_colour_manual(values=cpallete,guide=FALSE) +
    ylab("Cumulative\n") + xlab("\nTime Window")
  
  } else {
    #instead of ribbon, a circle which varies in size with number of items
    ggplot(r.df, aes(x=t-delta.w, y=cum.vec)) +
    geom_point( aes(x = t-delta.w, y = cum.vec - avrg.vec, size = N.ratings ), shape = 21, fill = "gray",
                  alpha = 0.8)  +
    scale_size_manual(values=c( as.numeric( levels( r.df$N.ratings ) ) * 3 ), guide=FALSE ) +
    geom_segment(aes(x = t-delta.w, xend = t, y = cum.vec - avrg.vec, yend = cum.vec, colour = qual.change ),
                 arrow = arrow(length = unit(0.2,"cm")), size = 0.75) +
    scale_colour_manual(values=cpallete,guide=FALSE) +
    ylab("Cumulative Agreement\n") + xlab("\nTime Window")
  }
  
}

plot.agreeVectors <- function( r.df ) {
  #instead of printing the cumulative vector (see plot.agreeCumVectors) this
  #function just shows the relative agreement vectors on the horizontal - kind of like a feather plot
  
  r.df$qual.change <- c(rep("Same",dim(r.df)[1]))
  r.df$qual.change[ r.df$avrg.vec > 0 ] <- "Improved"
  r.df$qual.change[ r.df$avrg.vec < 0 ] <- "Worsened"
  r.df$qual.change <- factor( r.df$qual.change )
  
  r.df$N.ratings <- factor( w.df$N.ratings )
  
  cpallete <- c(Worsened = "#D55E00", 
                Same = "#000000",
                Improved = "#009E73")  #colour-blind friendly pallette (red, grey, green)
  
  #vector plot of trajectories in symptom scores
  ggplot(r.df, aes(x=t, y=0)) +
    geom_point( aes(size = N.ratings), shape =21, fill = "gray", alpha = 0.8 ) +
    scale_size_manual(values=c( as.numeric( levels( r.df$N.ratings ) ) * 2 ),guide=FALSE ) +
    geom_segment(aes(xend = t+4, yend = avrg.vec, colour = qual.change),
                 arrow = arrow(length = unit(0.2,"cm")), size = 0.75) +
    scale_colour_manual(values=cpallete,guide=FALSE) +
    ylim(-0.6,0.6) +
    #stick in annotations
    geom_hline(yintercept = 0.5) +
    annotate("text", label = "Complete Agreement : Improvement", x=Inf, y=Inf, hjust=1.0, vjust=2.5) +
    geom_hline(yintercept = -0.5) +
    annotate("text", label = "Complete Agreement : Worsening", x=Inf, y=-Inf, hjust=1.0, vjust=-2.5) +
    ylab("Agreement\n") + xlab("\nTime Window")

}

