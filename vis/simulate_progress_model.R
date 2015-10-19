############# simulate some data
# in this example, assume 100 day admission, where around day 50 some intervention changes (medication, etc.)
# and the probability of improvement increases for all raters.  Visualisation functions do not
# depend on these (arbitrary) settings for days / week patterns of working, its just for convenience
# to generate simulated data.  Visualisation functions tolerate gaps in data when no ratings present reasonably gracefully.

period.1 <- 50
period.2 <- 100

#the number of raters (e.g. staff)
n.rater <- 8

days.1 <- seq(0,period.1,by=7)        #vector of days assuming a 7 day working shift pattern for first 50 days
days.2 <- seq(period.1,period.2,by=7) #same, but for second period (last 50 days)

D <- data.frame()

#for first 50 days, nothing much works and patient deteriorates.
prob.better <- 0.15
prob.worse  <- 0.4

for( i in 1:n.rater ) {
  this.d <- simulate_rater( days.1, prob.better, prob.worse, i );
  D <- rbind( D, this.d )
}


# ... then something happens around day 50, and starts to improve :
prob.better <- 0.6
prob.worse  <- 0.2

for( i in 1:n.rater ) {
  this.d <- simulate_rater( days.2, prob.better, prob.worse, i );
  D <- rbind( D, this.d )
}

#add cumulative ratings - just for show really.
D$cumul <- c(rep(0, dim( D )[1] ))
for( i in 1:n.rater ) {
  this.rater <- which( D$rater == i )
  D$cumul[ this.rater ] <- cumsum( D$rating[ this.rater ] ) 
}

D$rater <- as.factor(D$rater)   #turn rater id from numeric to factor to make ggplot happy.

############ end of simulated data generation

