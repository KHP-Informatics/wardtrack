#demonstrate visualisations
rm(list=ls() )
source("vis_functions.R")
source("simulate_progress_model.R")

#simulate_progress_model.R - script to simulate a bunch of n = 8 raters over 100 days admission for one patient
#and one (arbitrary) symptom.

#this just organises the data in a format that looks like output from hypothetical database.
D <- arrange( D, rater, day)

#D now has the format expected e.g. when pulled from a database - note "cumul" is not relevant and need not be 
#present as included just for show)

#D would represent the output from a DB by querying for a given patient and single symptom (from many being tracked)
# and therefore represents :
# i) multiple ratings 
# ii) over many (asynchronous) days 
# iii) by many people (i.e. because not all staff would work on the same day)
# iv) for ONE symptom of interest (e.g. thought disorder)

# > head( D )
#   rater day rating cumul
# 1     1   2      0     0
# 2     1  13     -1    -1
# 3     1  17      0    -1
# 4     1  27     -1    -2
# 5     1  31     -1    -3
# 6     1  37     -1    -4

# > tail( D )
#     rater day rating cumul
# 123     8  66      1     1
# 124     8  75      1     2
# 125     8  82      1     3
# 126     8  90      1     4
# 127     8  95      0     4
# 128     8 104      1     5


#window size - how many days in a window to aggregate over.
delta.w <-7

#select duration to review : e.g. just pass whole of D if you want the lot
#DD <- D[ define_duration( D, 20, 40 ),  ]    #this would select days 20 through 40 only
DD <- D #do over whole of D

#preprocess with windows delta.w : generate "binned" data
r.df       <- define_bins( DD, delta.w )
rater.list <- r.df$ids
w.df <- r.df$w.df
#r.df is a list with two elements - w.df is the window aggregated data, r.df is just a list of unique rater IDs (not used) 

#compute vector representation
r.df <- progress_cumVectors( w.df )

#lattice / facet plot of individual raters alone : for demonstration, not very useful
#uses raw data (not windowed)
plot.indivRater( DD )

#individual cumulative relative ratings : for demonstration, not very useful
#uses raw data (not windowed)
plot.indivRaterCum( DD )

#plot agreement on progress as "votes" as a kind of bar chart with green = votes for better, grey = votes for "same"
#and orange = votes for worse -- all arranged along timeline.
#Uses the windowed data from define_bins()
plot.rawAgreement( w.df )

#plot agreement as vectors/gradient instead of bar chart - 
#complete agreement is a tall vector up (better, green) and down (worse, orange) with
#horizontal black meaning "same".  The steeper the angle of the vector from horizontal, the greater the agreement
#over all raters in that time window.
#Uses the windowed data from define_bins()
plot.agreeVectors( r.df )

#plot cumulative vectors with balloons for number of ratings; note you cannot infer a total *magnitude* of change
#from the start / end points on the y-axis; this just shows the trajectory of change over time.  Grey balloons
#are large for high numbers of ratings, smaller for fewer ratings (so a large, green upward vector with a big grey balloon tells 
#you that a) there was strong agreement across raters that b) the patient improve and c) there was a large number of ratings
#during that window.  You would invest more in this opinion than smaller gradient/angles with smaller balloons (which
#implies less agreement and fewer ratings as a basis)
#Uses the windowed data from define_bins()
plot.agreeCumVectors( r.df, delta.w, balloon = TRUE ) 

#plot cumulative vectors with ribbon, where wide ribbon = fewer data, narrow = greater (to mimic
#everyone's favorite idea of confidence intervals)
#Note currently, the ribbon is pretty poor representation of number of ratings - likely this will be ditched - 
#as problems persuading (misusing) ggplot's ribbon feature and graphical alignment between time points.
#Uses the windowed data from define_bins()
plot.agreeCumVectors( r.df, delta.w, balloon = FALSE ) 





