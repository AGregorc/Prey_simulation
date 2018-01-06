source("RL.R")

#
# You can experiment with the different settings of the simulated environment
#

# MAPWIDTH <- 50
# MAPHEIGHT <- 50
# NUMPREYS <- 3
# NUMPREDATORS <- 3


# for the given (incomplete) example in the file "RL.R",
# the vector c(30, 4, 5) defines the maximum values of the corresponding elements 
# in the adopted state description

qmat <- qlearning(c(30, 4, 5), maxtrials=1000)

save(qmat, file="qmat.RData")
load(file="qmat.RData")

simulation(Q=qmat)


buildGraphVector <- function(states, initial_trials, max_trials, step) {
  vec <- vector()
  st <- 1
  for (trials in seq(initial_trials, max_trials, step)) {
    qmat <- qlearning(states, maxtrials=trials)
    vec[st] <- getAvg(qmat)
    st <- st+1
  }
  vec
}

getAvg <- function(qmat) {
  sum <- 0
  for (i in 1:20) {
    sum <- sum + simulation(Q=qmat)
  }
  sum <- sum/20
}

# just for testing if the function is working
graphVector <- buildGraphVector(c(30, 4, 5), 100, 1000, 100)
save(graphVector, file="graph.RData")


