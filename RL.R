source("simulation.R")
source ("utils.R")

# The result of the getStateDesc function is a discrete state, which represents a concise description 
# of the simulated environment, relevant from the agent’s point of view.
#
# The state space must be finite and discrete, so you should consider the following limitations:
# - the resulting vectors returned by the getStateDesc function must be of the same length
# - all elements in the resulting vectors must be positive integers
#
# In order to improve the speed and reliability of learning, 
# it is desirable to keep the state space as small as possible!

#
# In this example, a state contains the following components:
# - distance to the nearest predator (limited to 30), 
# - direction in which the predator is located, and 
# - whether the agent is on the border of the map (values 1-4 indicate the corresponding border; value 5 indicates that the agent is not on the border)
#

check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}

getStateDesc <- function(simData, preyId)
{
	pos <- getPreyPos(simData, 1)

	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	if (!check.integer(res[1])) {
	  res[1] = 10
	}
	distance <- max(res[1], 1) 
	distance <- min(distance, 10)
	

	direction <- res[2]
	if (direction > 4) {
	  direction = 4
	}
	#if (distance > 3) {
	#  direction <- 5
	#}

	if (pos[2] == 1)
		border <- 1
	else if (pos[2] == MAPHEIGHT)
		border <- 2
	else if (pos[1] == MAPWIDTH)
		border <- 3
	else if (pos[1] == 1)
		border <- 4
	else
		border <- 5

	dsw = min(max(getPreyDistToNearestWater(simData, preyId), 1), 10)
	drw = getPreyDirectionToNearestWater(simData, preyId)
	
	dsf = min(max(getPreyDistToNearestForest(simData, preyId), 1), 10)
	drf = getPreyDirectionToNearestForest(simData, preyId)

	dsg = min(max(getPreyDistToNearestGrass(simData, preyId), 1), 10)
	drg = getPreyDirectionToNearestGrass(simData, preyId)

	hunger = min(max(getPreyHungerLevel(simData, preyId), 1), 5)
	thirst = min(max(getPreyThirstLevel(simData, preyId), 1), 5)
	
	shouldConsume = 0
	if (isPreyHungry(simData, preyId) && isPreyOnGrass(simData, preyId))
		shouldConsume = 1
	if (isPreyThirsty(simData, preyId) && isPreyInWater(simData, preyId))
		shouldConsume = 1
	
	isHungry <- (getPreyHungerLevel(simData, preyId) > PREY_FATAL_HUNGER-PREY_HUNGER_THRESHOLD/2)
	isThirsty <- (getPreyThirstLevel(simData, preyId) > PREY_FATAL_THIRST-PREY_HUNGER_THRESHOLD/2)
	isHidden <- isPreyHidden(simData, preyId)
	
	dirForest <- getPreyDirectionToNearestForest(simData, preyId)
	if (!check.integer(dirForest)) {
	  dirForest = 3
	}
	distForest <- getPreyDistToNearestForest(simData, preyId)
	if (!check.integer(distForest)) {
	  distForest = 10
	}
	distForest <- max(distForest, 1) 
	distForest <- min(distForest, 10)
	
	dirWater <- getPreyDirectionToNearestWater(simData, preyId)
	dirWater <- max(dirWater, 1) 
	dirWater <- min(dirWater, 4)
	
	distWater <- getPreyDistToNearestWater(simData, preyId)
	distWater <- max(distWater, 1) 
	distWater <- min(distWater, 10)
	
	dirGrass <- getPreyDirectionToNearestGrass(simData, preyId)
	dirGrass <- max(dirGrass, 1) 
	dirGrass <- min(dirGrass, 4)
	
	distGrass <- getPreyDistToNearestGrass(simData, preyId)
	distGrass <- max(distGrass, 1) 
	distGrass <- min(distGrass, 10)
	
	
	state = 1
	wantedDirection <- dirForest
	wantedDistance <- distForest
	if (isHidden == T) {
	  state = 2
	  wantedDirection <- direction
	  wantedDistance <- distance
	}
	if (isHungry == T) {
	  state = 3
	  if (isPreyOnGrass(simData, preyId)) {
	    state = 5
	  }
	}
	if (isThirsty == T) {
	  state = 4
	  if (isPreyInWater(simData, preyId)) {
	    state = 6
	  }
	}

	c(dirForest, distForest, isHidden+1)
}

# The result of the getReward function is the reward (or punishment) that the agent collects after performing the specified action.
# The returned reward should encourage the agent to perform useful actions and to discourage harmful actions. 

#
# In this example, the reward depends on the distance to the nearest predator. 
#

getReward <- function(oldstate, action, newstate) {
  reward = 0
  if (newstate[3] == 2) {
    reward = 200
  }
  else {
    reward = 30 - newstate[2]
    if (action == oldstate[1]) {
      reward <- reward + 10
    }
  }
  if (oldstate[3] == 2 && newstate[3] != 2) {
    reward = reward - 70
  }
  reward
}

getRewardOld <- function(oldstate, action, newstate)
{
  state <- newstate[2]-1
  reward <- 0
	if (state == 0) {
	  if (action == newstate[3]) {
	      reward <- reward + 25
	  }
	}
  if (state == 1) {
    reward <- reward + 100
  }
  if (state == 2) {
    if (action == newstate[4]) {
      reward <- reward + 50
    }
  }
  if (state == 3) {
    if (action == newstate[5]) {
      reward <- reward + 50
    }
  }
  if (state == 4) {
    if (action == 5) {
      reward <- reward + 100
    }
  }
  if (state == 5) {
    if (action == 5) {
      reward <- reward + 100
    }
  }
  
  if (action < 5 && action == newstate[1]) {
    reward <- reward - 75
  }

	reward	
}

