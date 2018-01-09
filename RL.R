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

getStateDesc <- function(simData, preyId)
{
	pos <- getPreyPos(simData, 1)

	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)

	direction <- res[2]

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

	dsw = min(max(getPreyDistToNearestWater(simData, preyId), 1), 30)
	drw = getPreyDirectionToNearestWater(simData, preyId)
	
	dsf = min(max(getPreyDistToNearestForest(simData, preyId), 1), 30)
	drf = getPreyDirectionToNearestForest(simData, preyId)

	dsg = min(max(getPreyDistToNearestGrass(simData, preyId), 1), 30)
	drg = getPreyDirectionToNearestGrass(simData, preyId)
	
	c(distance, dsw, dsf, dsg, border)
}

# The result of the getReward function is the reward (or punishment) that the agent collects after performing the specified action.
# The returned reward should encourage the agent to perform useful actions and to discourage harmful actions. 

#
# In this example, the reward depends on the distance to the nearest predator. 
#

getReward <- function(oldstate, action, newstate)
{
	reward <- (newstate[1]-30)

	if (oldstate[3] == action)
		reward <- reward - 10

	reward	
}

