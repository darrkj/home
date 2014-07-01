
# Useful function for get an average time.
timer <- function(fun, iters, ...) {
  avgTime <- 0
  for (n in seq(iters)) {
    avgTime <- avgTime + system.time(fun(...))[3]
  }
  return(avgTime / iters)
}

# Less verbose but captures overhead of for loop.
timer2 <- function(fun, iters, ...) {
  system.time(for(n in seq(iters)) fun(...))[3] / iters
}


# Code by Matt Asher for statisticsblog.com
# Feel free to modify and redistribute, but please keep this notice 

costPerKafon = 50

# How many plates per kafon (it dies when hits this many)
plates = 4

# Changing this changes the variability in the Kafon's path
windSpeed = 1

# Dimmensions of mine field
depth = 1000
width = 1000

# Chance that a given location will have a mine
# We will try out diffent mine probabilities
# This may take a long time to run!
# To reduce run time, lower the number of mineProbs and trailsPerP
mineProbs = (1/2)^(10:23)

# How many trials should we simulate per mineProbs?
trialsPerP = 200

# Max cost we are willing to spend to check if a field has mines
costThreshold = 30000
maxKafons = floor(costThreshold/costPerKafon)

# Where will we be starting the kafons?
# Function to find optimal starting points when total number of kafons is 
# indeterminate. This may not be an efficient algorithm
buildStartPoints <- function(n, width) {
  startPoints = rep(0, n)
  k = 1
  i = 1
  count = 0
  while(k < n) {
    countBy = (1/2)^i
    count = countBy
    while(count < 1) {
      if(!count %in% startPoints) {
        startPoints[k] = count
        k = k + 1
      }
      count = count + countBy
    }
    i = i+1
    
  }
  # return(startPoints)
  
  return(round(startPoints*width))
}

startPoints = buildStartPoints(maxKafons, width)

# Create a blank data frame with trialsPerP * length(mineProbs) rows and 
# Columns for kafonsRun, minesFound, minesThere, 
results = matrix(0, nrow=(trialsPerP * length(mineProbs)), ncol=5)
colnames(results) = c("mineP", "kafonsRun", "mineFound", "minesThere", "areaCovered")

curRow = 0
for(mineP in mineProbs) {
  
  for(i in 1:trialsPerP) {
    # Generate the minefield
    covered = matrix(0, nrow=width, ncol=depth)
    mineMatrix = matrix(runif(width*depth), nrow=width, ncol=depth)
    mineMatrix = ifelse(mineMatrix < mineP, 1, 0)
    
    curRow = curRow + 1
    kafonsTried = 1
    mineFound = FALSE
    
    # Go until we find a mine or hit max cost
    while(!mineFound & (kafonsTried < maxKafons)) {
      startY = startPoints[i]
      startX = 0
      
      xPos = startX
      yPos = startY
      
      for(j in 1:depth) {
        
        # This is the noise in movement, feel free to adjust the function
        xPos = xPos + windSpeed
        yPos = yPos + rnorm(1,0,1)
        rndY = round(yPos)
        
        # Are we in the field?
        if(xPos > 0 && xPos <= depth && rndY > 0 && rndY <= width) {
          # Set this part of covered to 1
          if(covered[rndY,xPos] != 1) {
            covered[rndY,xPos] = 1
          }
          
          # Did we hit a mine? 
          if(mineMatrix[rndY,xPos] == 1) {
            
            # Note that we found a mine this trial
            results[curRow,] = c(mineP, kafonsTried, 1, sum(mineMatrix), sum(covered)) 
            
            # Note that at least one mine has been found
            mineFound = TRUE
            
            break;
          }
        }
      }
      kafonsTried = kafonsTried + 1
    }
    
    # If none found, record the data here
    if(!mineFound) {
      results[curRow,] = c(mineP, kafonsTried, 0, sum(mineMatrix), sum(covered))
    } 	
  }
}
par(mar=c(5.1,4.1,4.1,1.0)) plot(.5,.5,ylim=c(0, costThreshold), 
                                 xlim=rev(c(min(mineProbs),max(mineProbs))), yaxt="n", col="white", log="x", 
                                 xlab="Mine probability per unit", ylab="Cost to detect first mine", bty="n") 
title(main="Cost to detect the presence of landmines \n vs. probability of a 
 mine in a given zone. \n Numbers represent the true number of mines in the 
 field.", cex.main=1) axis(2,at=axTicks(2), labels=sprintf("$%s", axTicks(2))) # 
points(results[1:2800,"mineP"], (results[1:2800,"kafonsRun"]*costPerKafon), 
       pch=20, col="blue")











library(maps)
map("world", "China")
map.cities(country = "China", capitals = 2)
map("state", "New Jersey")
data(us.cities)
map.cities(us.cities, country="NJ")




choro <- c( "red", "blue")
map("state",  lty = 1, lwd =1,
    boundary=TRUE, fill=TRUE,
    col=choro)
