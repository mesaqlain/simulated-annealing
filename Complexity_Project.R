### COMPLEXITY AND APPROXIMATIONS PROJECT FOR PASCAL REBREYEND
### Written by Murshid Saqlain 
### Algorithm 1 - Simulated Annealing

## INSTALL AND LOAD Packages ##
#install.packages("data.table")
#install.packages("truncnorm")
library(data.table)
library(truncnorm)

## LOAD THE DATA ##
#Distance matrix. dij.
distance_data <- fread('~/Complexity and Approximations/PHD_Matrix.txt')
class(distance_data)

#Weight matrix.
populationPoints_data <- fread('~/Complexity and Approximations/PHD_Pop.txt')
#Nodes matrix.
settlements_data <- fread('~/Complexity and Approximations/PHD_Settlements.txt')
#Take only the weights
weights <- populationPoints_data[,3]
#Need to turn weights into a vector to do calculation in objective function.
weights_v <- as.vector(weights[["V3"]])

Ikea_locations <- fread('~/Complexity and Approximations/Ikea.txt')
Settlement_IDs <- Ikea_locations[,4] + 1 #Add 1 because first settlement ID is 0.

## TEST DATASET ##
#Take a subset of the data. i 1 to 50. j 1 to 100. So 50 nodes here. 100 population points.
#distance_data_test <- distance_data[1:50, 1:100]
#settlements_data_test <- settlements_data[1:50,]
#colnames(settlements_data_test) <- c("X", "Y","ID")
#weights_test <- weights[1:100,]
#Need to turn weights into a vector to do calculation in objective function.
#weights_test_v <- as.vector(weights_test[["V3"]])


## OBJECTIVE FUNCTION ##
#This is the objective function that calculates the minimum of the sum of weighted distance.
#The function input is the 19 samples drawn from the total number of nodes.
objective_function <- function(points) {
  #Take only the sampled rows from distance matrix.
  selected_nodes <- distance_data[points,] 
  #Calculate weighted distance. Make use of sweep function.
  weighted_distance <- sweep(selected_nodes,MARGIN=2,weights_v,`*`) 
  #Take only the minimum of each column (designated by 2), as every other number in that column
  #should be 0. Sum it all up to get the value of the objective function.
  value_new <- sum(apply(weighted_distance, 2, min))
  return(value_new)
}

#Reference Cost
referece_cost <- objective_function(Settlement_IDs$V4) ##262664839929

## SIMULATED ANNEALING ALGORITHM ##
#Function with inputs - func is the objective function, s0 is the initial 19 samples,
#niter is the number of iterations before the code stops running, step size is for
#the temperature at each iteration.

simulated_annealing <- function(func, s0, niter, step = 0.01) {

  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- func(s_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  #Create empty list for values and temperatures to store results.
  val <- c()
  temperatures <- c()
  states <- c()
  
    for (k in 1:niter) {     
    Temp <- (1 - step)^k
    ##To choose a neighbour, I replace ONE of the rows with a neighbouring row.
    ##The method is to use that row as a mean in a normal distribution with sd of 5, and
    ##choose a new number. If that new number is already in the 19 samples, then repeat
    ##until a unique new number is chose so that the 19 samples are distinct.
    
    #Choose a random row to replace from current state.
    row_to_replace <- sample(s_n, 1) 
    #Generate a new value using that number as mean and sd of 5
    new_value<-round(rtruncnorm(1,1,1938,as.numeric(row_to_replace),5))
    #Make sure that the new value is not already existing in current state.
    while (any(s_n==new_value)) {
      new_value<-round(rtruncnorm(1,1,1938,as.numeric(row_to_replace),5))
    }
    #Replace the randomly selected row with a new row.
    s_n[which(s_n==row_to_replace)]<-new_value
    
    #Evaluate objective function at neighbouring state.
    f_n <- func(s_n)
    # update current state
    if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) {
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
    val <- c(val,f_n)
    temperatures <- c(temperatures,Temp)
    states <- c(states,s_n)
    
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b, 
              values = val, temp = temperatures, states = states))
}


## RUN THE ALGORITHM FOR A SOLUTION ##
solution <- simulated_annealing(objective_function, niter = 1000, s0 = sample(1:1938,19))
# The selected states/rows at each iteration. 
states_matrix <- matrix(c(solution$states),byrow=T,ncol=19)



## COST OF SA
SA_cost <- solution$values[1000] #453420259384
# % Difference in cost
SA_diff <- ((referece_cost-SA_cost)/referece_cost)*100 #-72.62313


## PLOTS ##


#Plot the objective function value at each iteration.
plot(solution$values,type="l", ylab= "Objective function")
#Plot the temperature value at each iteration.
plot(solution$temp,type="l", ylab= "Temperature")


#Best State Solution.
best_states <- solution$best_state


#Extract coordinates of the best state.
best_locations <- settlements_data[best_states,]

#Known actual Ikea locations. Taken from provided Excel file in the course page.
loc_x <- c(6573794,6388469,6476990,6723684,6403765,6218831,6563749,6590299,6407232,
            6283062,7332628,6583416,6705148,6472657,6267919,6926002,6158115,6608630,
            6637401)
loc_y <- c(666225,323737,534198,608805,452533,360704,507780,662256,321608,580762,
           915833,410279,523076,313901,448482,620359,373038,581967,650899)

#Create a plot of the best locations.
plot.new()
plot(best_locations$V2,best_locations$V1,type="p",col="red",
     main="Coordinates of Best Locations",
     xlab="X Coordinate",
     ylab="Y Coordinate",
     pch=16)
points(loc_x,loc_y,col="blue",pch=17)
legend("bottomright", 
       legend = c("Solution", "Actual"), 
       col = c("red","blue"), 
       pch = c(16,17), 
       bty = "o", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = c("red","blue"), 
       horiz = F,
       inset = c(0.1, 0.1))



library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points from solution.
lon <- c(17.546432,
         20.24413071,
         15.55280678,
         15.88984818,
         17.5164543,
         12.89424867,
         14.6312089,
         18.0450116,
         15.73191287,
         13.14572362,
         13.43320002,
         16.54801484,
         14.31000203,
         15.1533692,
         17.14329148,
         15.09506476,
         12.98259836,
         14.53468272)
lat <- c(58.90244273,
         64.25370636,
         56.62698031,
         58.58404883,
         62.50415784,
         56.33712447,
         61.11603527,
         59.32082236,
         58.23274263,
         58.5076622,
         59.43635122,
         57.46494489,
         57.85406741,
         57.1273544,
         60.71349538,
         60.67688534,
         57.75928657,
         56.06070018)
solution_coordinates <- as.data.frame(cbind(lon,lat))


# creating a sample data.frame with your lat/lon points from actual coordinates.
lon_actual <- c(17.91678,
12.049947,
15.585639,
16.989094,
14.201989,
12.760882,
15.136266,
17.85965,
12.000483,
16.318393,
24.13155,
13.420827,
15.419848,
11.819336,
14.161956,
17.332445,
12.98736,
16.452458,
17.693409)

lat_actual <- c(59.270347,
57.604116,
58.432303,
60.634148,
57.773427,
56.094277,
59.21274,
59.419926,
57.771588,
56.684675,
65.842077,
59.379873,
60.481839,
58.355214,
56.552777,
62.445669,
55.552503,
59.607798,
59.846658)

actual_coordinates <- as.data.frame(cbind(lon_actual,lat_actual))

# getting the map
solution_map <- get_map(location = c(lon = mean(mean(solution_coordinates$lon),
                                                mean(actual_coordinates$lon_actual)),
                                     lat = mean(mean(solution_coordinates$lat),
                                                mean(actual_coordinates$lat_actual))), 
                        zoom = 5, scale = 2, maptype = "terrain")

# plotting the map with solution points on it.


solution_map <- get_map(location = c(min(c(solution_coordinates$lon,actual_coordinates$lon_actual)-1),
                                      min(c(solution_coordinates$lat,actual_coordinates$lat_actual)-1),
                                      max(c(solution_coordinates$lon,actual_coordinates$lon_actual)+1),
                                      max(c(solution_coordinates$lat,actual_coordinates$lat_actual)+1)),
                        zoom = 5, scale = 2, maptype = "terrain")

ggmap2 <- ggmap(solution_map) + geom_point(data = solution_coordinates, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)

# plotting the map with actual points on it in blue.
ggmap3 <- ggmap2 + geom_point(data = actual_coordinates, aes(x = lon_actual, y = lat_actual, fill = "blue", alpha = 0.8), size = 5, shape = 21)
ggmap3

(1.718*(10^26))*(4/3)