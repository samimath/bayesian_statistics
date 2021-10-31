graphics.off()
# remove existing environments
rm(list=ls(all=TRUE))
fileNameRoot="BernMetrop" # for output filenames
#load in pre-defined utility functions
source("../project/DBDA2E-utilities.R")

# Specify the data, to be used in the likelihood function.
myData = c(rep(0,6),rep(1,14))

# Define the Bernoulli likelihood function, p(D|theta).
# The argument theta could be a vector, not just a scalar.
likelihood = function( theta , data ) {
  z = sum( data )
  N = length( data )
  pDataGivenTheta = theta^z * (1-theta)^(N-z)
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The likelihood for theta > 1 or for theta < 0 is zero:
  pDataGivenTheta[ theta > 1 | theta < 0 ] = 0
  return( pDataGivenTheta )
}

# Define the prior density function. 
prior = function( theta ) {
  pTheta = dbeta( theta , 1 , 1 )
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The prior for theta > 1 or for theta < 0 is zero:
  pTheta[ theta > 1 | theta < 0 ] = 0
  return( pTheta )
}

# Define the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.
targetRelProb = function( theta , data ) {
  targetRelProb =  likelihood( theta , data ) * prior( theta )
  return( targetRelProb )
}

# Specify the length of the trajectory, i.e., the number of jumps to try:
trajLength = 50000 # arbitrary large number
# Initialize the vector that will store the results:
trajectory = rep( 0 , trajLength )
# Specify where to start the trajectory:
trajectory[1] = 0.01 # arbitrary value
# Specify the burn-in period:
burnIn = ceiling( 0.0 * trajLength ) # arbitrary number, less than trajLength
# Initialize accepted, rejected counters, just to monitor performance:
nAccepted = 0
nRejected = 0

# Now generate the random walk. The 't' index is time or trial in the walk.
# Specify seed to reproduce same random walk:
set.seed(47405)
# Specify standard deviation of proposal distribution:
proposalSD = c(0.02,0.2,2.0)[2]
for ( t in 1:(trajLength-1) ) {
  currentPosition = trajectory[t]
  # Use the proposal distribution to generate a proposed jump.
  proposedJump = rnorm( 1 , mean=0 , sd=proposalSD )
  # Compute the probability of accepting the proposed jump.
  probAccept = min( 1,
                    targetRelProb( currentPosition + proposedJump , myData )
                    / targetRelProb( currentPosition , myData ) )
  # Generate a random uniform value from the interval [0,1] to
  # decide whether or not to accept the proposed jump.
  if ( runif(1) < probAccept ) {
    # accept the proposed jump
    trajectory[ t+1 ] = currentPosition + proposedJump
    # increment the accepted counter, just to monitor performance
    if ( t > burnIn ) { nAccepted = nAccepted + 1 }
  } else {
    # reject the proposed jump, stay at current position
    trajectory[ t+1 ] = currentPosition
    # increment the rejected counter, just to monitor performance
    if ( t > burnIn ) { nRejected = nRejected + 1 }
  }
}

# Extract the post-burnIn portion of the trajectory.
acceptedTraj = trajectory[ (burnIn+1) : length(trajectory) ]
#openGraph(height=7,width=3.5)
# set matrix size to allocate figures
layout(matrix(1:2,nrow=2))
# call acf (auto-correlation function) for the trajectory of theta from the Markov chain
acf( acceptedTraj , lag.max=30 , col="skyblue" , lwd=3 )
# define length of the chain
Len = length( acceptedTraj )
# set lag size
Lag = 10
# set sequence of accepted trajectories to visualize the following pattern: (\theta_{10i)} vs theta_{i}, i = 1,2,3,...size of the chain)
trajHead = acceptedTraj[  1      : (Len-Lag) ]
trajTail = acceptedTraj[ (1+Lag) :  Len      ]

plot( trajHead , trajTail , pch="." , col="skyblue" ,
      main=bquote( list( "Prpsl.SD" == .(proposalSD) ,
                         lag == .(Lag) ,
                         cor == .(round(cor(trajHead,trajTail),3)))) )


