# rjags is a package to build graphical Bayesian models
library(rjags)
# coda is a package to visualize and analyze Markov chains
library(coda)
# utility function from our textbook
source('DBDA2E-utilities.R')



## 1. Data Assembly
## Suppose in this case we have a series of coin flips. We can use rbinom to simulate that:

y <-  rbinom(1,10,0.1)

## create the list object:

Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  y=y,
  Ntotal = Ntotal
)


## defining model string

modelString <-"
model {
  for ( i in 1:Ntotal){
   y[i] ~ dbern( theta )
  
  }
  
  theta ~ dbeta( 1, 1 )

} 

"
writeLines( modelString , con="TEMPmodel.txt" ) # write to file

# set up initial value for theta 
thetaInit = sum(y)/length(y)
# put initial theta in a list object
initsList = list( theta=thetaInit )

## Define jagsModel
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList ,
                        n.chains=3 , n.adapt=500 )


## Alternatively, we can generate a random value for the initial list using the following function:

initsList = function(y) {
  resampledY = sample( y , replace=TRUE )        # resample values from y
  thetaInit = sum(resampledY)/length(resampledY) # compute proportion (MLE)
  thetaInit = 0.001+0.998*thetaInit              # keep away from 0,1
  return( list( theta=thetaInit ) )              # return as a named list
}



jagsModel2 = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList(dataList$y) ,
                        n.chains=3 , n.adapt=500 )


## 2. Generating a Markov chain: After JAGS has created its model, we tell it to run the chains some number of steps to accomplish burn in.  
## recompile model:
update(object = jagsModel2,n.iter = 500)

# in case of error for recompile, use this: jagsModel2$recompile()

codaSamples = rjags::coda.samples( model = jagsModel2 , variable.names=c("theta") ,
                            n.iter=2000 )


### 3. Visualizing the Markov chain


### try out some coda function

### set color vec: 

color_vec <- c('#43A5A4','#4E3ACB','#1BAF8B','#2E5458','#2FC44F','#10595D','#0DCADD')

### cumulative quantle plot


coda::cumuplot(codaSamples[[1]],col = color_vec,probs = c(0.1,0.5,0.9))
  



### traceplot:

traceplot(codaSamples[,'theta'],col = color_vec)


### densty plot:

coda::densplot(codaSamples,col='blue')


### auto-correlation function plot: shows serial correlation of theta over time

coda::acfplot(codaSamples,col=color_vec,ylim=c(-0.1,1.1))


coda::autocorr.plot(codaSamples)
