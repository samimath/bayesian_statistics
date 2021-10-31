# Load the functions genMCMC, smryMCMC, and plotMCMC:
source("/cloud/project/homework assignments/hw2_mcmc_generation_jags.R")
# generate coin flip simulation with number of samples, trial size of each sample,  and probability of head

## initial example:


p <-0.1

n <- 10

myData <-  rbinom(n,1,p)


print(sum(myData))

mcmcCoda = genMCMC( data=myData , numSavedSteps=10000, a = 30, b =50 )

# Display diagnostics of chain, for specified parameter:
diagMCMC( mcmcCoda , parName="theta" )

# Display numerical summary statistics of chain:
chain_summary <-  smryMCMC( mcmcCoda )



myData <-list()
mcmcCoda <- list()
chain_diagnostic <-list()
chain_summary <-list()
i <-1

set.seed(2021)

# set different data size for the likelihood function
data_size <- c(2,10,100,1000,5000)

for (n in data_size){
  
  print(paste('n = ',n))

  myData[[i]] <-  rbinom(n,1,p)
 
  mcmcCoda[[i]] = genMCMC( data=myData[[i]] , numSavedSteps=10000, a = 3, b =5 )
  
  # Display diagnostics of chain, for specified parameter:
  diagMCMC( mcmcCoda[[i]] , parName="theta" )
  
  # Display numerical summary statistics of chain:
  chain_summary0 <- as.data.frame(smryMCMC( mcmcCoda[[i]] ))
  
  chain_summary0[,'likelihood.n']<-data_size[i]
  chain_summary<-  rbind(chain_summary,chain_summary0)
  i <- i+1
  print(paste('i = ', i))
  

}




test_summary<-data.frame()
for (i in (1:length(data_size))){
  print(paste('Data size for likelihood = ', data_size[i]))
  test_summary0 <-as.data.frame(t(summary(mcmcCoda[[i]])$statistics))
  test_summary0[,'likelihood.size']<-data_size[i]
  test_summary<-rbind(test_summary,test_summary0)

}