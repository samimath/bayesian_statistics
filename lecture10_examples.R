library(ggplot2)
library(BEST)
## simulated data for group comparison:

n <- 100
x <- rnorm(n, 0,10)
gp1 <- rep('group 1',n)
gp2 <- rep('group 2',n)
y1 <- x + 2 + rnorm(n,0,3)
y2 <- x + 6 + rnorm(n,0,5)

group <- c(gp1,gp2)

data <- c(y1,y2)

test.data <- data.frame(group = group, data = data)
ggplot2::ggplot(data = test.data,aes(x = data, col = group,fill=group)) + 
  geom_density(alpha = 0.4) 



## examples of t distribution as a function of deg. of freedom:


t_dist <- c()
v_val <- c()

for (v in c(2,5,10,20,100,500,1000,2000)){
  
  t_dist_temp <- rt(n=n,df = v)
  
  v_val_temp <-rep(v,n)
  
  t_dist <- c(t_dist,t_dist_temp)
  
  v_val <-c(v_val,v_val_temp)
}



t_dist_df <-data.frame(df = v_val,value = t_dist)

ggplot(data = t_dist_df,
       aes(x = value, 
           col=as.factor(df))) + 
  geom_density(alpha = 0.5)+facet_wrap(~df,ncol = 3)+xlim(c(-10,10))
