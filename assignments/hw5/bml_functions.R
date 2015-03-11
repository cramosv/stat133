#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m=matrix(data=sample(c(0,1,2),size=r*c,replace=TRUE,prob=c(1-p,p/2,p/2)),nrow=r,ncol=c)
  return(m)  
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step=function(m){
  s=m
  rmstar=cbind(m[,-1],m[,1])
  rmovers=(m==1)&(rmstar==0)
  rnon.movers=(m==1)&(rmstar!=0)
  rmoved.cars=cbind(rmovers[,ncol(m)],rmovers[,1:(ncol(m)-1)])
  
  m=m*(m==2)+m*rnon.movers+rmoved.cars
  
  bmstar=rbind(m[nrow(m),],m[1:(nrow(m)-1),])
  bmovers=(m==2)&(bmstar==0)
  bnon.movers=(m==2)&(bmstar!=0)
  bmoved.cars=rbind(bmovers[2:(nrow(m)),],bmovers[1,])
  
  m=m*(m==1)+m*bnon.movers+2*bmoved.cars
  
  grid.new=!identical(s,m)
  
  return(list(m,grid.new))
}
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)
bml.sim <- function(r, c, p){
  sim=bml.init(r,c,p)
  steps=0
  for(i in 1:10000){
    if (bml.step(sim)[[2]]==TRUE){
      sim= bml.step(sim)[[1]]
      steps=steps+1}
    else {
      break
    }
  }
  return(list(bml.step(sim)[[2]],steps))
}



