#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

simulation.iteration=function(r,c){
  densities=c(1:20)
  densities=(5*pvalues)/100
  matrixx=matrix(data=NA, nrow=100,ncol=20)
  colnames(matrixx)=densities
  for (i in 1:20){
    p=(5*i)/100
    for(j in 1:100){
      matrixx[j,i]=bml.sim(r,c,p)[[2]]
    }}
  
  return(matrixx)
}


average.gridlocks=function(x){
  average.v=c(1:ncol(x))
  for(i in 1:ncol(x))
  {average.v[i]=mean(x[,i])}
  return(average.v)
}


####function for before and after images: 

##FIRST STORE INITIAL  
initial=bml.init(r,c,p)  
image.trial=image(initial, col=c("white","red","blue"))
##THEN RUN CODE TO SEE HOW INITAL ENDS UP     
bml.sim.images <- function(sim){
  for(i in 1:10000){
    if (bml.step(sim)[[2]]==TRUE){
      sim= bml.step(sim)[[1]]
    }
    else {
      break
    }
  }
  return(image(sim, col=c("white","red","blue")))
}
