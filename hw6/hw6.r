# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).


initial.doctors=sample((0:1),size=100,replace=TRUE,prob=c(.9,.1))

  sim.doctors<- function(initial.doctors, n.doctors, n.days, p){
    has_adopted=matrix(initial.doctors,nrow=n.doctors, ncol=n.days)
    for (i in 2:n.days){
    
      doc.index=sample(1:n.doctors,size=2,replace=FALSE)
      if ((runif(1)<p)&sum(has_adopted[doc.index,i-1]==1)){
        initial.doctors[doc.index]=1
        has_adopted[,i]=initial.doctors
      }else{
        has_adopted[,i]=has_adopted[,i-1]
      }
    }
    
    return(has_adopted)
  }

a=colSums(sim.doctors(initial.doctors,100,100,.3))
b=colSums(sim.doctors(initial.doctors,100,100,.5))
c=colSums(sim.doctors(initial.doctors,100,100,.7))
d=colSums(sim.doctors(initial.doctors,100,100,.8))
e=colSums(sim.doctors(initial.doctors,100,100,.999))
sumary=data.frame(cbind(1:length(initial.doctors),a,b,c,d,e))
colnames(sumary)= c("day","p=.3","p=.5","p=.7","p=.8","p=.999")
xticks=c(1:100)
yticks=c(1:(max(sumary[,-1])+10))

plot(x=sumary[,1],y=a,xaxt='n',ylim=c(min(yticks),max(yticks)),col="red", type="l", ylab="Number of  Adopters", xlab="Day", main="New Drug Usage Trends Analysis")
axis(side=1,at=xticks)
lines(x=sumary[,1],y=b, col="blue")
lines(x=sumary[,1],y=c, col="green")
lines(x=sumary[,1],y=d, col="orange")
lines(x=sumary[,1],y=e, col="black")
legend("topleft",legend=names(sumary[,-1]), fill=c("red","blue","green","orange","black"), cex=.3, text.width=4)


# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

