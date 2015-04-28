xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}


genBootY = function(x, y, rep = TRUE){
# ux=unique(x)
# yvals=list()
# for (i in 1:length(unique(x))){
#   index=grep(ux[i],x)
#   sampley=y[index]
#   a=length(index)
#   yvals[[i]]=sample(x=sampley,size=a,rep=TRUE)
# }
# return(unlist(yvals))
# }
little.sample=tapply(y,x,function(hola) sample(hola,length(hola),replace=rep))
hola=unlist(little.sample,use.names=FALSE)
}



genBootR = function(fit, err, rep = TRUE){
  fit.with.errors=c(rep(0,length(fit)))
  sample.errors=sample(err,length(err),rep=rep)
  for (i in 1:length(fit)){
  fit.with.errors[i]=fit[i]+sample.errors[i]
  }
  return(fit.with.errors)
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
}




fitModel = function(x, y, degree = 1){
  if(degree==1){
    hola=lm(y~x)
    coeff=hola$coefficients
  }
  if(degree==2){
    coeff=lm(y~x+I(x^2))$coefficients
  }
  return(coeff)
}
  
  



oneBoot = function(data, fit = NULL, degree = 1){
  if(is.null(fit)){
    ynew=genBootY(data[,1],data[,2])
  }else{ynew=genBootR(fit[,1],fit[,2])}
  fitModel(data[,1],y=ynew,degree)
}


  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
 


#repBoot = function(data, B = 1000){
#   empty.list=list(rep(0,1000),rep(0,1000),rep(0,1000),rep(0,1000))
#   for (i in 1:B){
#     empty.list[[i]]=oneBoot(data)
#   }
# return(empty.list)
#}


#repBoot=function(data,B=1000){
  #lista=list()
  #y.values.Y.1=genBootY(data[,1],data[,2])
 # fit.model.Y.1=fitModel(data[,1],y.valuesY.1, degree=1)
  
#   y.values.R.1=genBootR(data[,1],data[,2])
#   fit.model.R.1=fitModel(data[,1],y.valuesR.1, degree=1)
#   
#   y.values.Y.2=genBootY(data[,1],data[,2])
#   fit.model.Y.2=fitModel(data[,1],y.valuesY.2, degree=2)
#   
#   y.values.R.2=genBootR(data[,1],data[,2])
#   fit.model.R.2=fitModel(data[,1],y.valuesR.2, degree=2)
  
#}


repBoot=function(data,B=1000){
  lista=list()
  matriz.1=matrix(0,ncol=2,nrow=B)
  matriz.2=matrix(0,ncol=3,nrow=B)
  matriz.3=matrix(0,ncol=2,nrow=B)
  matriz.4=matrix(0,ncol=3,nrow=B)

  
  mfit.1=lm(data[,2]~data[,1])
  mfit.2=lm(data[,2]~data[,1]+I(data[,1]^2))
  fit.1=cbind(mfit.1$fitted,mfit.1$residuals)
  fit.2=cbind(mfit.2$fitted,mfit.2$residuals)
  for (i in 1:B){
  matriz.1[i,]=matrix(oneBoot(data,fit=NULL,degree=1))
  matriz.2[i,]=matrix(oneBoot(data,fit=NULL,degree=2))
  matriz.3[i,]=matrix(oneBoot(data,fit=fit.1,degree=1))
  matriz.4[i,]=matrix(oneBoot(data,fit=fit.2,degree=2))
}
return(list(matriz.1,matriz.2,matriz.3,matriz.4))
}





#Poner atention a esto!!
  #you need four cases: genBootR (residual) with degree 1, genBootR (residual) with degree 2, genBotY (cases) with degree 1, genBootY (cases) with degree 2
#mfit.1=lm(data$y~data$x)
#fit.1=cbind(mfit.1$fitted,mfit.1$residual)
### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  plot(x,y)
  hello=nrow(coeff)
  if(ncol(coeff)==2){
    for (i in 1:hello){
      abline(a=coeff[i,1],b=coeff[i,2],col="#0000001A")
      
  }}else{
    for (i in 1:hello){
      curve(coeff[i,1]+coeff[i,2]*x+coeff[i,3]*x^2,add=TRUE,col="#0000001A")
    }
  }
curve(trueCoeff[1]+trueCoeff[2]*x+trueCoeff[3]*x^2,add=TRUE, col="red")
}
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out



### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}