## Given Code
myFunction<-function(doorthing, doorthing2, x){
  doorthing1<-doorthing2<-sample(1:3, 1) 
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE } 
  x 
} 
myFunction(sample(1:3, 1), sample(1:3, 1)) 
# Should return a TRUE if these samples are equal and 
# a false if they are not

##Rewritten, better code

myFunction1 <- function(choice, carLocation){ #
  return(choice == carLocation)
}

myFunction1(1,1)
myFunction1(1,3)


##Real HW

#1
