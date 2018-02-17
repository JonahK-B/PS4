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

##Part 1
setClass(Class = "door",
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c()
         )
)
setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
}
)
setValidity("door", function(x){
  test1 <- (x@chosenDoor == 1 | x@chosenDoor == 2 | x@chosenDoor == 3)
  test2 <- (x@carDoor == 1 | x@carDoor == 2 | x@carDoor == 3)
  test3 <- (x@switch == TRUE | x@switch == FALSE)
  test4 <- (length(x@chosenDoor) == 1 & length(x@carDoor) == 1)
  if(!test1){return("@chosenDoor is not a valid value, must be 1, 2, or 3")}
  if(!test2){return("@carDoor is not a valid value, must be 1, 2, or 3")}
  if(!test3){return("Switch must be TRUE or FALSE")}
  if(!test4){return("Doors must be a single number")}
}
)

testDoor <- new("door", chosenDoor = 2, carDoor = 1, switch = TRUE)

testDoorFake <- new("door", chosenDoor = c(2,3), carDoor = 1, switch = TRUE)

##Part 2

setGeneric("PlayGame", 
           function(object) { 
             standardGeneric("PlayGame") 
           } ) 
setMethod("PlayGame", "door", 
          function(object){ 
            object@carDoor <- sample(1:3, 1)
            if(object@switch){
              x <- c(1,2,3)
              firstChoice <- sample(1:3, 1)
              x <- x[x != firstChoice]
              x <- x[x != object@carDoor]
              object@chosenDoor <- sample(x, 1)
            }
            else{
              object@chosenDoor <- sample(1:3, 1)
            }
        winner = (object@carDoor == object@chosenDoor)
        return(winner)
          } )

PlayGame(testDoor)

##Simulation

## Without switching

