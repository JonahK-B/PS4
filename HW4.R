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

myFunction1 <- function(choice, carLocation){ #Input door choice and location of car
  return(choice == carLocation) #return true if they are equivelent, false if not
}

myFunction1(1,1) ##True
myFunction1(1,3) ##False


##Real HW

##Part 1

##Makes a door class (really a description of the monty hall player's situation) with slots for the player's chosen door, where the car is, and whether the player will switch doors
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
  
 ## Only allows the doors to be 1, 2, or 3
  test1 <- (x@chosenDoor == 1 | x@chosenDoor == 2 | x@chosenDoor == 3)
  test2 <- (x@carDoor == 1 | x@carDoor == 2 | x@carDoor == 3)
  test3 <- (x@switch == TRUE | x@switch == FALSE)
##Only allows one chosen door and one car
  test4 <- (length(x@chosenDoor) == 1 & length(x@carDoor) == 1)
  if(!test1){return("@chosenDoor is not a valid value, must be 1, 2, or 3")}
  if(!test2){return("@carDoor is not a valid value, must be 1, 2, or 3")}
  if(!test3){return("Switch must be TRUE or FALSE")}
  if(!test4){return("Doors must be a single number")}
}
)

##working door
testDoor <- new("door", chosenDoor = 2, carDoor = 1, switch = TRUE)

##not working door
testDoorFake <- new("door", chosenDoor = c(2,3), carDoor = 1, switch = TRUE)

##Part 2
##Implements the generic function "PlayGame"
setGeneric("PlayGame", 
           function(object) { 
             standardGeneric("PlayGame") 
           } ) 

##Implements "door" method for "PlayGame"
setMethod("PlayGame", "door", 
          function(object){                   ##The only attribute of the inputted door which is important is whether or not the player switches
            object@carDoor <- sample(1:3, 1)  ## Sets the car to be behind door 1, 2, or 3 at random
            if(object@switch){                ## If the Door player switches, do the following
              options <- c(1,2,3)             ## "options" are the possible doors to be revealed
              firstChoice <- sample(1:3, 1)   ## Sets the players initial door choice at random
              options <- options[options != firstChoice] ## removes players initial pick from options
              options <- options[options != object@carDoor] ## removes the door that the car is behind from options
                                                            ##The two if statements set the opened door, depending on if the initial guess was correct or not
              if(length(options)== 2){ 
                openedDoor = as.numeric(sample(c(options),1))
              }
              if(length(options)==1){
                openedDoor = as.numeric(options)
              object@chosenDoor <- c(1,2,3)[-c(firstChoice, openedDoor)] ## The player's final chosen door is the door that was not originally picked, and not the opened door
              }
            }
            else{ 
              object@chosenDoor <- sample(1:3, 1) ##If the player does not switch, then the choice is simply picked at random
            }
        winner = (object@carDoor == object@chosenDoor) ## Sets winner to true if the player's choice matches the car, otherwise false
        return(winner)
          } )

PlayGame(testDoor)

##Simulation

## Without switching

DumbGamePlay <- new("door", chosenDoor = 1, carDoor = 1, switch = FALSE) ##Creates a door where the player does not switch
BunchaIdiots <- rep(c(DumbGamePlay), 1000) ## Makes a list of 1000 of these doors
IdiotsPlayingGames <- sapply(BunchaIdiots, PlayGame) ##Makes all of them play the game
table(IdiotsPlayingGames) ## Returns how many won and lost (TRUE and FALSE)

## With switching

SmartGamePlay <- new("door", chosenDoor = 1, carDoor = 1, switch = TRUE) ##Creates a door where the player does switch
BunchaGameTheorists <- rep(c(SmartGamePlay), 1000) ## Makes a list of 1000 of these doors
GameTheoristsPlayingGames <- sapply(BunchaGameTheorists, PlayGame)##Makes all of them play the game
table(GameTheoristsPlayingGames)## Returns how many won and lost (TRUE and FALSE)
