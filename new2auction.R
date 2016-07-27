newauction <- function(evaluations, #an m x n matrix; rows as bidders;cols as items;entrie i j the evaluation
                       bidsteps,    #the amount to increase by every round of the auction
                       iterations){ #the number of rounds; default inf
  
  #install.packages("foreach",repos="http://cran.rstudio.com/")
  #library("abind")
  #library("iterations")
  #library("foreach")
  #library("parallel")
  #library("doParallel")
  #library("flexclust")
  #library("clue")
  #library("cluster")
  #library("fields")
  #library("cluster")
  #library("pracma")
  #library("Matrix")
  #library("plyr")
  
  #store the evaluation matrix
  x <- evaluations
  
  if (missing(bidsteps)) {
    bidsteps <- 1
  }
  
  if (missing(iterations)){
    #find the max entry in the evaluation matrix
    h_ind <- which.max(x)
    h_eval <- x[row(x)[h_ind], col(x)[h_ind]]
    #set the no. of iterations to the amount of time it takes to reach the max entry in bisteps
    #step size plus an extra two
    iterations <- as.integer(h_eval/bidsteps) + 2
  }
  
  #message("newauction:compareeval created")
  
  replIdent <- function(a){
    a <- a/10  
    indRep <-  which(duplicated(a)==TRUE)
    a[indRep] <- round(a[indRep])  
    return(a*10)
  }
  
  x <- apply(x, 2, function(a) replIdent(a))
  duplicates <- duplicated(x)
  #message("duplicates: ", duplicates)
  #create an empty copy of the auction matrix 
  compareEval <- matrix(0, nrow = nrow(x),ncol = ncol(x),dimnames = dimnames(x))
  
  aucMatrix <- abind(x,compareEval,along = 3)
  
  checkWins <- function(b){
    !length(which(b[,2] == max(b[,2]), arr.ind = TRUE)) <= 1
  }
  
  raiseBid <- function(b){
    apply(b, 1, function(a) if(a[2]+bidsteps <= a[1]){a[2] <- a[2]+bidsteps}else{a[2] <- a[2]})
  }
  
  checkWinners <- function(b){
    if(length(b)>1){
      return(1)
    }else{
      return(0)
    }
  }
  
  m <- iterations
  while (m > 0){
    #message("newauction:while loop ", m)
    continueAuc <- 0
    aucMatrix[,,2] <- apply(aucMatrix, 2, function(a) if(checkWins(a)){a <- raiseBid(a)}else{a[,2]<-a[,2]})
    winners <- apply(aucMatrix, 2, function(a) if(sum(a[,2])!= 0){which(a[,2] == max(a[,2]),arr.ind = T)}else{NA})
    if(m == 1){
      continueAuc <- lapply(winners, function(a) checkWinners(a))
      #message("typeof: ",typeof(continueAuc))
      #message("dim: ",dim(continueAuc))
      #message("sum: ",sum(as.matrix(unlist(continueAuc))))
    }
    if (sum(as.matrix(unlist(continueAuc))) >= 1){
      m <- m+20
      bidsteps <- bidsteps/2
      message("halving bidsteps: ", bidsteps)
      if (bidsteps < 0.1){
        m <- 0
      }
    }else{
      m <- m - 1
    }
  }
  
  SafetyCheck <- function(b){
    if(length(b)>1){
      message("found this fucker!")
      b <- 0
    }else{
      b <- b
    }
    return(b)
  }
  
  #message("winnersbeforeSafety: ",winners)
  #message("winnersbeforeIf: ", winners)
  if(typeof(winners) == "list"){
    #message("winnersbeforeSafety: ", winners)
    winners <- lapply(winners, function(a) SafetyCheck(a))
    winners <- data.matrix(as.matrix(unlist(winners)))
    #message("winnersAfterSafety: ", winners)
    #prmatrix(as.matrix(winners))
  }
  
  #store the highest bidder/winner of item [,j,]
  #winners <- apply(aucMatrix, 2, function(a) if(sum(a[,2])!= 0){which(a[,2] == max(a[,2]),arr.ind = T)}else{NA})
  
  #store the highest bids
  winBids <- apply(aucMatrix, 2, function(a) max(a[,2]))
  
  #return the results
  aucRes <- cbind(winBids,winners)
  
  return(aucRes)
  #stopCluster(cl)
}
