# GENETIC ALGORITHM

install.packages("Matrix")
install.packages("pracma")
install.packages("cluster")
install.packages("fields")
install.packages("cluster")
install.packages("clue")
install.packages("flexclust")
install.packages("doParallel")
install.packages("parallel")
install.packages("foreach")
install.packages("iterations")
library("abind")
library("iterations")
library("foreach")
library("parallel")
library("doParallel")
library("flexclust")
library("clue")
library("cluster")
library("fields")
library("cluster")
library("pracma")
library("Matrix")
library("plyr")
require(data.table)

auctionGA <- function(x, #evaluationmatrix;  in order to get original dims
                      popSize, #population size
                      crossOver, #where to cross over e.g. mid length = 0.5
                      survPerc, #percentage of survivors
                      mutRate, #mutation rate
                      bidsteps, #bidsteps for auction
                      iterations #the number of iterations to run
){
  
  #Initialize parallel computation
  
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  #cl <- makePSOCKcluster(c(rep("localhost",3),rep("momo@192.168.0.33",3)), master="192.168.0.32", port=1234, manual=TRUE)
  # rshcmd="plink"
  clusterEvalQ(cl, library(foreach))
  clusterEvalQ(cl, library(parallel))
  clusterEvalQ(cl, library(doParallel))
  clusterEvalQ(cl, library(flexclust))
  clusterEvalQ(cl, library(clue))
  clusterEvalQ(cl, library(cluster))
  clusterEvalQ(cl, library(fields))
  clusterEvalQ(cl, library(pracma))
  clusterEvalQ(cl, library(Matrix))
  clusterEvalQ(cl, library(plyr))
  clusterEvalQ(cl, library(abind))
  #clusterEvalQ(cl, "newAuction.R")
  #clusterEvalQ(cl, "geneticAuction.R")
  
  
  # —---------------—
  # GENERATE POPULATION
  
  dimEval <- dim(x) #find dimensions of original evaluation matrix
  #message("Original: ")
  #prmatrix(x)
  #create population of random evaluation matrices with random numbers in range min/max original evals
  matrixstorage <- list() # initialize matrixstorage as empty list
  
  for (i in 1:popSize){
    matrixstorage[[i]] <- matrix(sample(min(x):max(x),size = dimEval[1]*dimEval[2], replace = T), nrow = dimEval[1], ncol = dimEval[2])
    
    # copy row and column names
    colnames(matrixstorage[[i]]) <- colnames(x)
    rownames(matrixstorage[[i]]) <- rownames(x)
    
    #Count the number of zero entries in the initial evaluation matrix 
    amountZero <- sum(colSums(x == 0))
    
    #replace right number of zeros from the original for each pop evalaution matrix 
    for (j in 1:amountZero){
      randRow <- sample.int(nrow(x),1,replace = T)
      randCol <- sample.int(ncol(x),1,replace = T)
      if (matrixstorage[[i]][randRow,randCol] != 0) {
        matrixstorage[[i]][randRow,randCol] <- 0
      }else{
        j <- j -1
      }
    }
  }
  message("First random: ")
  #prmatrix(matrixstorage[[1]])
  
  while(iterations > 0){ #iterate as often as the input dictates
    message("Iteration", iterations)
    
    # —---------------—
    # SELECTION
    
    
    #fitness list to store all fitness values
    fitnessList <- list()
    
    message("fitnessList created")
    
    genAuctionFit <- function(x, #original eval matrix
                              evals,   #evaluationmatrix
                              bidsteps,#the amount to raise the bid every round
                              iterations){ 
      
      #evals <- matrix(evals, nrow = 11, ncol = 16)
      #count the number of items a bidder already owns/fancies
      #if (!is.null(dim(evals))){
      itemsNo <- apply(x, 1, function(a) nnzero(a))
      
      #get the total value of all evaluated items per bidder
      evalSum <- apply(x, 1, function(a) sum(a))
      
      #list all slots owned/used previously
      slotsUsed <- apply(x, 1, function(a) which(a!=0))
      #}
      #combine the above to an overview; we can later assign weights to each of the rows as well
      overvEval <- rbind(itemsNo, evalSum, slotsUsed)
      
      message("overveval created")
      
      #get the auciton results
      aucRes <- newauction(evaluations = evals, bidsteps = bidsteps)
      alnames <- t(colnames(overvEval))
      aucRes[,2] <- mapvalues(aucRes[,2], 1:length(alnames), alnames, warn_missing = F)
      
      message("newauction done")
      
      #initialize empty matrix same as overvEval
      overvAuc <- replace(overvEval,1:length(overvEval),0)
      
      #find all items an airline has obtained and count the amount
      itemsObt <- lapply(unique(aucRes[,2]),function(a) which(aucRes[,2] %in% a))
      message("typeof(aucRes)", typeof(aucRes))
      message("aucRes", aucRes)
      
      
      #count the number of items obtained per airline
      itemsNoObt <- as.data.frame(table(aucRes[,2]))
      
      #find the amount of money an airline has spent in an auction #  aggregate(aucRes[,2] ~ aucRes[,1] , data = aucRes, sum)
      aucSum <- as.data.frame(aggregate(as.integer(aucRes[,1]) ~ aucRes[,2], data = aucRes,sum))
      
      repSlot <- function(a){
        b <- match(as.character(a[,1]),colnames(overvAuc))
        replace(as.data.frame(overvAuc[1,]),as.data.frame(overvAuc[1,b]),as.integer(a[,2]))
      }
      
      #overvAuc[1,] <- apply(itemsNoObt,1, function(a) repSlot(a))
      
      for (i in 1:length(overvAuc[1,])){
        overvAuc[1,which(colnames(overvAuc)==itemsNoObt[,1][i])] <- itemsNoObt[i,2]
        
        overvAuc[2,which(colnames(overvAuc)==aucSum[,1][i])] <- aucSum[i,2]
        
      }
      
      for (i in 1:length(which(overvAuc[2,]!=0))){
        overvAuc[3,which(overvAuc[2,]!=0)[i]] <- itemsObt[i]
      }
      
      for (i in 1:length(unique(aucRes[,2]))){
        overvAuc[3,which(colnames(overvAuc)==unique(aucRes[,2])[i])] <- itemsObt[i]
      }
      
      
      #combine the initial evaluation and the auction results in a 3d array to put in fitness function
      #toEvaluate <- abind(overvEval,overvAuc, along = 3)  
      
      #find the minimum distance between the sets of slots used and slots obtained
      #this i done by computing the distance matrix and using the hungarian method to
      #find the rearrangment of vector, s.t. the pairwise distance is minimized
      distSlots <- function(a,b){
        a <- a[[1]]
        b <- b[[1]]
        if (length(a)>length(b) && length(a)!=0 && length(b)!=0){
          c <- replace(a,1:length(a),0)
          b <- replace(c,1:length(b),b)
        }else if (length(a)<length(b) && length(b)!=0){
          c <- replace(b,1:length(b),0)
          a <- replace(c,1:length(a),a)
        }
        d <- rbind(a,b)
        
        x <- dist2(d[1,],d[2,])
        y <- solve_LSAP(x)
        mindist <- sum(x[cbind(seq_along(y), y)])
        return(mindist)
      }
      
      #create a fitness function the takes n input matrix and the auction results, which will be overvEval
      #should evaluate s.t. the smaller the fitness the better
      fitness <- function(c){
        if(c[4]==0){
          fitNess <- 1.0
        } else{
          
          noItDiff <- abs(as.integer(c[1])-as.integer(c[4]))
          if(as.integer(c[2]==0)){monDiff <- 1
          } else {
            monDiff <- as.integer(c[2])/as.integer(c[5])
          }
          itemDiff <- distSlots(c[3],c[6])
          fitNess <- (1/3 * (1-(1/(2^noItDiff))) + 1/3 * monDiff + 1/3 * itemDiff/sum(1:length(evals[1,])))
        }
        return(fitNess)
      }
      
      fitNess <- matrix(0,nrow=1,ncol=length(overvEval[1,]))
      
      evFit <- rbind(overvAuc,overvEval) 
      evFit <- replace(evFit,integer(0),0)
      
      fitNess <-apply(evFit, 2, function(a) fitness(a))  
      avgFitness <- sum(fitNess)/length(fitNess)
      return(avgFitness)
      
    }
    
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
      
      message("newauction:compareeval created")
      
      replIdent <- function(a){
        a <- a/10  
        indRep <-  which(duplicated(a)==TRUE)
        a[indRep] <- round(a[indRep])  
        return(a*10)
      }
      
      x <- apply(x, 2, function(a) replIdent(a))
      duplicates <- duplicated(x)
      message("duplicates: ", duplicates)
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
          message("typeof: ",typeof(continueAuc))
          message("dim: ",dim(continueAuc))
          message("sum: ",sum(as.matrix(unlist(continueAuc))))
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
      
      message("winnersbeforeSafety: ",winners)
      message("winnersbeforeIf: ", winners)
      if(typeof(winners) == "list"){
        message("winnersbeforeSafety: ", winners)
        winners <- lapply(winners, function(a) SafetyCheck(a))
        winners <- data.matrix(as.matrix(unlist(winners)))
        message("winnersAfterSafety: ", winners)
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
    
    
    #run auctions for all evaluation matrices and calculate fitness values
    #for (i in 1:popSize){
    fitnessList <- parLapply(cl, matrixstorage, function(a) genAuctionFit(x = x, evals = a, bidsteps = bidsteps))
    #}
    #message("fitnessList length: ", length(fitnessList))
    numSurv <- round(survPerc*length(fitnessList)) #determine number of survivors
    #message("numSurv: ", numSurv)
    highestFitness <- sort(head(sort(unlist(fitnessList)),n = numSurv), decreasing = FALSE) #find lowest fitnessvalues and sort them descreasing
    
    if (iterations != 1){ #do not execute in last round
      
      # —---------------—
      # GENERATE MATING POOL
      
      matingPool <- list() # initialize matingPool for reproduction
      prob <- vector(,numSurv) # initialize selection probability vector
      
      for (i in 1:numSurv){
        pos <- which(fitnessList == highestFitness[i], arr.ind = T) # find position of this fitnessvalue in fitnessList
        message("pos: ", pos)
        matingPool[[i]] <- matrixstorage[[pos]] # save corresponding matrix in matingpool
        prob[i] = i/((numSurv*(numSurv+1))/2) # calculate selection probability linearly / higher probability for higher fitness
      }
      
      # —---------------—
      # REPRODUCTION & CROSSOVER
      
      for (i in seq(1,popSize,2)){
        
        parents <- sample(numSurv, size = 2, replace = TRUE, prob) # randomly select two parents
        message("parents1:", parents[1])
        message("parents1:", parents[2])
        crossOverPoint <- round(length(x[1,])*crossOver) # determine crossover point
        # Create first child
        matStorCols <- ncol(matingPool[[as.integer(parents[1])]])
        message(matStorCols)
        matrixstorage[[i]]  <- cbind(matingPool[[as.integer(parents[1])]][,1:crossOverPoint], matingPool[[as.integer(parents[2])]][,(crossOverPoint+1):matStorCols])
        # Create second child
        matrixstorage[[i+1]]  <- cbind(matingPool[[as.integer(parents[2])]][,1:crossOverPoint], matingPool[[as.integer(parents[1])]][,(crossOverPoint+1):matStorCols])
        
        
        # —---------------—
        # MUTATION
        
        # Mutation Decision based on given mutRate
        mutDecision <- function(){
          mutDec <- sample(c(0,1), size = 2, replace = FALSE, prob=c(1-mutRate,mutRate))
          if(mutDec == 1){
            return(TRUE)
          }else{
            return(FALSE)
          }
        }
        
        toMuteorNotToMute <- function(b){
          if(mutDecision == T) {
            b <- sample(min(x):max(x),size = 1,replace = T)
          }else{
            b <- b
          }
        }
        #message("First child before mutation: ")
        #prmatrix(matrixstorage[[i]])
        
        # Make mutation decision for each entry in both childs
        matrixstorage[[i]]  <- apply(matrixstorage[[i]], 1:2,function(a) {mutDec <- sample(c(0,1), size = 2, replace = FALSE, prob=c(1-mutRate,mutRate)) 
        if(mutDec== 1){a <- sample(min(x):max(x),size = 1,replace = T)}else{a <- a}})
        #message("First child after mutation: ")
        #prmatrix(matrixstorage[[i]])
        matrixstorage[[i+1]] <- apply(matrixstorage[[i+1]], 1:2,function(a) {mutDec <- sample(c(0,1), size = 2, replace = FALSE, prob=c(1-mutRate,mutRate)) 
        if(mutDec == 1){a <- sample(min(x):max(x),size = 1,replace = T)}else{a <- a}})
        
      }
    }
    
    iterations <- iterations - 1 #reduce iterations by one
  }
  stopCluster(cl)
  # find evaluation with lowest/best fitness value (no reciprocal this time)
  winFit <- which.min(fitnessList)
  
  winFitEval <- matrixstorage[[winFit]] #retrieve matrix with best fitness
  message("Best fitness score: ", winFit)
  
  return(winFitEval)
}