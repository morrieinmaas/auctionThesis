library(plyr)
require(reshape)
library(qgraph)
library(igraph)
library(rgexf)
library(stringr)
library(chron)
library(phonTools)
library(tictoc)
library(pracma)
library(rje)
library(knitr)
library(abind)


multiNewAuction <- function(evaluationMatrix, rounds, bidsteps){
  
  
  if(missing(rounds)){
    rounds <- 20
  }else{
    n <- rounds
  }
  if(missing(bidsteps)){
    bidsteps <- 1
  }
  
  #allocate some space (init empty matrix) to store results of each auction
  #entry[i,j] is a number corresponding to the airline, e.g. "1" is the first airline
  #when listing all airlines alphabetically
  winnerOv <- matrix(0, nrow = rounds, ncol = ncol(evaluationMatrix))
  priceOv <- matrix(0, nrow = rounds, ncol = ncol(evaluationMatrix))
  m <- as.integer(1)
  while (m<=rounds){
    #find inidices of 
    #get all airlines and corresponding item obtained in an auction
    aucres <- newauction(evaluations = evaluationMatrix, bidsteps = bidsteps)
    #get the corresponding max. bids for these items
    wins <- (aucres[,2])
    bidsmax <- (aucres[,1])
    #get the slots that got auctioned
    #slotobt <- wins[which(!wins==0)]
    #get all winners of the slots
    #winners <- col(t(as.matrix(wins)))[which(!wins == 0)]
    #place the winners into slot they obtained
    winnerOv[m, ] <- (wins)
    priceOv[m,] <- (bidsmax)
    #get all highest bids
    #bidsmax2 <- bidsmax[which(!bidsmax==0)]
    #update the evaluation matrix s.t. you subtract the price paid from max evaluation
    #assume that, if an ailine already has multiple activities in  slot it favors to keep that
    #eM[winners,slotobt] = eM[winners,slotobt] - bidsmax2
    slots <- c(1:ncol(evaluationMatrix))
    #if airline has obtained slot -> set evaluation to 0
    #winnerNoNA <- which(!is.na(aucres[,2]))
    aucres[,2][is.na(aucres[,2])] <- 1
    evaluationMatrix[aucres[,2],slots] <- 0
    
    #increase towards rounds
    m = m + 1 
  }
  #(un)comment following line in case you want the winners returned
  #with the abbreviated airline name instead of corresponding number and replace winOvNames with winnerOv
  #alNames <- t(as.matrix(rownames(evaluationMatrix)))
  #winnerOv <- mapvalues(winnerOv, from = c(1:nrow(evaluationMatrix)), to = alNames, warn_missing = FALSE)
  return(winnerOv)
  
}