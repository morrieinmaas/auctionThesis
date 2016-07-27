#perform multiple vickrey auctions
#
#from Vickrey auction we want alEval <- vickResOfRound$alEval

multiVickrey <- function(alEval, rounds){
  
  #if (!missing(vickResOfRound$alEval)){
  #alEval <- vickResOfRound$alEval  
  #}
  #else 
  if (missing(alEval)){
    alEval <- matrix(sample.int(100, 25*25, TRUE), 25, 25)
  }
  #}else{
  #  alEval <- vickResOfRound$alEvalVic
  #}
  if(missing(rounds)){
    rounds <- 10
  }
    #allocate some space to store winner of slot j in auction i in winnerOv[i,j]
    winnerOv <- matrix(0, nrow = rounds, ncol = ncol(alEval))
    m <- as.integer(1)
  
    while (m <= rounds){
      #perform and get results froma first Vickrey auction store them in alist
      alist <- vickreyAuction(alEval)
      #store the winners
      winner <- as.matrix(alist$winners[1,])
      #the corresponding items
      ofItems <- as.matrix(alist$winners[2,])
      #the overview matrix
      winnerOv[m, ofItems] = winner 
      #update the evaluation matrix for next round (does not work that well yet)
      #more rounds with results would be nice -> make bids below max?!
      alEval <- alist$alEvalVic
      m = m +1
    }
    #the airline names
    #alNames <- t(as.matrix(rownames(alEval)))
    #map airline names to the winnerOfSlot/round overview
   # winOvNames <- mapvalues(winnerOv, from = c(1:nrow(alEval)), to = alNames)
    return(winnerOv)

}