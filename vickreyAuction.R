#A simple Vickrey auction for an airline evaluation matrix
#a single round auction, where the highhest bidders obtains
#an item for the second highest bid
#
#as it turns out, bidding on the max evaluation of a slot results in
#no more submitted bids after very few rounds -> different updating fct. for evaluation 
#and bid with less then max eval?!

vickreyAuction <- function(alEval,      #airline slot evaluation matrix
                           output)      #if TRUE (default) prints each round and results to console  
{
  #set some defaults for missing inputs
  if (missing(alEval)){
    alEval <- matrix(sample.int(100, 25*25, TRUE), 25, 25)
  }
  if (missing(output)){
    output <- TRUE
  }
  #init some vectors to store winners and bids
  #queue to determine whos turn it is to bid
  bids =  zeros(1,nrow(alEval))
  wins =  zeros(1,nrow(alEval)) 
  
  #find highest value of column: we have a winner
  #a <- apply(alEval,2,max) -> gives dim=NULL
  aa <- matrix((apply(alEval,2,max)),nrow=1,ncol=ncol(alEval))
  #find the highest value/col and replace by 0
  b <- alEval
  b[which(apply(b, 2, function(x) x == max(x,na.rm=TRUE)))] <- 0 
  #b[alEval == apply(alEval,2,max)] <- 0
  #find the (now, former 2nd) highest value/column, which is actually the second highest: 
  #we have a price
  bc <- matrix((apply(b,2,max)),nrow=1,ncol=ncol(b))
  #max. evaluation minus price paid gives remaining willingness to obtain slot (for a simple start)
  cc <- aa-bc
  #replace in alEval to update to new evaluation matrix
  alEval2 <- apply(alEval, 2, FUN = function(x) mapvalues(x, from = aa, to = cc,warn_missing = FALSE))
  #indices of winners
  wininds <- which(alEval!=alEval2, arr.ind = TRUE)
  #w <- which( a==b ,arr.ind = TRUE)
  vickResOfRound <- list(alEvalVic = alEval2, prpaid = aa, winners = t(wininds))
  return(vickResOfRound)  

}