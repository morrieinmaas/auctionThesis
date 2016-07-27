#install.packages(adply)
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
library(dplyr)

#in order to create evaluation matrices e.g. alEvMatOutHER2010, alEvMatInHER2010 
#change Q to the desired input file (format must be consistent)
#use find/replace to replace all instances of e.g. HER for any desired IATA code for an airport e.g. HER
#replace all instances of e.g. 2010 with the appropriate year
#this will generate evaluation matrices for all outgoing and incoming at that airport for that year 
#replace similarly any "gre" with e.g "gre" for spain

Q=read.csv("~/Documents/FromMac/Thesis/data/EUROPEAUG2010.csv", sep=";", header=TRUE)
#code for airport HER
airport="HER"
outgoing=Q[which(Q[,3]==airport),]
incoming=Q[which(Q[,4]==airport),]
airlinefreqin = length(incoming[[2]])
airlinefreqout = length(outgoing[[2]])

#change Q in airline freqin and out to incoming and outgoing
airlines=ddply(Q,~Op.Al,summarise,number_of_distinct_airlines=length(unique(Op.Al)))

airlinesfreqout=ddply(outgoing,.(Op.Al),summarise,freq_of_distinct_airlines=sum(Ops.Week))
airlinesfreqin=ddply(incoming,.(Op.Al),summarise,freq_of_distinct_airlines=sum(Ops.Week))

#define start time, end time and step
start=chron(times="00:00:00")
end=chron(times="23:59:59")
stepp=chron(times="00:15:00")

#create sequence of slot times
tt <- times(0:95/96)

#check only incoming
slotsarray = array(0,length(seq(start,end,by=stepp)))
slotsarrayAL = array(0,length(seq(start,end,by=stepp)))
count=1

#determines the prices based on the distance with 1 currency unit/km; to change price/distance multiply price_of_flight with factor accordingly
flightpricein = as.data.frame(incoming$Kilometers)  
flightpriceout =  as.data.frame(outgoing$Kilometers) 

#total number of flights (all per week)
totinflight = sum(airlinesfreqin[[2]], na.rm = FALSE, dims=1)
totoutflights = sum(airlinesfreqout[[2]], na.rm = FALSE, dims=1)

#frequncy share as flights per airline over total no of flights; tHERe should be a version with slots per airline over total slots
#as fraction [0,1] -> for percent multiply frequency_share(in/out) with 100
freqsharein = ddply(airlinesfreqin,~Op.Al,summarise, frequency_sharein=freq_of_distinct_airlines/totinflight)
freqshareout = ddply(airlinesfreqout,~Op.Al,summarise, frequency_shareout=freq_of_distinct_airlines/totoutflights)

#convert time into time format incoming
arr_timein = matrix(sub("([[:digit:]]{2,2})$", ":\\1", incoming[[11]]),nrow = length( incoming[[11]]), byrow = TRUE)
dep_timein = matrix(sub("([[:digit:]]{2,2})$", ":\\1", incoming[[10]]),nrow = length( incoming[[10]]), byrow = TRUE)

arr_timeout = matrix(sub("([[:digit:]]{2,2})$", ":\\1", outgoing[[11]]),nrow = length( outgoing[[11]]), byrow = TRUE)
dep_timeout = matrix(sub("([[:digit:]]{2,2})$", ":\\1", outgoing[[10]]),nrow = length( outgoing[[10]]), byrow = TRUE)

#insert seconds as :00 at the end
arr_timein2 = matrix(sub("$", ":00", arr_timein))
dep_timein2 = matrix(sub("$", ":00", dep_timein))
arr_timeout2 = matrix(sub("$", ":00", arr_timeout))
dep_timeout2 = matrix(sub("$", ":00", dep_timeout))

#replace the times in integer as chronological
incoming[11] = chron(times=arr_timein2)
incoming[10] = chron(times=dep_timein2)
outgoing[11] = chron(times=arr_timeout2)
outgoing[10] = chron(times=dep_timeout2)

#marketshare
MSin = ddply(freqsharein,~Op.Al,summarise, market_sharein=frequency_sharein/sum(freqsharein[[2]], na.rm = FALSE, dims=1))
MSout = ddply(freqshareout,~Op.Al,summarise, market_shareout=frequency_shareout/sum(freqshareout[[2]], na.rm = FALSE, dims=1))

timeframes=seq(start,end,by=stepp)

unique_airIn=unique(incoming$Op.Al)
unique_airOut=unique(outgoing$Op.Al)

slotsarrayALin=matrix(0,ncol=length(unique_airIn),nrow=length(slotsarray))
slotsarrayALout=matrix(0,ncol=length(unique_airOut),nrow=length(slotsarray))

colnames(slotsarrayALin)<-unique_airIn
colnames(slotsarrayALout)<-unique_airOut

#lets initialize a 0-entry matrix dim[airline rows, slots cols] 
#storing the count of activity of airline i in slot j in that matrix [i,j]
numAlin <-  length(matrix(unique_airIn)) ##or length(unique(airlines[,1]))
numAlout <-  length(matrix(unique_airOut)) ##or length(unique(airlines[,1]))

numSlots <- length(timeframes)
alfreqdistIn = matrix(0,ncol=length(slotsarray),nrow=length(unique_airIn))
alfreqdistOut = matrix(0,ncol=length(slotsarray),nrow=length(unique_airOut))
alFrDistr <- t(slotsarrayAL)
timeframesString <- format(timeframes, c("h:m:s"), sep = "T", enclose = c(""))

evaluateslotsALin=matrix(0,ncol=length(unique_airIn),nrow=length(slotsarray))
evaluateslotsALout=matrix(0,ncol=length(unique_airOut),nrow=length(slotsarray))

#tabel of flight frequncies
frequencyin <- table(airlinefreqin)
frequencyout <- table(airlinefreqout)
airlinesframe <- data.frame(airlines)

wwAPc <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",header = F)

wwAPcodesIATA <- data.frame(wwAPc$V5,wwAPc$V3,wwAPc$V4)
colnames(wwAPcodesIATA) <- c("Code","City","Country")

#alternatively for EU; also more useful to search for otHER regions
euAirports <- wwAPc[grep("^Europe", wwAPc$V12),]
euAPcodesIATA <- data.frame(euAirports$V5,euAirports$V3,euAirports$V4)
colnames(euAPcodesIATA) <- c("Code","City","Country")

#all united kingdom
greAirports <- wwAPc[grep("United Kingdom", wwAPc$V4),]
greAPcodesIATA <- data.frame(greAirports$V5,greAirports$V3,greAirports$V4)
colnames(greAPcodesIATA) <- c("Code","City","Country")

#all spain
#greAirports <- wwAPc[grep("Spain", wwAPc$V4),]
#greAPcodesIATA <- data.frame(greAirports$V5,greAirports$V3,greAirports$V4)
#colnames(greAPcodesIATA) <- c("Code","City","Country")

# low-cost carrier codes from www.icao.int/sustainability/Documents/LCC-List.pdf
lcc <- c("E5","JX", "3O", "8A", "8J", "T6","VQ","JQ", "TT", "VA", "9C", "UO","O8","ZG","IX", "G8", "6E","IT","SG", "KI", "QG", "QZ", "JT", "RI", "JW", "HD","GK","BC","LQ","7G", "JW", "AK", "D7","MN", "JE", "FN","YM","KC","DJ","NZ","3K","TR","VF","ZE","7C","LJ","TW","MJ","DD","OX","FD","3L","TV","8Z","D5","DI","ST","4U","X3","5P","W6","LZ","GX","OD","Y5","SJ","E4","PA","Z2","5J","2P","PQ","DG","DJ","SL","BL","VJ","HG","QS","NB","KF","SH","TO","DE","HC","WW","RE","VE","FR","TV","VA","5D","HV","5K","0B","XW","NE","XG","V7","VY","DS","F7","2G","U2","BE","Y2","GO","IG","8I","IV","DY","BV","9X","4P","C0","SH","LF","2L","KK","7H","8Q","H9","XQ","LS","AD","7R","G3","WH","H2","EF","O6","JR","C6","HQ","SG","FL","G4","TZ","NM","ZB","JN","C4","6A","V5","4O","ZE","QA","VB","Y4","J9","XY","ZS","G9","FZ","WG","WS","3J","Z4","ZA","WV","W9","F9","YV","DH","B6","KP","ML","YX","N7","NY","PS","PE","P9","QQ","SX","XP","WN","NK","SY","FF","U5","J7","NJ","VX","W7")

#evl with different carrier types; see airportcodes.R
evaluationAlin <- function(x){
  x <- as.data.frame(x)
  #prprftmrgn <- 0.025
  profitmargin <- function(x){
    #if low-cost carrier
    if (is.element(x$Op.Al,lcc)){
      prprftmrgn <- 0.088
    }
    #if gre flight
    else if (is.element(x$Orig,greAPcodesIATA$Code)) {
      prprftmrgn <- (-0.020)
    }
    #if EU but non gre
    else if (is.element(x$Orig,euAPcodesIATA$Code) && !is.element(x$Op.Al,lcc)) {
      prprftmrgn <- (0.042)
    }
    #if non EU 
    else if (!is.element(x$Orig,euAPcodesIATA$Code) && !is.element(x$Op.Al,lcc)) {
      prprftmrgn <- (0.032)
    } else {
      prprftmrgn <- (0.025)
    }
    return(prprftmrgn)
  }
  
  evalsAl <- adply(x, 1,function(a) c(evaluation=prod(c(a$Ops.Week,a$Kilometers,a$Seats.Week))*profitmargin(a)))
  evalsAl <- as.data.frame(cbind(Op.Al = x$Op.Al, evalsAl))
  return(evalsAl)
}

evaluationAlout <- function(x){
  x <- as.data.frame(x)
  #prprftmrgn <- 0.025
  profitmargin <- function(x){
    #if low-cost carrier
    if (is.element(x$Op.Al,lcc)){
      prprftmrgn <- 0.088
    }
    #if gre flight
    else if (is.element(x$Dest,greAPcodesIATA$Code)) {
      prprftmrgn <- (-0.020)
    }
    #if EU but non gre
    else if (is.element(x$Dest,euAPcodesIATA$Code) && !is.element(x$Op.Al,lcc)) {
      prprftmrgn <- (0.042)
    }
    #if non EU 
    else if (!is.element(x$Dest,euAPcodesIATA$Code) && !is.element(x$Op.Al,lcc)) {
      prprftmrgn <- (0.032)
    } else {
      prprftmrgn <- (0.025)
    }
    return(prprftmrgn)
  }
  
  evalsAl <- adply(x, 1,function(a) c(evaluation=prod(c(a$Ops.Week,a$Kilometers,a$Seats.Week))*profitmargin(a)))
  evalsAl <- as.data.frame(cbind(Op.Al = x$Op.Al, evalsAl))
  return(evalsAl)
}

incoming <- evaluationAlin(incoming)
outgoing <- evaluationAlout(outgoing)

count=1

for (i in seq(start,end,by=stepp)) {
  #get flights in the first interval
  pente=incoming[which(incoming$Arr.Time<(i+stepp) & incoming$Arr.Time>=i),]
  pen = incoming
  #pen$evaluation <- alEvMatInHER2010$evaluation
  pente2 = pen[which(pen$Arr.Time<(i+stepp) & pen$Arr.Time>=i),]
 
  #count how many flights per week
  for (j in 1:(length(unique_airIn)-1)) {
    slotsarrayALin[count,j]=sum(pente[which(pente$Op.Al==unique_airIn[j]),]$Ops.Week)
    evaluateslotsALin[count,j]=sum(pente2[which(pente2$Op.Al==unique_airIn[j]),]$evaluation)
  }
  slotsarray[count] = sum(pente$Ops.Week)
  #alternatively you can check for specific day of the week
  #e.g. for monday (day=1)
  #day=1
  #slotsarray[count]=sum(str_count(as.character(pente$Op.Days),fixed(day)))
  #slotsarrayAL[count] = sum(pente$Op.Al)
  
  #don't forget to reset counter after running once
  count=count+1
}

count = 1

for (i in seq(start,end,by=stepp)) {
  #get flights in the first interval
  pente=outgoing[which(outgoing$Arr.Time<(i+stepp) & outgoing$Arr.Time>=i),]
  pen = outgoing
  #pen$evaluation <- alEvMatOutHER2010$evaluation
  pente2 = pen[which(pen$Arr.Time<(i+stepp) & pen$Arr.Time>=i),]
  
  #count how many flights per week
  for (j in 1:(length(unique_airOut)-1)) {
    slotsarrayALout[count,j]=sum(pente[which(pente$Op.Al==unique_airOut[j]),]$Ops.Week)
    evaluateslotsALout[count,j]=sum(pente2[which(pente2$Op.Al==unique_airOut[j]),]$evaluation)
  }
  slotsarray[count] = sum(pente$Ops.Week)
  #alternatively you can check for specific day of the week
  #e.g. for monday (day=1)
  #day=1
  #slotsarray[count]=sum(str_count(as.character(pente$Op.Days),fixed(day)))
  #slotsarrayAL[count] = sum(pente$Op.Al)
  
  #don't forget to reset counter after running once
  count=count+1
}

#calculated evaluations
alEvMatInHER2010 <- t(evaluateslotsALin)
alEvMatInHER2010 <- as.data.frame(alEvMatInHER2010)
colnames(alEvMatInHER2010)<-timeframesString
rownames(alEvMatInHER2010)<-unique_airIn

alEvMatOutHER2010 <- t(evaluateslotsALout)
alEvMatOutHER2010 <- as.data.frame(alEvMatOutHER2010)
colnames(alEvMatOutHER2010)<-timeframesString
rownames(alEvMatOutHER2010)<-unique_airOut

outgoing2010evalHER <- data.matrix(as.matrix(alEvMatOutHER2010))
colnames(outgoing2010evalHER) <- colnames(alEvMatOutHER2010)
rownames(outgoing2010evalHER) <- rownames(alEvMatOutHER2010)

incoming2010evalHER <- data.matrix(as.matrix(alEvMatInHER2010))
colnames(incoming2010evalHER) <- colnames(alEvMatInHER2010)
rownames(incoming2010evalHER) <- rownames(alEvMatInHER2010)

alEvMatOutHER2010 <- outgoing2010evalHER
alEvMatInHER2010 <- incoming2010evalHER

