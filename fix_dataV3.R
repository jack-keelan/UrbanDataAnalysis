
################################################################################
library(dplyr)
library(tidyverse)
library(zoo)
############################################################3

setwd("C:/Users/mooreet_la/projects/SDRD/competition/")

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas


#####################################################################################  CURRENT!!!!!!!!!!!!
### current training set simple 5 draws

#binData_path <- c("data/trainingData/64Ch_1sec/")
#binData_path <- c("data/trainingData/144Ch_1sec/")
#binData_path <- c("data/trainingData/32Ch_1sec/")
#binData_path <- c("data/trainingData/56Ch_1sec/")
binData_path <- c("data/trainingData/metric_1sec/")
Hz <- 1     # integration time Hertz

# initialized 'TNG_set'
event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),header=FALSE, sep=",")

colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
TNG_set <- event[1:5,]
TNG_set$SourceID <- 0
TNG_set$location <- 0.
TNG_set$runID <- "junk"
TNG_set <- TNG_set[-(1:5),]

g <- read.csv("g.csv")

st_time <- Sys.time()
for(j in 1:9800){
  evtID <- paste("10",as.character(j),sep = "")
  if(j < 1000){evtID <- paste("100",as.character(j),sep = "")}
  if(j < 100){evtID <- paste("1000",as.character(j),sep = "")}
  if(j < 10){evtID <- paste("10000",as.character(j),sep = "")}
  
  event <- read.csv(paste(binData_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")

  colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
#  colnames(event2) <- paste("Channel",c(1:dim(event)[2]),sep="")
  ans <- answers$SourceTime[j]
  ansID <- answers$SourceID[j]
#  if((round(ans)-2) >= 1 & (round(ans)+2) <= dim(event)[1]){  
  if(ansID == 0){
    x4 <- sample(1:dim(event)[1], (4*Hz + 1), replace=T)
    evt_source <- event[x4,]

  } else if((round(ans)- (2*Hz) ) >= 1 & (round(ans)+ (2*Hz) ) <= dim(event)[1]){
      evt_source <- event[(round(ans)- (2*Hz) ):(round(ans)+ (2*Hz) ),]
      
  }
  
#  evt_source <- as.data.frame(rollapply(evt_source[1:(dim(evt_source)[1]),], 3, sum) )
  
  evt_source$SourceID <- ansID
  evt_source$location <- ans
  evt_source$runID <- evtID
  
  TNG_set <- rbind(TNG_set,evt_source)

  if(j %% 200 == 0){cur_time <- Sys.time() - st_time; print(j); print(cur_time)}

}

write.table(TNG_set, file = "data/TNG_metric_1sec.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")
# write.table(TNG_set, file = "data/TNG_56Ch_3sec.csv", 
#             row.names=FALSE, col.names = TRUE, sep=",", na="0")
# write.table(TNG_set, file = "data/TNG_32Ch.csv", 
#             row.names=FALSE, col.names = TRUE, sep=",", na="0")
# write.table(TNG_set, file = "data/TNG_64Ch.csv", 
#             row.names=FALSE, col.names = TRUE, sep=",", na="0")

# write.table(TNG_set, file = "data/TNG_144Ch.csv", 
#             row.names=FALSE, col.names = TRUE, sep=",", na="0")
#######################
############################################################################# CURRENT!!!!!!!!!!!!!11
#####################################################################################################
binData_path <- c("data/trainingData/metric_1sec/")
Data_path2 <- c("data/trainingData/metric_1sec_bkgR/")
Data_path3 <- c("data/trainingData/56Ch_1sec/")

# initialized 'TNG_set'
event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),header=FALSE, sep=",")

colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
TNG_set <- event[1:5,]
TNG_set$SourceID <- 0
TNG_set$location <- 0.
TNG_set$runID <- "junk"
TNG_set <- TNG_set[-(1:5),]

gname <- read.csv("g.csv")
#####  Used to generate the 'selected dataset'
winW <- 2

st_time <- Sys.time()
for(j in 1:9800){
  evtID <- paste("10",as.character(j),sep = "")
  if(j < 1000){evtID <- paste("100",as.character(j),sep = "")}
  if(j < 100){evtID <- paste("1000",as.character(j),sep = "")}
  if(j < 10){evtID <- paste("10000",as.character(j),sep = "")}
  
  event <- read.csv(paste(binData_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  event2 <- read.csv(paste(Data_path2, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  
  event3 <- read.csv(paste(Data_path3, evtID, ".csv",sep = ""), header=FALSE, sep=",")  
  colnames(event3) <- paste("Channel",c(1:dim(event3)[2]),sep="")
  # colnames(event2) <- paste("Channel",c(1:dim(event)[2]),sep="")
  
  colnames(event) <- c(as.character(gname$x))
  colnames(event2) <- c(as.character(gname$x))
  ans <- answers$SourceTime[j]
  ansID <- answers$SourceID[j]
  if( ((round(ans)-4) >= 1 & (round(ans)+4) <= dim(event)[1]) | ansID == 0 ){
#    print("fuck")
    if(ansID == 0){
#      print("what")
      x4 <- sample(1:dim(event)[1], (1*winW + 1), replace=T)
      evt_source <- event[x4,]
  
    } else if((round(ans)- (4*winW) ) >= 1 & (round(ans)+ (4*winW) ) <= dim(event)[1]){
      evt_source <- event[(round(ans)- (4*winW) ):(round(ans)+ (4*winW) ),]
    }
  }
    
  if(ansID == 1){
    g <- rowSums(event2[,c("U1","U2")])
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 120)) ,]
  } else if(ansID == 2){
    g <- rowSums(event2[,c("P1","P2","P3")])#; g[g == 0] <- 1
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 120)) ,]
  } else if(ansID == 3){
    g <- rowSums(event2[,c("I1","I2","I3","I4")])#; g[g == 0] <- 1
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 100)) ,]
  } else if(ansID == 4){
    g <- rowSums(event2[,c("C1","C2")])#; g[g == 0] <- 1
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 50)) ,]
  } else if(ansID == 5){
    g <- rowSums(event2[,c("T1","T2")])#; g[g == 0] <- 1
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 90)) ,]
  } else if(ansID == 6){
    g <- rowSums(event2[,c("T2","U2")])#; g[g == 0] <- 1
    h <- rownames(evt_source)
    h2 <- g[as.numeric(h[1]):as.numeric(h[dim(evt_source)[1]])]
    evt_temp <- evt_source[-c(which(h2 < 80)) ,]
  } else {evt_temp <- evt_source}

  if(dim(evt_temp)[1] == 0){evt_temp <- event[round(ans),]}

  d <- as.numeric(rownames(evt_temp))

#  evt_temp <- cbind(event3[c(d),],evt_temp)
  evt_temp <- event2[c(d),]

  evt_temp$SourceID <- ansID
  evt_temp$location <- ans
  evt_temp$runID <- evtID

  TNG_set <- rbind(TNG_set,evt_temp)
  
  if(j %% 200 == 0){cur_time <- Sys.time() - st_time; print(j); print(cur_time)}
  
}

dim(TNG_set)

#test_stats <- count(TNG_set[1:20000,], SourceID)
test_stats <- count(TNG_set, SourceID)
test_stats

g <- rowSums(evt_source[50:72]); g[g == 0] <- 1 ## 3,2 thresh=5,10? on f
f <- rowSums(evt_source[1:22])/abs(g)                ## 3,2
evt_temp <- evt_source[-c(which(f < 8)),]
dim(TNG_set)

max(TNG_set$P1); min(TNG_set$P1); median(TNG_set$P1[1:30])
hist(TNG_set$P1); hist(TNG_set$P2); hist(TNG_set$P3)
hist(TNG_set$P1[1:30]); hist(TNG_set$P2); hist(TNG_set$P3)


plot(1:128,evt_source[1,])
plot(1:128,evt_source[2,])
plot(1:128,evt_source[3,])
plot(1:128,evt_source[4,])
plot(1:128,evt_source[5,])
plot(1:128,evt_source[6,])
plot(1:128,evt_source[7,])
plot(1:128,evt_source[8,])
plot(1:128,evt_source[9,])
plot(1:128,evt_temp[1,])
plot(1:128,evt_temp[2,])
plot(1:128,evt_temp[3,])
plot(1:128,evt_temp[4,])
plot(1:128,evt_temp[5,])
plot(1:128,evt_temp[6,])
plot(1:128,evt_temp[7,])
plot(1:128,evt_temp[8,])
plot(1:128,evt_temp[9,])
f <- rowSums(evt_source[53:68])/abs(rowSums(evt_source[80:95]) )
g <- rowSums(evt_temp[75:120]); g[g == 0] <- 1

g <- rowSums(evt_source[55:70]); g[g == 0] <- 1 ## 1,5,6 thresh=20? on f
f <- rowSums(evt_source[1:15])/abs(g)                ## 1
f

g <- rowSums(evt_source[50:72]); g[g == 0] <- 1 ## 3,2 thresh=5,10? on f
f <- rowSums(evt_source[1:22])/abs(g)                ## 3,2
f

g <- rowSums(evt_source[80:95]); g[g == 0] <- 1 ## 4 thresh=5,10? on f
f <- rowSums(evt_source[53:68])/abs(g)                ## 4
f

h <- rowSums(evt_temp[65:75])/g

(1460/2650)*128; (2000/2650)*128  # 75-95
(1500/2650)*128; (2000/2650)*128  # 75-95
(1000/2650)*128; (2500/2650)*128  # 50-120
(1100/2650)*128; (1400/2650)*128  # 50-65  Co60 (4)
(1/2650)*128; (250/2650)*128  # 1-15  HEU (1)
(1/2650)*128; (250/2650)*128  # 1-15  Tc99 (5) & combined (6)
(1/2650)*128; (450/2650)*128  # 1-22  I131 (3)
(1/2650)*128; (450/2650)*128  # 1-22  WGPu (2)
############




evt_resum <- as.data.frame(rollapply(evt_source[1:(dim(evt_source)[1]),], 4, sum) )
plot(1:128,d)
plot(1:128,TNG_set[14,1:128])

write.table(TNG_set, file = "data/TNG_128Ch_roll_25s.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")


write.table(TNG_set, file = "data/TNG_128Ch_bkgRMv2.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

write.table(TNG_set, file = "data/TNG_metric_selected.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

write.table(TNG_set, file = "data/TNG_56Ch_selectedbyMetric.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

write.table(TNG_set, file = "data/TNG_56Chand18_selectedbyMetric.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

write.table(TNG_set, file = "data/TNG_18Brm_selectedbyMetric.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

########################################################################
###  backround removal
################################
#################################
input_path <- c("data/trainingData/metric_1sec/")
output_path <- c("data/trainingData/metric_1sec_bkgR/")
nbins <- 18; Erange = 2650
numer <- as.numeric(vector() ); denom <- as.numeric(vector() )

st_time <- Sys.time()

for(j in 1:9800){
  evtID <- paste("10",as.character(j), sep = "")
  if(j < 1000){evtID <- paste("100",as.character(j),sep = "")}
  if(j < 100){evtID <- paste("1000",as.character(j),sep = "")}
  if(j < 10){evtID <- paste("10000",as.character(j),sep = "")}
  
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
                    header=FALSE, sep=",")  # Emma's
  colnames(event) <- c(as.character(g$x)) 
  
  evt_roll1 <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 20, sum) )
  evt_roll2 <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 30, sum) )
  evt_roll <- rbind(evt_roll1[1:10,], evt_roll2[1:(dim(evt_roll2)[1] - 10),])
  
  numer <- rowSums(event[30:dim(event)[1], c("B2","B3","B4")])
  denom <- rowSums(evt_roll[1:dim(evt_roll)[1], c("B2","B3","B4")])
  
  norm_bkg <- evt_roll
  norm_bkg[,] <- evt_roll[,] * numer[]/denom[]
  
  evt_BKGrm <- event
  evt_BKGrm[30:dim(event)[1],] <- event[30:dim(event)[1],] - norm_bkg[1:dim(norm_bkg)[1],]
#  evt_BKGrm[evt_BKGrm < 0.] <- 0.
  
  
  if(j %% 200 == 0){cur_time <- Sys.time() - st_time; print(j); print(cur_time)}
  
  write.table(evt_BKGrm, file = paste(output_path, evtID, ".csv",sep = ""),
              row.names=FALSE, col.names = FALSE, sep=",", na="0")
  
}

st_time <- Sys.time()

cur_time <- Sys.time() - st_time

plot(1:dim(event)[2], event[3,])
plot(1:dim(evt_roll)[2], evt_roll[3,]); abline(v = 1460*nbins/Erange); abline(v = 1750*nbins/Erange)
abline(h = sum(evt_roll[3,90:128])/39)

norm_roll <- evt_roll
norm_roll[346,] <- evt_roll[346,] * (sum(event[385,90:128])/(sum(evt_roll[346,90:128])) )
new <- event[385,]-norm_roll[346,]
plot(1:dim(norm_roll)[2], norm_roll[3,]); abline(v = 1460*nbins/Erange); abline(v = 1750*nbins/Erange)

bkg_subEVT <- event
bkg_subEVT[40:dim(event)[1]] <- event[40:dim(event)[1],] - norm_roll[1:dim(norm_roll)[1],]

#  gross cnts
  plot(1:dim(event)[1],rowSums(event))
  abline(v = ans)
  
  plot(1:275,rowSums(event[1:275,]))

plot(1:128, TNG_set[17,1:128])
###############################
############################################################################

