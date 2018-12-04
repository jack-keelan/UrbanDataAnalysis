##  Build inputs into the ML pipeline

# 1. read files two seconds on either side of hit
# 2. write channel data w/o labels
# 3. write the collelated label file to #2.
# 4. exclude thirty seconds around hit and write background w/ #s 2&3

#############################################################
library(dplyr)
library(tidyverse)
library(zoo)
#############################################################

setwd("C:/Users/mooreet_la/projects/SDRD/competition/")
source("Eric_functions.R")

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas

input_path <- c("data/trainingData/")
#output_path <- c("data/integrated_128Ch/")
#binData_path <- c("data/integrated_128Ch/")
binData_path <- c("data/integrated_128Ch25s/")
#binData_path <- c("data/0144b00025s/")
Hz <- 1     # integration time Hertz

# initialized 'TNG_set'
event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),
                 header=FALSE, sep=",")  # for 128 channel files
#event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),
#                  header=TRUE, sep=",")  # Emma's files
#event <- event[-c(1,2,3)]   # ONLY for Emma's format!!
colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
TNG_set <- event[1:5,]
TNG_set$SourceID <- 0
TNG_set$location <- 0.
TNG_set$runID <- "junk"
TNG_set <- TNG_set[-(1:5),]

for(i in 1:9800){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  event <- read.csv(paste(binData_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
# event <- read.csv(paste(binData_path, evtID, ".csv",sep = ""),
#                   header=TRUE, sep=",")  # Emma's
# event <- event[-c(1,2,3)]   # ONLY for Emma's format!!
  colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
  ans <- answers$SourceTime[i]
  ansID <- answers$SourceID[i]
#  if((round(ans)-2) >= 1 & (round(ans)+2) <= dim(event)[1]){  
  if(ansID == 0){
    x4 <- sample(1:dim(event)[1], (4*Hz + 1), replace=T)
    evt_source <- event[x4,]
    # evt_source$SourceID <- ansID
    # evt_source$location <- ans
    # evt_source$runID <- evtID
  } else if((round(ans)- (2*Hz) ) >= 1 & (round(ans)+ (2*Hz) ) <= dim(event)[1]){
      evt_source <- event[(round(ans)- (2*Hz) ):(round(ans)+ (2*Hz) ),]
      # evt_source$SourceID <- ansID
      # evt_source$location <- ans
      # evt_source$runID <- evtID
      
  }
  
  evt_roll <- as.data.frame(rollapply(evt_source[1:(dim(evt_source)[1]),], 4, sum) )
  evt_roll$SourceID <- ansID
  evt_roll$location <- ans
  evt_roll$runID <- evtID
  
  TNG_set <- rbind(TNG_set,evt_roll)
  
  if(i %% 200 == 0){print(i)}

}

test_stats <- count(TNG_set[1:20000,], SourceID)
test_stats <- count(TNG_set, SourceID)


evt_resum <- as.data.frame(rollapply(evt_source[1:(dim(evt_source)[1]),], 4, sum) )
plot(1:128,d)
plot(1:128,TNG_set[14,1:128])
#write.csv(TNG_set, file = "data/TNG_set.csv")
# write.table(TNG_set, file = "data/TNG_set128_v2.csv", 
#             row.names=FALSE, col.names = TRUE, sep=",", na="0")
write.table(TNG_set, file = "data/TNG_128Ch_roll_25s.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")


write.table(TNG_set, file = "data/TNG_128Ch_roll_test.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")
##########################################################################
##  working the 3 second data
#############################
# input_path <- c("data/0144b00025s/")
# output_path <- c("data/144Ch_rolling/")
# input_path <- c("data/0144b00025s/")
# output_path <- c("data/144Ch_rolling/")
input_path <- c("data/integrated_128Ch/")
output_path <- c("data/integrated_128Ch_3sec/")

# initialized 'TNG_set'
event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),
                  header=FALSE, sep=",")  # for 128 channel files
#event <- read.csv(paste(binData_path, "100014", ".csv",sep = ""),
#                  header=TRUE, sep=",")  # Emma's files
#event <- event[-c(1,2,3)]   # ONLY for Emma's format!!
colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
TNG_set <- event[1:5,]
TNG_set$SourceID <- 0
TNG_set$location <- 0.
TNG_set$runID <- "junk"
TNG_set <- TNG_set[-(1:5),]
Hz <- 1

for(i in 1:9800){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
                    header=FALSE, sep=",")  # Emma's
#  event <- event[-c(1,2,3)]   # ONLY for Emma's format!!
  colnames(event) <- paste("Channel",c(1:dim(event)[2]),sep="")
  ans <- answers$SourceTime[i]
  ansID <- answers$SourceID[i]
#  if((round(ans)-2) >= 1 & (round(ans)+2) <= dim(event)[1]){  
  if(ansID == 0){
    x4 <- sample(1:dim(event)[1], (4*Hz + 1), replace=T)
    evt_source <- event[x4,]
    # evt_source$SourceID <- ansID
    # evt_source$location <- ans
    # evt_source$runID <- evtID
  } else if((round(ans)- (2*Hz) ) >= 1 & (round(ans)+ (2*Hz) ) <= dim(event)[1]){
    evt_source <- event[(round(ans)- (2*Hz) ):(round(ans)+ (2*Hz) ),]
    # evt_source$SourceID <- ansID
    # evt_source$location <- ans
    # evt_source$runID <- evtID
    
  }

  # plot(1:128, evt_roll[2,])
  evt_roll <- as.data.frame(rollapply(evt_source[1:(dim(evt_source)[1]),], 3, sum) )
  evt_roll$SourceID <- ansID
  evt_roll$location <- ans
  evt_roll$runID <- evtID
  
  TNG_set <- rbind(TNG_set,evt_roll)
  
  if(i %% 200 == 0){print(i)}
  
}

plot(1:128, TNG_set[17,1:128])

#######



write.table(TNG_set, file = "data/TNG_128Ch_roll_3sec.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

###########################################################################   CURRENT!!!!!!!!!
# create simple sum  one sec or rolling one sec
#input_path <- c("data/integrated_128Ch/")
#output_path <- c("data/integrated_128Ch_3sec/")
#input_path <- c("data/integrated_128Ch25s/")
#output_path <- c("data/128Ch_rolling/")
#input_path <- c("data/testingData/integrated_128Ch25s/")
#output_path <- c("data/testingData/128Ch_rolling/")

#####  Simple sum paths:
# 144 channel
input_path <- c("data/trainingData/0144b00025s/")
#output_path <- c("data/trainingData/144Ch_1Sec/")
#output_path <- c("data/trainingData/144Ch_rolling/")  ## rolling

# 64 channel
input_path <- c("data/trainingData/integrated_64Ch25s/")
#output_path <- c("data/trainingData/64Ch_1Sec/")
#output_path <- c("data/trainingData/64Ch_rolling/")  ## rolling

# 32 Channel
input_path <- c("data/trainingData/integrated_32Ch25s/")
#input_path <- c("data/trainingData/integrated_32Ch125s/")
#output_path <- c("data/trainingData/32Ch_1Sec/")
output_path <- c("data/trainingData/32Ch_rolling/")  ## rolling

# 56 Channel
#input_path <- c("data/trainingData/integrated_56Ch125s/")
input_path <- c("data/trainingData/integrated_56Ch25s/")
#input_path <- c("data/testingData/integrated_56Ch25s/")
#input_path <- c("data/trainingData/integrated_32Ch125s/")
#output_path <- c("data/trainingData/56Ch_1Sec/")
#output_path <- c("data/trainingData/56Ch_rolling/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_1sec_125s/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_2sec_125s/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_3sec_125s/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_4sec_125s/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_3sec/")  ## rolling
#output_path <- c("data/trainingData/56Ch_rolling_4sec/")  ## rolling
output_path <- c("data/trainingData/56Ch_rolling_Hsec/")  ## rolling
#output_path <- c("data/testingData/56Ch_rolling/")  ## rolling
#output_path <- c("data/testingData/56Ch_rolling_3sec/")  ## rolling

# # metric
# input_path <- c("data/trainingData/metrics_256Ch125s/")
# #output_path <- c("data/trainingData/metric_1sec/")
# output_path <- c("data/trainingData/metric_rolling_1sec/")

st_time <- Sys.time()
for(i in 4800:9800){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
# for(i in 1:15923){
#   evtID <- paste("4",as.character(i),sep = "")  ## '4' for testing   '10' for training
#   if(i < 10000){evtID <- paste("40",as.character(i),sep = "")}
#   if(i < 1000){evtID <- paste("400",as.character(i),sep = "")}
#   if(i < 100){evtID <- paste("4000",as.character(i),sep = "")}
#   if(i < 10){evtID <- paste("40000",as.character(i),sep = "")}
  
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
                    header=FALSE, sep=",")
  # event <- read.csv(paste(input_path, evtID, ".csv",sep = ""), #Emma's bungle binning
  #                   header=TRUE, sep=",")    #Emma's bungle binning
  # event <- event[,-c(1:3)]          #Emma's bungle binning  144Ch
  

##  This line for simple sum  
#  evt_one <- simple_sum8(event) # 4 hertz only
########  
  
  # ans <- answers$SourceTime[i]
  # ansID <- answers$SourceID[i]

##  This line for rolling sum
  evt_roll <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 2, sum) )
#####

  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
 write.table(evt_roll, file = paste(output_path, evtID, ".csv",sep = ""),  ## rolling
             row.names=FALSE, col.names = FALSE, sep=",", na="0")
  # write.table(evt_one, file = paste(output_path, evtID, ".csv",sep = ""),  ## simple sum
  #             row.names=FALSE, col.names = FALSE, sep=",", na="0")
  
}

plot(1:128, TNG_set[17,1:128])


########################################################################
###  backround removal
################################
#################################
input_path <- c("data/integrated_128Ch/")
output_path <- c("data/integrated_128Ch_bkgR/")
nbins <- 128; Erange = 2650
numer <- as.numeric(vector() ); denom <- as.numeric(vector() )

st_time <- Sys.time()

for(i in 1:9800){
  evtID <- paste("10",as.character(i), sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
                    header=FALSE, sep=",")  # Emma's
  
  
  evt_roll1 <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 20, sum) )
  evt_roll2 <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 30, sum) )
  evt_roll <- rbind(evt_roll1[1:10,], evt_roll2[1:(dim(evt_roll2)[1] - 10),])
  
  numer <- rowSums(event[30:dim(event)[1],90:128])
  denom <- rowSums(evt_roll[1:dim(evt_roll)[1],90:128])
  
  norm_bkg <- evt_roll
  norm_bkg[,] <- evt_roll[,] * numer[]/denom[]
  
  evt_BKGrm <- event
  evt_BKGrm[30:dim(event)[1],] <- event[30:dim(event)[1],] - norm_bkg[1:dim(norm_bkg)[1],]
#  evt_BKGrm[evt_BKGrm < 0.] <- 0.
  
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(cur_time)}
  
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

####################################################################
###########   3 seconds
####################################################################

###  For half the dataset - careful to split on an event
test_stats <- count(TNG_set[1:19999,], SourceID)
g2 <- (test_stats[1,2] - 5)/3 + (sum(test_stats[2:7,2])/5)  ## files thru this # are train
## filenumber 5027 starts the test set
train_Data <- TNG_set[1:19999,]
write.table(train_Data, file = "data/train_Data.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")
#write.table(events_int, file = paste(binData_path,evtID,".csv", sep = ""), 
#              row.names=FALSE, col.names = FALSE, sep=",", na="0")


##########test

  #   plot(1:dim(event)[2], evt_source[3,])
  # #  gross cnts    
  #   plot(1:dim(event)[1],rowSums(event))
  #   abline(v = ans)  
  
  
dummy <- read.csv(paste(binData_path, "106019", ".csv", sep = ""), header=FALSE, sep=",")
answers[6019,]; dim(dummy)
plot(c(1:128), dummy[40,])

#### grep the name of file to RunID asint
#evt <- end of RunID only
#names(event) <- c("timeDelta","energy")

###########################################


#junk[is.na(junk)] <- 0

#colnames(events_int) <- paste("Channel",c(1:bins),sep="")
#plot(c(1:bins),events_int[384,])



#i_Par <- round(TNG_labels$SourceTime[evt], digits = 1)
#head(event)
#head(TNG_labels)


#gg <- as.list(as.character(c(1:bins)))
# sp382 <- subset(spectra, spectra$spec_num == 382) 
# plot(sp382$channel,sp382$n)
# 
# 
# stocks <- data.frame(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# stocksm <- stocks %>% gather(stock, price, -time)
# stocksm %>% spread(stock, price)
# stocksm %>% spread(time, price)

plot(b$channel,b$freq)

#histogram(event$energy, breaks=1024, log(y))
#histogram(event$time, breaks=100,xlim=c(1:10000))#, log(y))
#plot(event$energy, log="y", type='h')#, lwd=10, lend=2)

## number of bins in one sec
#seconds <- round(dim(event)[1]/(sum(event$time)/1000000))



#j <- subset(event$channel, event$spec_num == 382)
#hist.1 <- hist(j, breaks=seq(0,bins,by=1.), plot = FALSE)

# ############### UNeeded
# #hist.1 <- hist(runif(100,min=2,max=4), breaks=seq(0,6,by=.2), plot=FALSE)
# hist.trimmed <- TrimHistogram(hist.1)
# 
# length(hist.1$counts)
# sum(hist.1$counts)
# length(hist.trimmed$counts)
# sum(hist.trimmed$counts)
# 
# plot(1:length(hist.trimmed$counts), hist.trimmed$counts)
# 
# testAgg <- aggregate(event[,3:4], by=list(event$channel,event$spec_num),
#                      FUN=sum,na.rm=TRUE)
# ############### UNeeded
#aggregate(state.x77, list(Region = state.region, Cold = state.x77[,"Frost"] > 130), mean)
# 
# t2 <- aggregate(spec_num ~ channel, data = event, sum)
# cps <- aggregate(channel ~ spec_num, data = event, sum)
# #event <- 
# 
# 
# a <- table(subset(event$channel, event$spec_num == 100))
# plot(1:127,a$freq)

###############  This is the money
d <- subset(event,event$spec_num == 382)[,3:4]
#c <- d[,3:4]
b <- count(d, "channel") #, wt_var = 1)
plot(b$channel,b$freq)

##########################################

test3 <- aggregate(spec_num ~ channel, data = d, sum)
f <- subset(d, d$channel == 20)
plot(test3$channel,test3$spec_num)

########################################
bevs <- data.frame(cbind(name = c("Bill", "Llib"), drink = c("coffee", "tea", "cocoa", "water"), cost = seq(1:8)))
bevs$cost <- as.integer(bevs$cost)

library(plyr)
cnt1 <- count(bevs, "name")
cnt2 <- count(bevs, c("name", "drink"))

agg1 <- aggregate(cost ~ name + drink, data = bevs, sum)
agg2 <- aggregate(cost ~ name, data = bevs, sum)
agg3 <- aggregate(cost ~ name, data = bevs, mean)

# aggregate data frame mtcars by cyl and vs, returning means
# for numeric variables
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs), 
                    FUN=mean, na.rm=TRUE)
print(aggdata)
detach(mtcars)

#plot(event2$spectra,event2$cps)
j <- subset(event$energy, event$spec_num == (i_Par*10))
hist(j, breaks=256)

sig_set <- subset(event, event$spec_num >= (10*i_Par - 10) & event$spec_num <= (10*i_Par + 10) )
#sig_set <- C()

# sig_set[i] <- subset(event$energy, event$spec_num >= (10*i_Par - 11 + [i]) & event$spec_num <= (10*i_Par - 1 + [i]) )
# hist(k, breaks=256)

ed_exp1 <- event[c((10*i_Par - 10):(10*i_Par + 10)),]






r <- hist(log(event$energy))
r <- hist(event$energy)
plot(r$breaks[-1], r$counts, log='y', type='h')
# or alternatively:
barplot(r$counts, log="y", col="white", names.arg=r$breaks[-1])

x <- rnorm(10000)
h <- hist(event$energy[20000:45000], breaks=bins, plot=FALSE)
plot(h$mids, h$density, log="y", type='b')
length(h)

event2 <- read.csv("temp2.csv", header=TRUE)

plot(event2$spectra,event2$cps)