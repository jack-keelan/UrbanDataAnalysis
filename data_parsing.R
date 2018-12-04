################################################################################
library(dplyr)
library(tidyverse)
#library(HistogramTools)
############################################################3

setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas

source("Eric_functions.R")
####################################### CURRENT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
input_path <- c("data/trainingData/listData/")
#input_path <- c("data/testingData/listData/")
#output_path2 <- c("data/integrated_128Ch/")
#output_path <- c("data/integrated_128Ch25s/")
output_path <- c("data/trainingData/integrated_228Ch_5Hz/") # 0-1500 (200 bins) then 28 bins
#output_path <- c("data/trainingData/integrated_512Ch_12Hz/")
#output_path <- c("data/testingData/integrated_128Ch25s/")
#output_path <- c("data/trainingData/integrated_64Ch25s/")
#output_path <- c("data/trainingData/integrated_32Ch25s/")
#output_path <- c("data/testingData/integrated_32Ch25s/")
bins1 <- 200*2
#bins <- 128
#bins <- 64
bins2 <- 28*2
Hz <- 5
## 
int_length <- 1000000/Hz # integration window in microseconds
energy_range <- 3000  # energy range
shortest_evt <- 50

st_time <- Sys.time()
for(i in 1:9800){
  evtID <- paste("10",as.character(i),sep = "")  ## '4' for testing   '10' for training
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
#  event <- read.csv(paste(input_path, "runID-", evtID, ".csv",sep = ""), header=FALSE, sep=",")
  
#   evt_len <- sum(event$V1)/1000000
#   if(evt_len <= shortest_evt){shortest_evt <- evt_len}
#   if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
# }
# 
# shortest_evt ## the shortest run in the training set is 45.72114 seconds

  event[, 'spec_num'] <- cumsum(event[, 1]) %/% int_length
#  event[, 'channel'] <- event[, 2] %/% (energy_range/bins) # single bin width
  event2 <- event1 <- event
  event1[, 'channel'] <- event1[, 2] %/% (energy_range/bins1)
  event2[, 'channel'] <- event2[, 2] %/% (energy_range/bins2)
  
#  event[,129:256] <- events_int[,129:256]

# # single bin width
#   evts <- transform(subset(event)[,3:4], spec_num = as.integer(spec_num), 
#                channel = as.integer(channel)) 
#   spectra <- count_(evts, c("spec_num","channel"))
#   junk <- subset(spectra %>% spread(spec_num,n))[1:bins,]
#   events_int <- (t(junk))[-1,]

# two differnet bin widths
  evts1 <- transform(subset(event1)[,3:4], spec_num = as.integer(spec_num), 
                    channel = as.integer(channel)) 
  spectra1 <- count_(evts1, c("spec_num","channel"))
  junk1 <- subset(spectra1 %>% spread(spec_num,n))[1:bins1,]
  events_int1 <- (t(junk1))[-1,]
  events_int1 <- events_int1[,1:(as.integer(bins1/2) )]

  evts2 <- transform(subset(event2)[,3:4], spec_num = as.integer(spec_num), 
                     channel = as.integer(channel)) 
  spectra2 <- count_(evts2, c("spec_num","channel"))
  junk2 <- subset(spectra2 %>% spread(spec_num,n))[1:bins2,]
  events_int2 <- (t(junk2))[-1,]
  events_int2 <- events_int2[,(as.integer((bins2/2)+1) ):bins2]
  
  events_int <- cbind(events_int1,events_int2)
    
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
  write.table(events_int, file = paste(output_path,evtID,".csv", sep = ""), 
            row.names=FALSE, col.names = FALSE, sep=",", na="0")
}

files <- listData
Erange <- 2650
bins <- 128
Hz <- 4
start <- 1
finish <- 100
type <-trainin

parsingData(files = listData, type, bins, Hz = 4, Erange = 2650, start = 1, finish)

########################################################################
## random parsing stuff


##########test
dummy <- read.csv(paste(output_path, "102334", ".csv", sep = ""), header=FALSE, sep=",")

plot(c(1:128), dummy[33,])

#### grep the name of file to RunID asint
#evt <- end of RunID only
#names(event) <- c("timeDelta","energy")


## look at gross counts
gross_cnts <- event %>% 
  group_by(spec_num) %>%
  summarise(cps = length(spec_num))

plot(gross_cnts$spec_num,gross_cnts$cps)
abline(v = 382.13)
abline(v = 392.13)
###########################################

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
