################################################################################

################################################################################
# Script for following along in Data Exploration and Visualization module
# Copy-paste line by line or use the "Run" button in R Studio
#library(lattice)
# Enhanced Scatter Plot Matrices
#library(GGally)
#ggpairs(iris, ggplot2::aes(color=Species))

## ggplot2 introduction ##
#library(ggplot2)
#library(plyr)
library(dplyr)
library(tidyverse)
library(zoo)
#library(HistogramTools)
############################################################3

setwd("C:/Users/mooreet_la/projects/SDRD/competition/")

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas

####################################### CURRENT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#input_path <- c("data/trainingData/listData/")
input_path <- c("data/testingData/listData/")
#output_path2 <- c("data/integrated_128Ch/")
#output_path <- c("data/integrated_128Ch25s/")
#output_path <- c("data/testingData/integrated_128Ch25s/")
#output_path <- c("data/trainingData/integrated_64Ch25s/")
#output_path <- c("data/trainingData/integrated_32Ch125s/")
#output_path <- c("data/trainingData/integrated_128Ch125s/")
output_path <- c("data/testingData/metrics_125s/")
# output_path <- c("data/testingData/integrated_32Ch_125s/")
# output_path2 <- c("data/testingData/integrated_128Ch_125s/")
bins <- 256
#bins <- 64
#bins <- 32
#bins2 <- 128
#bins <- 32
Hz <- 8
int_length <- 1000000/Hz # integration window in microseconds
energy_range <- 2650  # energy range
#energy_range <- 3000  # energy range

st_time <- Sys.time()
for(i in 1:15923){
  evtID <- paste("4",as.character(i),sep = "")  ## '4' for testing   '10' for training
  if(i < 10000){evtID <- paste("40",as.character(i),sep = "")}
  if(i < 1000){evtID <- paste("400",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("4000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("40000",as.character(i),sep = "")}
# for(i in 1:9800){
#   evtID <- paste("10",as.character(i),sep = "")  ## '4' for testing   '10' for training
#   if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
#   if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
#   if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  event <- read.csv(paste(input_path, "runID-", evtID, ".csv",sep = ""), header=FALSE, sep=",")
#  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  
#  event2 <- event
  
  event[, 'spec_num'] <- cumsum(event[, 1]) %/% int_length
  event[, 'channel'] <- event[, 2] %/% (energy_range/bins)

  evts <- transform(subset(event)[,3:4], spec_num = as.integer(spec_num), 
               channel = as.integer(channel))

  spectra <- count_(evts, c("spec_num","channel"))

  junk <- subset(spectra %>% spread(spec_num,n))[1:bins,]
  events_int <- (t(junk))[-1,]
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
  # write.table(events_int, file = paste(output_path,evtID,".csv", sep = ""), 
  #           row.names=FALSE, col.names = FALSE, sep=",", na="0")

  ####################
  #second binning
  # event2[, 'spec_num'] <- cumsum(event2[, 1]) %/% int_length
  # event2[, 'channel'] <- event2[, 2] %/% (energy_range/bins2)
  # 
  # evts2 <- transform(subset(event2)[,3:4], spec_num = as.integer(spec_num), 
  #                   channel = as.integer(channel))
  # 
  # spectra2 <- count_(evts2, c("spec_num","channel"))
  # 
  # junk2 <- subset(spectra2 %>% spread(spec_num,n))[1:bins2,]
  # events_int2 <- (t(junk2))[-1,]
  # 
  # if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  # 
  # write.table(events_int2, file = paste(output_path2,evtID,".csv", sep = ""), 
  #             row.names=FALSE, col.names = FALSE, sep=",", na="0")
  
  events_int[is.na(events_int)] <- 0

  win_metric <- window_sums(events_int)

  write.table(win_metric, file = paste(output_path,evtID,".csv", sep = ""),
              row.names=FALSE, col.names = FALSE, sep=",", na="0")
}

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



# gross_cnts <- event %>% 
#   group_by(spec_num) %>%
#   summarise(cps = length(spec_num))
# 
# plot(gross_cnts$spec_num,gross_cnts$cps)


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

Eng_Range <- 2650
nbins <- 1024

width <- Eng_Range/nbins

mid_vec <- vector()

for(i in 1:nbins){
  mid <- ( (i*width) - (width/2) )
  mid_vec <- c(mid_vec,mid)
}

mid_vec


#########################
window_sums <- function(evt){
  
  bins <- 256
  energy_range <- 2650 
  f <- bins/energy_range
  
  win_sums <- data.frame(matrix(ncol = 0, nrow = dim(evt)[1]))
  
  win_sums$P1 <- rowSums(evt[,round(52*f):round(64*f)])
  win_sums$U1 <- rowSums(evt[,round(76*f):round(116*f)])
  win_sums$T1 <- rowSums(evt[,round(83*f):round(100*f)])
  win_sums$P2 <- rowSums(evt[,round(88*f):round(112*f)])
  win_sums$T2 <- rowSums(evt[,round(132*f):round(148*f)])
  win_sums$U2 <- rowSums(evt[,round(175*f):round(195*f)])
  win_sums$I1 <- rowSums(evt[,round(270*f):round(300*f)])
  win_sums$P3 <- rowSums(evt[,round(318*f):round(430*f)])
  win_sums$I2 <- rowSums(evt[,round(345*f):round(385*f)])
  win_sums$B1 <- rowSums(evt[,round(560*f):round(630*f)])
  win_sums$I3 <- rowSums(evt[,round(613*f):round(661*f)])
  win_sums$P4 <- rowSums(evt[,round(638*f):round(688*f)])
  win_sums$I4 <- rowSums(evt[,round(700*f):round(750*f)])
  win_sums$C1 <- rowSums(evt[,round(1140*f):round(1210*f)])
  win_sums$C2 <- rowSums(evt[,round(1295*f):round(1375*f)])
  win_sums$B2 <- rowSums(evt[,round(1425*f):round(1495*f)])
  win_sums$B3 <- rowSums(evt[,round(1723*f):round(1803*f)])
  win_sums$B4 <- rowSums(evt[,round(2560*f):256])
  
  return(win_sums)
  
}
