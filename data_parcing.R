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
library(plyr)
library(dplyr)
#library(HistogramTools)
############################################################3
## Extended Titanic Exploration ##

# Set your working directory to the bootcamp root folder using setwd()
# or this line won't work

setwd("C:/Users/mooreet_la/projects/SDRD/competition/")
event <- read.csv("data/trainingData/100014.csv", header=FALSE, sep=",")
TNG_labels <- read.csv("data/answers.csv", header = TRUE)

#### grep the name of file to RunID asint
#evt <- end of RunID only
evt <- 14
i_Par <- round(TNG_labels$SourceTime[evt], digits = 1)
head(event)
head(TNG_labels)

names(event) <- c("timeDelta","energy")

event[, 'spec_num'] <- cumsum(event[, 1]) %/% 1000000
event[, 'channel'] <- event[, 2] %/% (2650/128)

d <- subset(event,event$spec_num == 382)[,3:4]
#c <- d[,3:4]
b <- count(d, "channel") #, wt_var = 1)
c <- c(0:127)

c <- as.data.frame(table( factor(b$channel, levels = 0:127)) )
names(b) <- c("channel","Freq"); names(c) <- c("channel","Freq")

#df4 <- merge(b,c, by.x = "channel", by.y = "channel", all = TRUE)
df4 <- merge(c, b, by = "channel", all = TRUE)
df4[is.na(df4)] <- 0
plot(df4$channel,df4$Freq.y)

library(tidyverse)
evts <- subset(event)[,3:4]
spectra <- count_(evts, c("spec_num","channel"))




plot(b$channel,b$freq)

#histogram(event$energy, breaks=1024, log(y))
#histogram(event$time, breaks=100,xlim=c(1:10000))#, log(y))
#plot(event$energy, log="y", type='h')#, lwd=10, lend=2)

## number of bins in one sec
#seconds <- round(dim(event)[1]/(sum(event$time)/1000000))



#j <- subset(event$channel, event$spec_num == 382)
#hist.1 <- hist(j, breaks=seq(0,128,by=1.), plot = FALSE)

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


gross_cnts <- event %>% 
  group_by(spec_num) %>%
  summarise(cps = length(spec_num))

plot(gross_cnts$spec_num,gross_cnts$cps)
abline(v = 382.13)

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
h <- hist(event$energy[20000:45000], breaks=128, plot=FALSE)
plot(h$mids, h$density, log="y", type='b')
length(h)

event2 <- read.csv("temp2.csv", header=TRUE)

plot(event2$spectra,event2$cps)
