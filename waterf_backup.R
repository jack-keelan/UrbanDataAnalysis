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

new_answers <- answers

# 32 Channel
input_path <- c("data/trainingData/0144b00025s/")

#input_path <- c("data/trainingData/integrated_32Ch125s/")
output_path <- c("data/trainingData/new_runs_0144b00025s/")
#output_path <- c("data/trainingData/32Ch_rolling/")  ## rolling
df_evt <- data.frame(matrix(ncol = 3, nrow = 9800))

names(df_evt) <- c("length","answer","location")

Hz <- 4; old_location <- 0
st_time <- Sys.time()
for(i in 1:9800){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  # event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
  #                   header=FALSE, sep=",")
  event <- read.csv(paste(input_path, evtID, ".csv",sep = ""),
                    header=TRUE, sep=",")  # Emma's files
  event <- event[-c(1,2,3)]   # ONLY for Emma's format!!
  
#   df_evt$length[i] <- dim(event)[1]
#   df_evt$answer[i] <- answers$SourceID[i]
#   df_evt$location[i] <- answers$SourceTime[i]/dim(event)[1]
# #  df_evt$abs_loc[i] <- answers$SourceID[i]
  
  shift <- 0  # not really used just tags short/long runs
  
  if(answers$SourceID[i] == 0){
    new_evt_temp <- event
    new_location <- 0
    limit <- 0
  } else{
    limit <- (2*Hz)     # this is so the source won't be in the first/last 2seconds
    old_location <- as.integer(answers$SourceTime[i] * Hz)
    max_location <- old_location + as.integer(30*Hz)
    min_location <- old_location - as.integer(30*Hz) + 1
    if(min_location < 1){     # account for their being short/long runs
      min_location <- 1
      shift <- (old_location - as.integer(30*Hz) )
    }
    if(max_location > dim(event)[1]){ max_location <- dim(event)[1] }
    new_evt_temp <- subset( event[min_location:max_location,] )
  }
  
  range_avl <- as.integer( dim(new_evt_temp)[1] - (30 * Hz) - limit)
  new_start <- as.integer( round(sample(limit:range_avl,1, replace = TRUE)) )
  ## new_start will be the true index start of the 'new_evt_temp'
  ## new_end should just be new_start+(30*Hz)
  new_end <- new_start + (30*Hz) - 1

  if(answers$SourceID[i] != 0){
    new_location <- (30*Hz) - new_start + 1  # new start is opposit to source location
    new_answers$SourceTime[i] <- new_location + (answers$SourceTime[i]-(old_location/Hz) )
  }
  
  
  
  new_event <- subset(new_evt_temp[(new_start:new_end),])
  
  # if(answers$SourceID[i] != 0){
  #   shift <- (new_start - 1)
  #   new_location <- ( (dim(event)[1] - new_end) + 1)
  #   new_answers$SourceTime[i] <- new_location + (answers$SourceTime[i] - old_location)
  # }

  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
  write.table(new_event, file = paste(output_path, evtID, ".csv",sep = ""),  ## simple sum
              row.names=FALSE, col.names = FALSE, sep=",", na="0")
  
}

write.table(new_answers, file = paste("new_answers", ".csv",sep = ""),  ## simple sum
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

plot(1:dim(df_evt)[1], df_evt[,1])
freq <- as.data.frame(count(df_evt,round(length,digits = 1)))
freq_weigth <- vector(); k=1
for (i in 1:dim(freq)[1]){
  for(j in 1:freq$n[i]){
    freq_weigth[k] <- c(freq_weigth, freq[i,1])
    k <- k+1
  }
}
length_sec <- dim(freq)[1]
plot(1:(length_sec),freq$n ); title("Length of run in Seconds")
#hist(freq$`round(length, digits = 1)`, breaks = 50)
abline(v = median(freq_weigth) );  median(freq_weigth)

freq_iso <- as.data.frame(count(df_evt,answer))
isotope <- dim(freq_iso)[1]
plot(2:isotope,freq_iso$n[2:dim(freq_iso)[1]] ); title("Frequency of Isotope")

freq_loc <- as.data.frame(count(df_evt,round(location,digits = 1)) )
names(freq_loc) <- c("location","count")
#freq_loc$location <- round(freq_loc$location, digits = 2)
#freq_loc <- as.data.frame(count(freq_loc, location))

location <- dim(freq_loc)[1]
plot(2:(location),freq_loc$count[2:dim(freq_loc)[1]] ); title("Frequency of Location as percentage - excluding BKG")

plot(1:144, event[72,1:144])

df_grp <- df_evt %>% group_by(answer)


for(i in 0:6){
  df_temp <- subset(df_evt,answer == i)
  freq_loc_temp <- as.data.frame(count(df_temp,round(location,digits = 1)) )
  names(freq_loc_temp) <- c("location","count")
  location <- dim(freq_loc_temp)[1]
  plot(2:(location),freq_loc_temp$count[2:dim(freq_loc_temp)[1]] )
  title(paste("Frequency of Location as percentage - Class",toString(i),sep=" "))
}

#### play area
library(truncnorm)
x <- rtruncnorm(10000,a=-3,b=4)
hist(x)
rnorm(x)

y <- sample(3:10,100, replace = TRUE)
hist(y)

library(data.table)
library(dplyr)
DT <- data.table(A=1:100, B=runif(100), Amount=runif(100, 0, 100))

DT %>% 
  group_by(gr=cut(B, breaks= seq(0, 1, by = 0.05)) ) %>% 
  summarise(n= n()) %>%
  arrange(as.numeric(gr))

junk <- data.frame(a=1:10, b=c("a","a","a","b","b","c","c","d","d","d"), 
                  c=c(1.2, 2.2, 2.4, 1.7, 2.7, 3.1, 3.2, 4.2, 3.3, 2.2), 
                  d= c("small", "med", "larg", "larg", "larg", "med", "small", "small", "small", "med"))

g <- junk %>% group_by(b) %>% filter(c == min(c))

g <- vector(); h <- vector()
for(i in 1:10000000){
  g[i] <- (as.integer( (sample(1:10,1, replace = TRUE)) ) )
#  h[i] <- (as.integer(( ((sample(1:1000,1, replace = TRUE) / 1000))) ) )
}
hist(g, breaks = 100)#; hist(f$g, breaks = 100)
