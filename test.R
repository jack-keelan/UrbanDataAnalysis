
Eng_Range <- 2650
nbins <- 30

width <- Eng_Range/nbins

mid_vec <- vector()

for(i in 1:nbins){
  mid <- ( (i*width) - (width/2) )
  mid_vec <- c(mid_vec,mid)
}

mid_vec

setwd("C:/Users/mooreet_la/projects/SDRD/competition/")

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas


# input128_path <- c("data/trainingData/128Ch_1Sec/")
# input144_path <- c("data/trainingData/144Ch_1Sec/")
# input64_path <- c("data/trainingData/64Ch_1Sec/")
# input32_path <- c("data/trainingData/32Ch_1Sec/")
# output_path <- c("data/trainingData/56Ch_1Sec/")

# input128_path <- c("data/trainingData/integrated_128Ch25s/")
# input32_path <- c("data/trainingData/integrated_32Ch25s/")
# output_path <- c("data/trainingData/integrated_56Ch25s/")

# input128_path <- c("data/testingData/integrated_128Ch25s/")
# input32_path <- c("data/testingData/integrated_32Ch25s/")
# output_path <- c("data/testingData/integrated_56Ch25s/")

# input128_path <- c("data/trainingData/integrated_128Ch125s/")
# input32_path <- c("data/trainingData/integrated_32Ch125s/")
# output_path <- c("data/trainingData/integrated_56Ch125s/")

input128_path <- c("data/testingData/integrated_128Ch_125s/")
input32_path <- c("data/testingData/integrated_32Ch_125s/")
output_path <- c("data/testingData/integrated_56Ch_125s/")

st_time <- Sys.time()

for(i in 1:15923){
  evtID <- paste("4",as.character(i),sep = "")  ## '4' for testing   '10' for training
  if(i < 10000){evtID <- paste("40",as.character(i),sep = "")}
  if(i < 1000){evtID <- paste("400",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("4000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("40000",as.character(i),sep = "")}
# for(i in 1:9800){
#   evtID <- paste("10",as.character(i),sep = "")
#   if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
#   if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
#   if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  

  event128 <- read.csv(paste(input128_path, evtID, ".csv",sep = ""),
                    header=FALSE, sep=",")
  event32 <- read.csv(paste(input32_path, evtID, ".csv",sep = ""),
                      header=FALSE, sep=",")

  
#  if(dim(event128)[1] > dim(event32)[1]){event128 <- event128[1:(dim(event128)[1]-1),]}
  comb_event <- cbind(event128[,1:32], event32[,9:32])
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
  write.table(comb_event, file = paste(output_path, evtID, ".csv",sep = ""),
              row.names=FALSE, col.names = FALSE, sep=",", na="0")

}

# event144 <- read.csv(paste(input144_path, evtID, ".csv",sep = ""),
#                      header=FALSE, sep=",")
# event64 <- read.csv(paste(input64_path, evtID, ".csv",sep = ""),
#                      header=FALSE, sep=",")

dim(event144); dim(event128); dim(event64); dim(event32)

plot(1:144, event144[39,]); plot(1:144, event144[40,])
plot(1:144, event144[41,]); plot(1:144, event144[42,])

plot(1:144, event144[68,]); plot(1:64, event64[67,])

plot(1:64, event64[39,]); plot(1:64, event64[40,])
plot(1:64, event64[41,]); plot(1:64, event64[42,])

event128 <- event128[1:(dim(event128)[1]-1),]
event144 <- event144[1:(dim(event144)[1]-1),]
comb_event <- cbind(event128[,1:32], event32[,9:32])

plot(1:56, comb_event[41,]); plot(1:56, comb_event[42,])
