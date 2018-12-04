##  My Functions

# aggregate over mode of evtID
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# aggregate over mode w/o zeros unless all zeros
Mode.woZ <- function(x) {
  ux <- unique(x)
  h <- ux[which.max(tabulate(match(x, ux)))]
  if(h == "0" & !is.na(ux[2]) ){h <- ux[2]}
  return(h)
}

# df_data <- as.data.frame(matrix(c(sample(1:40, 108360, replace=T)),nrow = 903,ncol = 120))
# junk


## Turn integrated 0.25 second data into non-rolling 1 second data ##
## uses Hz is input bins is 
simple_sum <- function(df_data) {
#  summed_data <- t(sapply(split(data, rep(1:(dim(data)[1]/Hz), each=4)), colSums) )
  mod <- dim(df_data)[1] %/% 4 ## Hz = 4
  ch <- dim(df_data)[2]
  indx <- as.vector( rbind( c(1:mod), c(1:mod), c(1:mod), c(1:mod) )) ## 4 Hz
  
  df_temp <- cbind(df_data[1:(mod*4),],indx)
  
  df_new <- aggregate(x = df_temp[,1:ch], by = list(df_temp$indx), FUN = sum)
  df_new <- df_new[,-1]
}

## Turn integrated 0.125 second data into non-rolling 1 second data ##
## uses Hz is input bins is 
simple_sum8 <- function(df_data) {
  #  summed_data <- t(sapply(split(data, rep(1:(dim(data)[1]/Hz), each=4)), colSums) )
  mod <- dim(df_data)[1] %/% 8 ## Hz = 8
  ch <- dim(df_data)[2]
  indx <- as.vector( rbind( c(1:mod), c(1:mod), c(1:mod), c(1:mod), 
                            c(1:mod), c(1:mod), c(1:mod), c(1:mod) )) ## 8 Hz
  
  df_temp <- cbind(df_data[1:(mod*8),],indx)  ## 8 Hz
  
  df_new <- aggregate(x = df_temp[,1:ch], by = list(df_temp$indx), FUN = sum)
  df_new <- df_new[,-1]
}

################################################################
## Parsing from list mode data to integrated data

parsingData <- function(files = listData, type, bins, Hz = 4, Erange = 2650, start = 1, finish){

  input_path <- paste("data/", type, "Data/listData/", sep = "")
  output_path <- paste("data/",type,"Data/integrated_",as.character(bins),"Ch_",as.character(Hz),"Hz", sep="")
  
  int_length <- 1000000/Hz # integration window in microseconds
  energy_range <- Erange  # energy range
  
  st_time <- Sys.time()
  for(i in start:finish){
    if(type == "testing"){
      evtID <- paste("4",as.character(i),sep = "")  ## '4' for testing   '10' for training
      if(i < 10000){evtID <- paste("40",as.character(i),sep = "")}
      if(i < 1000){evtID <- paste("400",as.character(i),sep = "")}
      if(i < 100){evtID <- paste("4000",as.character(i),sep = "")}
      if(i < 10){evtID <- paste("40000",as.character(i),sep = "")}
      event <- read.csv(paste(input_path, "runID-", evtID, ".csv",sep = ""), header=FALSE, sep=",")  
    }else if(type == "training"){
      evtID <- paste("10",as.character(i),sep = "")  ## '4' for testing   '10' for training
      if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
      if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
      if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
      event <- read.csv(paste(input_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
    }else{
      print("Error type must be training or testing!")
      i <- finish
    }
    
    event[, 'spec_num'] <- cumsum(event[, 1]) %/% int_length
    event[, 'channel'] <- event[, 2] %/% (energy_range/bins)
    
    evts <- transform(subset(event)[,3:4], spec_num = as.integer(spec_num), 
                      channel = as.integer(channel))
    
    spectra <- count_(evts, c("spec_num","channel"))
    
    junk <- subset(spectra %>% spread(spec_num,n))[1:bins,]
    events_int <- (t(junk))[-1,]
    
    if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
    
    write.table(events_int, file = paste(output_path,evtID,".csv", sep = ""), 
                row.names=FALSE, col.names = FALSE, sep=",", na="0")
  }

}


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
input_path <- c("data/trainingData/integrated_56Ch25s/")
#input_path <- c("data/trainingData/integrated_32Ch125s/")
output_path <- c("data/trainingData/56Ch_1Sec/")
#output_path <- c("data/trainingData/56Ch_rolling/")  ## rolling

one_sec <- function(Ch, type, evt1st, evtLast){

  st_time <- Sys.time()
  for(i in evt1st:evtLast){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  # for(i in 4000:7999){
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
  evt_one <- simple_sum(event) # 4 hertz only
  ########  
  
  # ans <- answers$SourceTime[i]
  # ansID <- answers$SourceID[i]
  
  ##  This line for rolling sum
  #  evt_roll <- as.data.frame(rollapply(event[1:(dim(event)[1]),], 4, sum) )
  #####
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
  
  #  write.table(evt_roll, file = paste(output_path, evtID, ".csv",sep = ""),  ## rolling
  #              row.names=FALSE, col.names = FALSE, sep=",", na="0")
  write.table(evt_one, file = paste(output_path, evtID, ".csv",sep = ""),  ## simple sum
              row.names=FALSE, col.names = FALSE, sep=",", na="0")
  
}

}
