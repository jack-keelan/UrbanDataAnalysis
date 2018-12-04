##############################################
##  This script is to evaluate performance of any given model.
##  See the model script for generation of model.
###########################################################
## Sections: ##
## Determine the longest and shortest runs
## reading in asymptotics from files
## creation & writing the asymptotics
## Scaling & plotting the asymptotics
## Kolmogorov–Smirnov test
## Anderson Darling test
## Make all plots for TAD each compared to the longest bkg run
## or make subset plots like fNeg double
## create combined data set for building statistics (df.comb_data)
##  Bayes analysis
## main subsetting of data
## compare the two method stats
## examine the CNN false negatives in detail
## Look at all second choices
## plots specific to this 144x144 CNN

library(dplyr)
library(lattice)
#library(plyr)
setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")

## FUCNTIONS

## calculate norms 
Calc_norm <- function(x, bins = as.integer(dim(x)[2]) ) {
  norms <- rowSums(x[,1:bins])
}


########    get data
#  really only need the column names
#df.mod <- read.csv("data/TNG_128Ch_selected.csv")
df.mod <- read.csv("data/TNG_56Ch.csv")

## read in model of interest
#mod <- readRDS("RF_56Ch_1sec_FULL_50per.rds") ## raw model
mod <- readRDS("RF_56Ch_1sec_50per.rds")  ## response model
#mod <- readRDS("RF_50per_128ch.rds")  ## response model
nbins <- 56

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas

###############################################################
# Create predictions test example dummy  TNG_set128_roll_25s.csv
#data_path <- "data/integrated_128Ch_3sec/"
#data_path <- "data/integrated_128Ch/"
#data_path <- "data/integrated_128Ch_bkgR/"
#data_path <- "data/128Ch_rolling/"
#data_path <- "data/trainingData/32Ch_rolling/"
data_path <- "data/trainingData/56Ch_rolling/"
#data_path <- "data/trainingData/56Ch_rolling_4sec/"
#data_path <- "data/trainingData/56Ch_rolling_2sec_125s/"
intrv <- 4  ## number of quarter seconds in the integration window

vec_loc <- as.numeric(vector() )
vec_ans <- as.integer(vector() )
vec_lng <- as.integer(vector() )

st_time <- Sys.time()

for(i in 5400:7200){
  #for(i in 4901:9800){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  dummy <- read.csv(paste(data_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  dummy <- dummy * 4. / intrv  # scale to match the training
  
  names(dummy) <- colnames(df.mod[1:nbins])
  
  #  preds_dummy <- predict(rf.cv, dummy, type = "raw")  # 3 seconds
  preds_dummy <- predict(mod, dummy, type = "response")  # 1 second
  #  preds_dummy <- predict(rf.cv, dummy, type = "raw")   #current bkg rm'ed
  preds_dummy[1:30] <- 0   # the first 30 second never have a source
  
  df_dummy <- data.frame(matrix(ncol = 2, nrow = length(preds_dummy)))
  names(df_dummy) <- c("second","prediction")
  df_dummy$second <- c(1:length(preds_dummy))
  df_dummy$prediction <- preds_dummy
  
  ##  need to set a threshold for true prediction
  stats_dummy <- as.data.frame(count(df_dummy, prediction) )
  s <- stats_dummy[-1,]
  b <- which(s$n %in% c(max(s$n)))
  d <- s$prediction[b]
  
  
  if(max(s$n) >= 5 ){             ##  4 for the 3 sec integr set to 7 for 128Ch
    ans_dummy <- as.integer(d) - 1
    loc_dummy <- round(median( which(as.integer(preds_dummy) %in% c(as.integer(d))) ),2)
    vec_loc[(i-5400)] <- (((intrv*loc_dummy) + (1-intrv)*trunc(loc_dummy) + (intrv-1) )/4)
  } else {
    ans_dummy <- 0
    loc_dummy <- 0.00
    vec_loc[(i-5400)] <- loc_dummy
  }
  
  #  vec_loc[(i-4900)] <- loc_dummy
  vec_ans[(i-5400)] <- ans_dummy
  vec_lng[(i-5400)] <- max(s$n)
  
  
  
  #mask_dummy <- as.integer(preds_dummy) == ans_dummy
  #length( which(as.integer(preds_dummy) %in% c(ans_dummy)) )
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time); print(max(s$n))}
}

########  compare to real answers
##  Get answers for comparision to prediction
true_test <- answers[5401:7200,]

true_val <- as.factor(true_test$SourceID)   # need to use factors
true_loc <- round(true_test$SourceTime)
true_loc <- (true_loc %/% 10) * 10    # approximate how good is good on location

## write out the predicted answers
df.output <- true_test
df.output$oneD_RF_pred <- vec_ans #(1:length(vec_ans))
df.output$oneD_RF_pred_loc <- vec_loc
write.table(df.output, file = "results/56Ch_rolling_against_TNG_56Ch.csv", 
            row.names=FALSE, col.names = TRUE, sep=",", na="0")

plot(df.output$SourceTime,df.output$oneD_RF_pred_loc)
abline(a=0,b=1)
################################################################################
confMTX.evts <- confusionMatrix(as.factor(vec_ans),true_val)
confMTX.evts
vec_locV2 <- (vec_loc %/% 10) * 10
compare.loc <- as.data.frame(cbind(vec_locV2,true_loc))
compare.loc$diff <- true_loc - vec_locV2
stats_comp <- as.data.frame(count(compare.loc,diff) )
histogram(stats_comp$n)
plot(stats_comp$diff,stats_comp$n)
# compare2.loc <- cbind(compare.loc,vec_ans)
# temp.loc <- subset(compare2.loc, abs(compare.loc$diff) > 50)
# temp2.loc <- subset(temp.loc, temp.loc$vec_ans != 0)
#View(temp.loc)
#####
sum(stats_comp$n)
stats_comp$diff
sum(stats_comp$n) - stats_comp$n[2]
sum(stats_comp$n) - stats_comp$n[43]

################################################################################
##  fancy display
# create confusion
#caret::confusionMatrix(pred, truth)
#r <- confusionMatrix(pred, truth)
confusion <- as.data.frame(as.table(confMTX.evts$table[1:7,1:7]))
d1 <- rep(800, times = 42); d2 <- rep(5000, times = 7); d <- c(d1,d2)
confusion$Percentage <- round( (confusion$Freq) * 100/d, digits = 2)

confusion[confusion == 0] <- 0.01 # trick the display
colP <- brewer.pal(9,"Greens")
breaks <- c(1,10, 100, 1000)
per_breaks <- c(0.1,1, 10,70)
## plot confusion matrix frequency
plot <- ggplot(data = confusion, mapping = aes(x=Reference, y=Prediction))
plot + geom_tile(aes(fill=Freq), colour = "white" ) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), colour = "orange", vjust = 1) +
  scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(trans = "log", low = "white", high = "darkblue", 
                      #                     breaks=seq(from=0, to=5000, by=1000)) +
                      breaks = breaks, labels = breaks) +
  labs(fill="Frequency") 


## plot confusion matrix
plot <- ggplot(data = confusion, mapping = aes(x=Reference, y=Prediction))
plot + geom_tile(aes(fill=Percentage), colour = "white" ) +
  geom_text(aes(label = sprintf("%1.1f", Percentage)), colour = "orange", vjust = 1) +
  scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(trans = "log",low = "white", high = "darkgreen", 
                      #                      breaks=seq(from=0, to=100, by=10)) +
                      breaks = per_breaks, labels = per_breaks) +
  labs(fill="Percentage") 

######################################################################################
## answers for both the Random Forect and the CNN exist & are in the files specified
## read them in and examine them in detail
## for referrence classes 0-6 are {background, HEU, WGPu, I, Co, Tc, Tc&HEU}
########################################

#runNum <- 100002 Figure 1
#runNum <- 100014
#runID <- "100014"
# df.raw <- read.csv(paste("results/56Ch_rolling/", runID, ".csv", sep = "") )
# bins <- 56; Hz <- 4


###################################################
##  Determine the longest and shortest runs
longest <- 0
#longest <- shortest <- 700
# for(j in 1:dim(junk5)[1]){
#   run <- as.character(junk5$RunID[j])
#   df.temp <- read.csv(paste("data/128Ch_1sec/", run, ".csv", sep = "") )
#   if(as.integer(dim(df.temp)[1]) > longest){longest <- as.integer(dim(df.temp)[1]); cnt <- j}
# }
# runNum <- 106310  longest
# runNum <- 106399  shortest
##
##  Determine the longest and shortest runs
###########################################################
## reading in asymptotics from files

#str_id <- c("background", "HEU", "WGPu", "I", "Co", "Tc", "^99m Tc & HEU")

bins <- 128; Hz <- 1
bins <- 144; Hz <- 1
asy_class <- read.csv(paste("data/asy_class_",bins,".csv",sep="") )#,row.names = FALSE)
bins <- 56; Hz <- 1
bins <- 32; Hz <- 1
## reading in asymptotics from files
###########################################################

# bins <- 144; Hz <- 1
setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")
answers_raw <- read.csv("answers.csv")
# answers_raw <- read.csv("new_answers.csv")

#############################################################
##  Training Asymtotics (creation) - writing the asymptotics
asy_spec <- data.frame(matrix(ncol = bins, nrow = 7200))
asy_spec$SourceID <- NA 
for(i in 1:7200){
  runNum <- i
  runID <- as.character(runNum+100000)
  setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")
#  df.raw <- read.csv(paste("data/integrated_128Ch/", runID, ".csv", sep = "") )
  df.raw <- read.csv(paste("data/",bins,"Ch_1sec/", runID, ".csv", sep = "") )
  ans_id <- answers_raw$SourceID[runNum]
  ans_loc <- answers_raw$SourceTime[runNum]
  ans_loc <- as.integer(round(ans_loc, digits = 0) ) * Hz
 
  spec_src = vector() 
  if(ans_id == 0){
    ans_loc <- as.integer(round(dim(df.raw)[1]/2, digits = 0) ) * Hz
    spec_src[1:bins] <- colSums(df.raw[(ans_loc-10):(ans_loc+10),])
  } else{
    spec_src[1:bins] <- colSums(df.raw[(ans_loc-2):(ans_loc+2),])    
  }
  
  asy_spec[i,1:bins] <- spec_src
  asy_spec[i,(bins+1)] <- ans_id
  
  if(i %% 200 == 0){print(i)}
}

asy_class <- data.frame(matrix(ncol = bins, nrow = 7))
asy_class$SourceID <- NA 

for(i in 1:6){
#  k <- i+1
  df_temp <- subset(asy_spec, asy_spec$SourceID == i)
  df_temp$SourceID <- NULL
  asy_class[i,1:bins] <- colSums( df_temp )
  asy_class$SourceID[i] <- i 
}
df_temp <- subset(asy_spec, asy_spec$SourceID == 0)
df_temp$SourceID <- NULL
asy_class[7,1:bins] <- colSums( df_temp )
asy_class$SourceID[7] <- 0 

plot(1:bins,asy_class[1,1:bins])
plot(1:bins,asy_class[4,1:bins])

names(asy_class) <- c(1:bins,"SourceID")
write.csv(asy_class,paste("data/asy_class_",bins,".csv",sep=""),row.names = FALSE)

##  Training Asymtotics (creation) - writing the asymptotics
#############################################################

####################################################
## Scaling & plotting the asymptotics
norms <- rowSums(asy_class[,1:bins])
max_asy_bkg <- max(asy_class[7,1:bins])

j=5
max_asy_src <- max(asy_class[j,1:bins])
#scaleFact_asy <- max_asy_src/max_asy_bkg
#(128/2650) *609
(bins/2650) *1461
scaleFact_asy <- asy_class[j,71]/asy_class[7,71]

df1_t <- as.data.frame(t(asy_class[7,1:bins]) * scaleFact_asy ); df2_t <- as.data.frame(t(asy_class[j,1:bins]) )
df1_t$type <- c("Background"); df2_t$type <- c("Source")
colnames(df1_t) <- c("Normalized_Counts", "Type"); colnames(df2_t) <- c("Normalized_Counts", "Type")

df1_t$sd <- (sqrt( t(asy_class[7,1:bins]) )/(t(asy_class[7,1:bins])) )*df1_t$Normalized_Counts
df2_t$sd <- sqrt( df2_t$Normalized_Counts )
row.names(df1_t) <- row.names(df2_t) <- c(1:bins)
#identical(names(df1_t),names(df2_t))

df2_asy <- data.frame(matrix(ncol = 3, nrow = (bins*2)))
names(df2_asy) <- names(df1_t)

df2_asy[1:bins,] <- df1_t[,]; df2_asy[(bins+1):(bins*2),] <- df2_t[,]

df2_asy$Channel <- c(1:bins)
df2_asy$Energy <- df2_asy$Channel * (2650/bins)

#iso_line <- 400; iso_line2 <- 700
#iso_line <- 186; iso_line2 <- 1001
iso_line <- 141; iso_line2 <- 322
iso_line <- 400; iso_line2 <- 700
iso_line <- 1173; iso_line2 <- 1330
iso_line <- 141; iso_line2 <- 186
p_asy_Tc99m <- ggplot(df2_asy, aes(x=Energy, y=Normalized_Counts, group=Type, color=Type)) +    ## PAPER FIGURE 1??
  geom_line() +
  geom_pointrange(aes(ymin=Normalized_Counts-sd, ymax=Normalized_Counts+sd)) +
  scale_y_log10(breaks = c( 10, 100, 1000, 10000, 100000), limits = c(100, 450000)) + ## 5473 & 5479
#  scale_y_continuous(breaks = c( 10, 100, 1000, 10000, 100000), limits = c(100, 450000)) + ## 5473 & 5479
  scale_x_continuous(breaks = c( 0, 186, 609, 1120, 1460, 2000, 2600), limits = c(-2, 2700)) + ## 5473 & 5479
  #  scale_y_log10(breaks = c( 1, 10, 100, 1000, 10000), limits = c(8, 20000)) +
  geom_vline(xintercept=iso_line) +
  geom_vline(xintercept=iso_line2) +
  geom_vline(xintercept=1461, linetype = "dashed") +  ##1461
  geom_vline(xintercept=609, linetype = "dashed") +  ##609
  geom_vline(xintercept=1120, linetype = "dashed") +  ##1120
  geom_vline(xintercept=2614, linetype = "dashed") +  ##2614
  ggtitle(paste("Source Asymptotic Tc99m")) +
#  ggtitle(paste("Source Asymptotic HEU")) +
  ylab("Counts") +
  xlab("Energy (KeV)") +
  theme_bw()

p_asy_HEU
p_asy_Tc99m
p_asy_HEU_Tc99m
p_asy_WGPu
p_asy_I
p_asy_Co

class_type <- c("HEU","Tc99m","HEU_Tc99m","WGPu","I","Co")
plot_type <- list(p_asy_HEU,p_asy_Tc99m,p_asy_HEU_Tc99m,p_asy_WGPu,p_asy_I,p_asy_Co)

setwd(paste("C:/Users/mooreet/Desktop/current/plots/asymptotic/",sep = "") )
for(i in 1:6){
  pplot <- plot_type[i]
  rplot <- paste('asymp_',class_type[i], '.png',sep='')
  png(rplot, width = 600, height = 480)
  print(pplot)
  dev.off()
}

## Scaling & plotting the asymptotics
#####################################################

######################################################
## Kolmogorov–Smirnov test
## Anderson Darling test
library(kSamples)
df.CNN_success <- subset(df.comb_data, df.comb_data$twoD_CNN_pred == df.comb_data$SourceID)
df.CNN_failure <- subset(df.comb_data, df.comb_data$twoD_CNN_pred != df.comb_data$SourceID)

#for(j in 1:dim(df.fNeg_double)[1]){
# for(j in 1:dim(df.CNN_failure)[1]){
#   runNum <- df.CNN_failure$RunID[j]
#for(j in 1:dim(df.CNN_success)[1]){
KS_pVal = as.double(vector()); AD_ver1 = as.double(vector())
KS_stat = as.double(vector()); AD_ver2 = as.double(vector())
KS_ans = as.double(vector()); AD_ans = as.double(vector())
KS_stat_success <- data.frame(matrix(ncol = 7, nrow = 9800))
KS_pVal_success <- data.frame(matrix(ncol = 7, nrow = 9800))
AD_ver1_tot <- data.frame(matrix(ncol = 7, nrow = 9800))
AD_ver2_tot <- data.frame(matrix(ncol = 7, nrow = 9800))
new_norm <- norms
binNum <- bins - 60
#bins <- 144

for(j in 1:9800){
#  runNum <- df.CNN_success$RunID[j]
  runNum <- j
  runID <- as.character(runNum+100000)#; runNum <- runNum-100000
  
  setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")
#  df.raw <- read.csv(paste("data/integrated_128Ch/", runID, ".csv", sep = "") )
  df.raw <- read.csv(paste("data/",bins,"Ch_1sec/", runID, ".csv", sep = "") )
#  print(runID)
  ans_id <- answers_raw$SourceID[runNum]
  ans_loc <- answers_raw$SourceTime[runNum]
  ans_loc <- as.integer(round(ans_loc, digits = 0) ) * Hz

  spec_src = vector()
  if(ans_id == 0){ans_loc <- as.integer(round(dim(df.raw)[1]/2, digits = 0) ) * Hz}
  spec_src[1:bins] <- colSums(df.raw[(ans_loc-2):(ans_loc+2),])
  normFac <- sum(spec_src[1:binNum])
  new_norm <- norms/normFac
#  new_norm[7] <- new_norm[7]/20

  for(i in 1:7){
    ks_temp <- ks.test(spec_src[1:binNum], as.double(asy_class[i,1:binNum]/(new_norm[i]*1) ) )
    KS_stat[i] <- as.double(ks_temp$statistic); KS_pVal[i] <- as.double(ks_temp$p.value)
    ad_temp <- ad.test(spec_src[1:binNum], as.double(asy_class[i,1:binNum]/(new_norm[i]*1) ))
    AD_ver1[i] <- as.double(ad_temp$ad[1,3]); AD_ver2[i] <- as.double(ad_temp$ad[2,3]) 
  }
#  KS_test_failure <- as.data.frame(cbind(KS_stat,KS_pVal) )
#  KS_test_success[j,] <- as.data.frame(cbind(KS_stat[1:7],KS_pVal[1:7]) )
  KS_stat_success[j,] <- KS_stat
  KS_pVal_success[j,] <- KS_pVal
  KS_ans[j] <- ans_id
  
  AD_ver1_tot[j,] <- AD_ver1
  AD_ver2_tot[j,] <- AD_ver2
  AD_ans[j] <- ans_id

  if(j %% 200 == 0){print(j)}
#  print(j)
}

a1 <- cbind(answers_raw[1:9800,1:2], KS_pVal_success[1:9800,])
colnames(a1) <- c("run","label","HEU","WGPu","I131","Co60","Tc99m","HEU&Tc99m","Background")
a_bkg <- subset(a1, a1$SourceID == 0)
a_HEU <- subset(a1, a1$SourceID == 1)
a_WGPu <- subset(a1, a1$label == 2)

df_WGPu <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_WGPu) <- c("run","label","pVal","Class")
for(k in 3:9){
  df_temp <- a_WGPu[,c(1:2,k)]
  df_temp$Class <- names(a1)[k]
  colnames(df_temp) <- c("run","label","pVal","Class")
  df_WGPu <- rbind(df_WGPu,df_temp)
}

a_I <- subset(a1, a1$SourceID == 3)
a_Co <- subset(a1, a1$SourceID == 4)
a_Tc <- subset(a1, a1$SourceID == 5)
a_HEU_Tc <- subset(a1, a1$SourceID == 6)

histogram(a_WGPu$Co60, breaks = 30)

p_WGPu_KS <- ggplot(data = df_WGPu, aes(x=pVal, fill=Class) ) +    ## PAPER FIGURE 1?
  
  geom_histogram(alpha = 0.5) +
  # geom_histogram(aes(y =(..count..)/sum(..count..)*100 ), 
  #                breaks=seq(0, 1, by = 0.1), 
  #                col="green", 
  #                fill="green", 
  #                alpha = .2) + 
  scale_fill_manual(name="pVal",values=c("red","blue","yellow","green","darkblue","darkred","darkgray"),labels=c("a","b","c","d","e","f","g"))
  labs(title="Histogram for WGPu") +
  labs(x="p-value", y="Percent of Total")

plo <-  ggplot(df_WGPu, aes(x = pVal, colour = Class)) +
  geom_histogram(aes(y =(..count..)/sum(..count..)*100 ) ) +
  scale_colour_manual(values = c("Background" = "mediumpurple1", "HEU" = "mediumpurple4",
                                 "WGPu" = "darkred", "I131" = "darkblue", "Co60" = "green",
                                 "Tc99m" = "darkgreen","HEU&Tc99m" = "yellow"), name = "class" ) +
  scale_fill_manual(values = c("Background" = "mediumpurple1", "HEU" = "mediumpurple4",
                                 "WGPu" = "darkred", "I131" = "darkblue", "Co60" = "green",
                                 "Tc99m" = "darkgreen","HEU&Tc99m" = "yellow"), name = "class" ) +
  labs(title="Histogram for WGPu") +
  labs(x="p-value", y="Percent of Total")

dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ploo <- ggplot(df_WGPu,aes(x=pVal, fill = Class , y = ( (..count..)/sum(..count..)*100) ) ) +
#  geom_histogram(aes(y =(..count..)/sum(..count..)*100 ) ) +
#  geom_histogram(data=subset(df_WGPu, Class == 'HEU'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(df_WGPu, Class == 'WGPu'),fill = "blue", alpha = 0.2, 
                 bins = 17, position = "dodge2") +
#  geom_bar(position = 'dodge', stat = 'identity', linetype = 0) +
   geom_histogram(data=subset(df_WGPu, Class == 'I131'),fill = "green", alpha = 0.4,
      bins = 32)+#, position = "dodge2") +
#   geom_histogram(data=subset(df_WGPu, Class == 'Co60'),fill = "darkred", alpha = 0.2, bins = 35) +
   geom_histogram(data=subset(df_WGPu, Class == 'Tc99m'),fill = "red", alpha = 0.2,
                                   bins = 25) +#, position = "dodge2") +
  # geom_histogram(data=subset(df_WGPu, Class == 'HEU&Tc99m'),fill = "purple", alpha = 0.2) +
#  geom_histogram(data=subset(df_WGPu, Class == 'Background'),fill = "red", alpha = 0.2, 
#                 bins = 25) +#, position = "dodge2") +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
labs(title="KS of WGPu against (WGPu, blue), (Tc99m, red), (I-131, green)") +
  labs(x="p-value", y="Percent of Total")




ggplotly()

dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(dat,aes(x=xx)) +
  geom_histogram(data=subset(dat,yy == 'a'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'b'),fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)

ggplotly()


w <- vector();w2 <- vector(); z <- vector()
for(i in 1:9800){
#  v <- KS_stat_success[i,]
  v <- AD_ver1_tot[i,]
  w[i] <- which(v==max(v))
#  v2 <- KS_pVal_success[i,]
  v2 <- AD_ver2_tot[i,]
  w2[i] <- which(v2==max(v2))
  z[i] <- max(v2)
}
fg <- as.data.frame(w)
fg <- cbind(fg,w2,KS_ans[1:9800],z)
#fg <- cbind(fg,answers_raw$SourceID[1:200])
colnames(fg) <- c("stat","prob","label","pVal")

fg[fg==7]<-0
fg2 <- subset(fg, fg$stat == fg$label)
fg3 <- subset(fg, fg$prob != fg$label)
fg4 <- subset(fg, fg$prob == fg$stat)
dim(fg);dim(fg2);dim(fg3);dim(fg4)
non_bkg_KS <- subset(fg2, fg2$label != 0)

colnames(fg) <- c("ver1","ver2","label","AD_pVal_ver2")
fg[fg==7]<-0
fg2 <- subset(fg, fg$ver1 == fg$label)
fg3 <- subset(fg, fg$ver2 == fg$label)
fg4 <- subset(fg, fg$ver1 == fg$ver2)
dim(fg);dim(fg2);dim(fg3);dim(fg4)
non_bkg_AD2 <- subset(fg3, fg3$label == 0)
non_bkg_AD1 <- subset(fg2, fg2$label == 0)

## Kolmogorov–Smirnov test
## Anderson Darling test
####################################################
## Make all plots for TAD each compared to the longest bkg run
## or make subset plots like fNeg double
library(ggplot2)

df.CNN_success <- subset(df.comb_data, df.comb_data$twoD_CNN_pred == df.comb_data$SourceID)
df.CNN_failure <- subset(df.comb_data, df.comb_data$twoD_CNN_pred != df.comb_data$SourceID)
df.CNN_failure_only <- subset(df.CNN_failure, df.CNN_failure$oneD_RF_pred == df.CNN_failure$SourceID)

runs_120x144_CNNonly_failure <- df.CNN_failure_only$RunID
stats_120x144_CNN_fail_true <- as.data.frame(count(df.CNN_failure_only, SourceID))
stats_120x144_CNN_fail_pred <- as.data.frame(count(df.CNN_failure_only, twoD_CNN_pred ))
runs_144x144_CNNonly_failure <- df.CNN_failure_only$RunID
stats_144x144_CNN_fail_true <- as.data.frame(count(df.CNN_failure_only, SourceID))
stats_144x144_CNN_fail_pred <- as.data.frame(count(df.CNN_failure_only, twoD_CNN_pred ))
int_CNN_only_failure <- intersect(runs_120x144_CNNonly_failure,runs_144x144_CNNonly_failure)
length(int_CNN_only_failure)
stats_120x144_CNN_fail_true;stats_144x144_CNN_fail_true
stats_120x144_CNN_fail_pred;stats_144x144_CNN_fail_pred
int_CNN_only_failure

bins <- 128; Hz <- 1
setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")
asy_class <- read.csv(paste("data/asy_class_",bins,".csv",sep="") )#,row.names = FALSE)

st_time <- Sys.time()
#for(j in 1:100){
#for(j in 1:dim(df.CNN_success)[1]){
#  runNum <- df.CNN_success$RunID[j]
for(j in 1:dim(df.CNN_failure_only)[1]){
  runNum <- df.CNN_failure_only$RunID[j]
# for(j in 1:dim(df.fNeg_double)[1]){
#   runNum <- df.fNeg_double$RunID[j]  # PAPER FIGURE 5
  runID <- as.character(runNum); runNum <- runNum-100000

  setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")
  df.raw <- read.csv(paste("data/",bins,"Ch_1sec/", runID, ".csv", sep = "") )
  
  ans_CNN <- df.CNN_failure_only$twoD_CNN_pred[j]
  ans_id <- answers_raw$SourceID[runNum]
  ans_loc <- answers_raw$SourceTime[runNum]
  ans_loc <- as.integer(round(ans_loc, digits = 0) ) * Hz
#  if(ans_id == 0){ans_loc <- as.integer(round(dim(df.raw)[1]/2, digits = 0) ) * Hz}

  if(ans_id == 0){
    spec_src = vector()
    spec_src[1:bins] <- colSums(df.raw[,])*(5/dim(df.raw)[1] )
  } else {
    spec_src = vector()
    spec_src[1:bins] <- colSums(df.raw[(ans_loc-2):(ans_loc+2),])
  }

  if(ans_id == 1){
    scaleFact_bkg <- sum(spec_src[67:72])/sum(asy_class[7,67:72])
    scaleFact_asy <- sum(spec_src[67:72])/sum(asy_class[ans_id,67:72]) 
  } else if(ans_id == 4){
    scaleFact_bkg <- sum(spec_src[70:80])/sum(asy_class[7,70:80])
    scaleFact_asy <- sum(spec_src[70:80])/sum(asy_class[ans_id,70:80])
  } else if(ans_id == 0){
    scaleFact_asy <- scaleFact_bkg <- sum(spec_src[45:80])/sum(asy_class[7,45:80])
  } else {
    scaleFact_bkg <- sum(spec_src[45:80])/sum(asy_class[7,45:80])
    scaleFact_asy <- sum(spec_src[45:80])/sum(asy_class[ans_id,45:80])
  }
  
  df1_t <- as.data.frame(t(asy_class[7,1:bins]) * scaleFact_bkg)
  if(ans_id == 0){
    df2_t <- df1_t
  } else{
    df2_t <- as.data.frame(t(asy_class[ans_id,1:bins]) * scaleFact_asy)
  }
  df3_t <- as.data.frame(spec_src)
  df1_t$type <- c("Background"); df2_t$type <- c("Source"); df3_t$type <- c("Event")
  colnames(df1_t) <- c("Normalized_Counts", "Type")
  colnames(df2_t) <- c("Normalized_Counts", "Type")
  colnames(df3_t) <- c("Normalized_Counts", "Type")
  df1_t$sd <- (sqrt(t(asy_class[7,1:bins]) )/t(asy_class[7,1:bins]) )*df1_t$Normalized_Counts
  if(ans_id == 0){
    df2_t <- df1_t
  } else{
    df2_t$sd <- (sqrt(t(asy_class[ans_id,1:bins]) )/t(asy_class[ans_id,1:bins]) )*df2_t$Normalized_Counts
  }
  df3_t$sd <- sqrt(df3_t$Normalized_Counts)
  
  df2 <- as.data.frame(matrix(nrow = bins*3, ncol = 3)); colnames(df2) <- names(df1_t)
  df2[1:bins,] <- df1_t[1:bins,]; df2[(bins+1):(bins*2),] <- df2_t[1:bins,]
  df2[((bins*2)+1):(bins*3),] <- df3_t[1:bins,]
  df2$Channel <- c(1:bins)
  df2$Energy <- df2$Channel * (2650/bins)
#df2$sd <- sqrt(df2$Normalized_Counts)
# df2$sd_s <- sqrt(df2$Source)

if(ans_id == 2){  ## WGPu
  iso_line <- 700
  iso_line2 <- 400
  x_limits = c(-2,1000); y_limits = c(5,1000)
  p_DFNeg <- ggplot(df2, aes(x=Energy, y=Normalized_Counts, group=Type, color=Type)) +    ## PAPER FIGURE 1?
    geom_line() +
    geom_pointrange(aes(ymin=Normalized_Counts-sd, ymax=Normalized_Counts+sd)) +
    scale_y_log10(breaks = c( 0.01, 0.1, 1, 10, 100, 1000, 10000), limits = y_limits) +
    scale_x_continuous(breaks = c( 0, 186, 609, 1120, 1460, 2000, 2600), limits = x_limits) + 
    #  scale_y_log10(breaks = c( 1, 10, 100, 1000, 10000), limits = c(8, 20000)) +
    geom_vline(xintercept=iso_line) +
    geom_vline(xintercept=iso_line2) +
    geom_vline(xintercept=1461, linetype = "dashed") +  ##1461
    geom_vline(xintercept=609, linetype = "dashed") +  ##609
    geom_vline(xintercept=1120, linetype = "dashed") +  ##1120
    geom_vline(xintercept=2614, linetype = "dashed") +  ##2614
    # geom_vline(xintercept=70.57, linetype = "dashed") +  ##1461
    # geom_vline(xintercept=29.42, linetype = "dashed") +  ##609
    # geom_vline(xintercept=54.1, linetype = "dashed") +  ##1120
    # geom_vline(xintercept=126.26, linetype = "dashed") +  ##2614
#    ggtitle(paste("CNN Success",runNum,"Class ", ans_id, sep = " ")) +
    ggtitle(paste("CNN only Fail as(",ans_CNN,")",runNum,"Class ", ans_id, sep = " ")) +
#    ggtitle(paste("Double False Negative",runNum,sep = " ")) +
    ylab("Counts") +
    xlab("Energy (KeV)") +
    theme_bw()
#  iso_line2 <- iso_line2 *(bins/2650)
  
} else{
  iso_line <- 1461
  if(ans_id == 0){iso_line <- 1461; x_limits = c(-2,2700); y_limits = c(0.1,1000)}  #HEU
  if(ans_id == 1){iso_line <- 186; x_limits = c(-2,2700); y_limits = c(0.1,1000)}  #HEU
  if(ans_id == 6){iso_line <- 90; x_limits = c(-2,2700); y_limits = c(0.1,1000)}  #HEU & Tc
  if(ans_id == 4){iso_line <- (1173 + 1332.5)/2; x_limits = c(-2,1600); y_limits = c(1,1000)}  ###1173 1332.5  Co60
  if(ans_id == 3){iso_line <- 364; x_limits = c(-2,1000); y_limits = c(5,1000)}  #i 131
  if(ans_id == 5){iso_line <- 141; x_limits = c(-2,500); y_limits = c(10,1000)} # Tc 99m
  p_DFNeg <- ggplot(df2, aes(x=Energy, y=Normalized_Counts, group=Type, color=Type)) +    ## PAPER FIGURE 1??
    geom_line() +
    geom_pointrange(aes(ymin=Normalized_Counts-sd, ymax=Normalized_Counts+sd)) +
    scale_y_log10(breaks = c( 0.01, 0.1, 1, 10, 100, 1000, 10000), limits = y_limits) + ## 5473 & 5479
    scale_x_continuous(breaks = c( 0, 186, 609, 1120, 1460, 2000, 2600), limits = x_limits) + ## 5473 & 5479
    geom_vline(xintercept=iso_line) +
    #  geom_vline(xintercept=iso_line2) +
    geom_vline(xintercept=1461, linetype = "dashed") +  ##1461
    geom_vline(xintercept=609, linetype = "dashed") +  ##609
    geom_vline(xintercept=1120, linetype = "dashed") +  ##1120
    geom_vline(xintercept=2614, linetype = "dashed") +  ##2614
#    ggtitle(paste("Double False Negative",runNum,sep = " ")) +
#    ggtitle(paste("CNN Success",runNum,"Class ", ans_id, sep = " ")) +
    ggtitle(paste("CNN only Fail as(",ans_CNN,")",runNum,"Class ", ans_id, sep = " ")) +
    ylab("Counts") +
    xlab("Energy (KeV)") +
    theme_bw() 
}

  # setwd(paste("C:/Users/mooreet/Desktop/current/plots/CNN_success/Class_",ans_id,"/",sep = "") )
  # rplot <- paste('CNN_success',runNum, '.png',sep='')
  setwd(paste("C:/Users/mooreet/Desktop/current/plots/CNN_failure_only_120x144/Class_",ans_id,"/",sep = "") )
  rplot <- paste('CNN_failure_only',runNum, '.png',sep='')
  # setwd(paste("C:/Users/mooreet/Desktop/current/plots/double_FNeg/Class_",ans_id,"/",sep = "") )
  # rplot <- paste('CNN_fNeg_double',runNum, '.png',sep='')
  png(rplot, width = 600, height = 480)
  print(p_DFNeg)
  dev.off()

  if(j %% 100 == 0){cur_time <- Sys.time() - st_time; print(j); print(cur_time)}
}
## Make all plots for TAD each compared to the longest bkg run
## or make subset plots like fNeg double
##################################################################

## visualization KS test
# Library
library(plotly)

# Overlaid histogram of 2 vectors:
f_t <- list(
  family = "sans serif, monospace",
  size = 16,
  color = "blue"
)
x_t <- list(
  title = "P-values",
  titlefont = f_t
)
y_t <- list(
  title = "Counts",
  titlefont = f_t
)
x_t2 <- list(
  title = "Statistic",
  titlefont = f_t
)

graph=plot_ly(x = KS_test_success$KS_pVal, opacity = 0.6, type = "histogram", name = 'proper classification') %>%
  add_trace(x = KS_test_failure$KS_pVal, name = 'misclassified') %>%
  layout(legend = list(orientation = 'h')) %>%
  layout(title = "Distribution Kolmogorov–Smirnov test") %>%
  layout(xaxis = x_t, yaxis = y_t) %>%
  layout(barmode="overlay")
graph

graph2=plot_ly(x = KS_test_success$KS_stat, opacity = 0.6, type = "histogram", name = 'proper classification') %>%
  add_trace(x = KS_test_failure$KS_stat, name = 'misclassified') %>%
  layout(legend = list(orientation = 'h')) %>%
  layout(title = "Distribution Kolmogorov–Smirnov test") %>%
  layout(xaxis = x_t2, yaxis = y_t) %>%
  layout(barmode="overlay")
graph2

plot(KS_test_success$KS_stat,KS_test_success$KS_pVal)
plot(KS_test_failure$KS_stat,KS_test_failure$KS_pVal)

###############################################################################
gross_cnts <- as.data.frame(rowSums(df.raw) )
gross_cnts$Time <- c(1:dim(df.raw)[1])
colnames(gross_cnts) <- c("cnts","Time")
gross_cnts$loc <- ans_loc
plot(1:dim(df.raw)[1], gross_cnts$cnts)
abline(v=ans_loc)

gross_p <- ggplot(gross_cnts, aes(x=Time,y=cnts) ) +
  geom_point() +
#  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(data=gross_cnts, aes(xintercept=loc),
             linetype="dashed", size=1, colour="red")


gross_p
gross_p <- ggplotly(gross_p)    ## PAPER FIGURE 2


d <- density(spec_bkg[1:bins]); plot(d, xlim=range(1:1280))
###################################################################################
###################################################################################
## create combined data set for building statistics (df.comb_data)

setwd("C:/Users/mooreet/Documents/projects/SDRD/competition/")

## these are the RF & CNN predictions for events 5401 thru 7200
df.1D_data <- read.csv("results/56Ch_rolling_against_TNG_56Ch.csv")#, 
#                       row.names=FALSE, col.names = TRUE, sep=",", na="0"))

df.2D_data.raw <- read.csv("results/predictions.dat", header = FALSE) # same
df.junk.raw <- read.csv("results/predictions_new.dat", header = FALSE)
df.junk2.raw <- read.csv("results/predictions_5401_7200_144x144.dat", header = FALSE) # same
df.junk3.raw <- read.csv("results/predictions_5401_7200_120x144.dat", header = FALSE)
df.junk4.raw <- read.csv("results/predictions_81perc.dat", header = FALSE)

df.2D_data <- (round(df.2D_data.raw, digits = 4) ) * 100
df.junk <- (round(df.junk.raw, digits = 4) ) * 100
df.junk2 <- (round(df.junk2.raw, digits = 4) ) * 100
df.junk3 <- (round(df.junk3.raw, digits = 4) ) * 100

# plot((d))
# histogram(1:128,spec_bkg, breaks = 128 )
# lines(density(spec_bkg))

df.2D_data <- df.junk3
df.2D_data <- df.junk2

#df.tmp_bkg <- subset(df.2D_data, df.2D_data$V1 > 10 & df.2D_data$V1 < 90)
sig <- data.frame(matrix(ncol = dim(df.2D_data)[2], nrow = dim(df.2D_data)[1]) )
v = vector(); isotope = vector(); isotope2 = vector(mode = 'integer',length = 1800)
for(i in 1:1800){
  v <- df.2D_data[i,]
  isotope[i] <- which(v==max(v))   ###  NOTE!!! there is more information in the CNN
  sig[i,] <- df.2D_data[i,]
#  isotope2[i] <- max(v)
  if(max(v) < 65){isotope2[i] <- 8}
#  if(isotope[i] == 1){sig[i,] <- df.2D_data[i,]}
  # v_sort <- sort(v, decreasing = TRUE)
  # isotope2[i] <- v_sort[2]
}                                  ###          which you are giving up here!!!!!
isotope <- isotope-1
histogram(isotope2)
sig2 <- subset(sig, !is.na(sig$X1) )
sig3 <- subset(sig, sig$X1 > 85)
sig4 <- subset(sig, sig$X1 < 85)
histogram(sig4$X1, breaks=30)

df.comb_data <- cbind(df.1D_data[,c(1,2,4)], isotope2)
df.comb_data$twoD_CNN_pred <- isotope
#df.comb_data <- df.comb_data[,-c(4)]   # rm 'isotope name
df.comb_data <- cbind(df.comb_data, sig)

## create combined data set for building statistics (df.comb_data)
###################################################################################

# ####### test threshold cuts on CNN
# df.cut <- subset(df.comb_data, df.comb_data$isotope2 == 8)
# df.cut_succCNN <- subset(df.cut, df.cut$SourceID == df.cut$twoD_CNN_pred)
# dim(df.cut); dim(df.cut_succCNN)
# ######################################
### RF != CNN
df.RF_CNN_agree <- subset(df.comb_data, df.comb_data$oneD_RF_pred == df.comb_data$twoD_CNN_pred)
df.RF_CNN_agree_fail <- subset(df.RF_CNN_agree, df.RF_CNN_agree$SourceID != df.RF_CNN_agree$twoD_CNN_pred)
df.RF_CNN_tmp <- subset(df.RF_CNN_agree_fail, df.RF_CNN_agree_fail$SourceID != 0 ) # if CNN = RF it is never a false positive
df.RF_CNN_tmp2 <- subset(df.RF_CNN_agree_fail, df.RF_CNN_agree_fail$twoD_CNN_pred == 0)  # they agree incorrectly that it is bkg
df.RF_CNN_tmp2 <- subset(df.RF_CNN_agree_fail, df.RF_CNN_agree_fail$twoD_CNN_pred != 0)  # they agree incorrectly that it is a particular source
df.RF_CNN_agree_fail_fneg <- subset(df.RF_CNN_agree_fail, df.RF_CNN_agree_fail$SourceID != 0 & df.RF_CNN_agree_fail$twoD_CNN_pred == 0)
histogram(df.RF_CNN_agree_fail$SourceID)

##################################

#######################################################
##  Bayes analysis
Prob_BKG <- dim(subset(df.comb_data, df.comb_data$SourceID == 0) )[1] /dim(df.comb_data)[1]  ## P(BG)
Prob_CNNbg <- dim(subset(df.comb_data, df.comb_data$twoD_CNN_pred == 0) )[1] /dim(df.comb_data)[1]  ## P(CNN_BG)
Prob_CNNbg_ifBG <- dim(subset(df.comb_data, df.comb_data$twoD_CNN_pred == 0 & df.comb_data$SourceID ==0) )[1] / 
  dim(subset(df.comb_data, df.comb_data$SourceID == 0) )[1]  ## P(CNN_BG|BG)
Prob_RFbg_ifBG <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$SourceID ==0) )[1] / 
  dim(subset(df.comb_data, df.comb_data$SourceID == 0) )[1]  ## P(RF_BG|BG)
Prob_CNNbg_RFbg <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred == 0))[1] /
  dim(df.comb_data)[1] ## P(CNN_BG & RF_BG)
Prob_notRFbg_ifBG <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$SourceID == 0) )[1] / 
  dim(subset(df.comb_data, df.comb_data$SourceID == 0) )[1]  ## P(notRF_BG|BG)
Prob_notCNNbg_ifBG <- dim(subset(df.comb_data, df.comb_data$twoD_CNN_pred != 0 & df.comb_data$SourceID == 0) )[1] / 
  dim(subset(df.comb_data, df.comb_data$SourceID == 0) )[1]  ## P(notCNN_BG|BG)

Prob_CNNbg_notRFbg <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred == 0))[1] /
  dim(df.comb_data)[1] ## P(CNN_BG & notRF_BG)
Prob_notCNNbg_RFbg <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred != 0))[1] /
  dim(df.comb_data)[1] ## P(notCNN_BG & RF_BG)
Prob_notCNNbg_notRFbg <- dim(subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred != 0))[1] /
  dim(df.comb_data)[1] ## P(notCNN_BG & notRF_BG)

Prob_BG_given_CNNbg <- (Prob_CNNbg_ifBG * Prob_BKG) / Prob_CNNbg
Prob_BG_given_CNNbg  ### P(BG|CNN_BG)

Prob_BG_given_CNNandRFbg <- ( Prob_RFbg_ifBG * Prob_CNNbg_ifBG * Prob_BKG ) / Prob_CNNbg_RFbg
Prob_BG_given_CNNandRFbg
junk <- subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred == 0)
junk <- subset(junk, junk$SourceID == 0)

Prob_BG_given_CNNBGandNotRFbg <- (Prob_notRFbg_ifBG * Prob_CNNbg_ifBG * Prob_BKG) / Prob_CNNbg_notRFbg
Prob_BG_given_CNNBGandNotRFbg
junk2 <- subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred == 0)
junk2 <- subset(junk2, junk2$SourceID == 0)

Prob_BG_given_notCNNBGandRFbg <- (Prob_notCNNbg_ifBG * Prob_RFbg_ifBG * Prob_BKG) / Prob_notCNNbg_RFbg
Prob_BG_given_notCNNBGandRFbg
junk3 <- subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred != 0);dim(junk3)
junk3 <- subset(junk3, junk3$SourceID == 0);dim(junk3)

Prob_BG_given_notCNNBGandNotRFbg <- (Prob_notCNNbg_ifBG * Prob_notRFbg_ifBG * Prob_BKG) / Prob_notCNNbg_notRFbg
Prob_BG_given_notCNNBGandNotRFbg
junk4 <- subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred != 0);dim(junk4)
junk4 <- subset(junk4, junk4$SourceID == 0);dim(junk4)

df.BKG_bayes_both <- subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred == 0)
df.BKG_bayes_RF <- subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred != 0)
df.BKG_bayes_CNN <- subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred == 0)
df.BKG_bayes_niether <- subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred != 0)
dim(df.BKG_bayes_both)
dim(df.BKG_bayes_RF)
dim(df.BKG_bayes_CNN)
dim(df.BKG_bayes_niether)
df.BKG_condProb_both <- subset(df.BKG_bayes_both, df.BKG_bayes_both$SourceID == 0)
df.BKG_condProb_RF <- subset(df.BKG_bayes_RF, df.BKG_bayes_RF$SourceID == 0)
df.BKG_condProb_CNN <- subset(df.BKG_bayes_CNN, df.BKG_bayes_CNN$SourceID == 0)
df.BKG_condProb_niether <- subset(df.BKG_bayes_niether, df.BKG_bayes_niether$SourceID == 0)
dim(df.BKG_condProb_both)
dim(df.BKG_condProb_RF)
dim(df.BKG_condProb_CNN)
dim(df.BKG_condProb_niether)
##  Bayes analysis
##############################################################

#############################################################
df.tmp2_bkg <- cbind(df.comb_data, sig)
df.tmp2_bkg <- subset(df.tmp2_bkg, df.tmp2_bkg$twoD_CNN_pred == 0)
df.tmp3_bkg <- subset(df.tmp2_bkg, df.tmp2_bkg$SourceID != 0)

df.tmp4_bkg <- data.frame(matrix(ncol = dim(df.tmp3_bkg)[2], nrow = dim(df.tmp3_bkg)[1]) )
for(i in 1:dim(df.tmp3_bkg)[1]){
  if(df.tmp3_bkg[i,5] < (max(df.tmp3_bkg[i,6:11]) + 20)  ){
    df.tmp4_bkg[i,] <- df.tmp3_bkg[i,]
  }
}
df.tmp4_bkg <- subset(df.tmp4_bkg, !is.na(df.tmp4_bkg$X1) )
df.tmp5_bkg <- subset(df.tmp3_bkg, df.tmp3_bkg$X1 < 85)
df.tmp6_bkg <- subset(df.tmp2_bkg, df.tmp2_bkg$X1 < 65)

histogram(df.tmp6_bkg$X1, breaks=30)

#######################################################################################
## main subsetting of data
df.fails_1D <- subset(df.comb_data, df.comb_data$SourceID != df.comb_data$oneD_RF_pred)
df.fails_2D <- subset(df.comb_data, df.comb_data$SourceID != df.comb_data$twoD_CNN_pred)
df.fails_both <- subset(df.fails_2D, df.fails_2D$SourceID != df.fails_2D$oneD_RF_pred)  ## fails both algorithms

df.fails_1D_onBKG <- subset(df.fails_1D, df.fails_1D$SourceID == 0)
df.fails_2D_onBKG <- subset(df.fails_2D, df.fails_2D$SourceID == 0)
df.fails_either_ALG <- subset(df.comb_data, df.comb_data$SourceID != df.comb_data$oneD_RF_pred | 
                                df.comb_data$SourceID != df.comb_data$twoD_CNN_pred ) ## union of failure 302=117+65+120

stats_union_fail <- as.data.frame(count(df.fails_either_ALG, SourceID) ) ## 302 evts fail one or both
stats_intersection_fail <- as.data.frame(count(df.fails_both, SourceID) ) ## 120 evts fail one or both

## fails for 2D cases (185 of 1800) but then success in 1D (65 of 1800)
## so there are 65 events were the 1D methods may help
## keeping in mind that 87 failed falsely ID background
df.fails_only2D <- subset(df.fails_2D, df.fails_2D$SourceID == df.fails_2D$oneD_RF_pred)  ## 65 evts only fail CNN
df.fails_both_same <- subset(df.fails_2D, df.fails_2D$twoD_CNN_pred == df.fails_2D$oneD_RF_pred) ## 105 fail both same misID
df.fails_both_same_Notbkg <- subset(df.both_fail_same, df.both_fail_same$oneD_RF_pred != 0)  ## 18
stats_1D.success_true <- as.data.frame(count(df.fails_only2D, SourceID) )
stats_1D.success_2Dpred <- as.data.frame(count(df.fails_only2D, twoD_CNN_pred) )
stats_1D.success_1Dpred <- as.data.frame(count(df.fails_only2D, oneD_RF_pred) )

# fails for 1D case but then seccess in 2D
df.fails_only1D <- subset(df.fails_1D, df.fails_1D$SourceID == df.fails_1D$twoD_CNN_pred)
stats_2D.success_true <- as.data.frame(count(df.fails_only1D, SourceID) )

## methods agree on it being background
df.both_bkg <- subset(df.comb_data, df.comb_data$oneD_RF_pred == 0 & df.comb_data$twoD_CNN_pred == 0)
## but are wrong
df.both_wrong_bkg <- subset(df.both_bkg, df.both_bkg$SourceID != 0)

stats_both.fail_same_true <- as.data.frame(count(df.fails_both_same, SourceID) )  ## 16 HEU & 2 Tc overlap false ID
stats_both.fail_true <- as.data.frame(count(df.fails_both, SourceID) )
stats_both.fail_2Dpred <- as.data.frame(count(df.fails_both, twoD_CNN_pred) )
stats_both.fail_1Dpred <- as.data.frame(count(df.fails_both, oneD_RF_pred) )
## indicateds a possible S/N issue in 87 events of the 1800

## failure to ID background with either method
df.tmp_fPos <- subset(df.comb_data, df.comb_data$SourceID == 0 )
df.double_fPos <- subset(df.tmp_fPos, df.tmp_fPos$oneD_RF_pred != 0 & df.tmp_fPos$twoD_CNN_pred != 0) # 91/20 fPos (RF/CNN) but never both

## ONLY EXAMINE one METHOD AT A TIME #################
## first 2D
stats_2D.fail_true <- as.data.frame(count(df.fails_2D, SourceID) )  ## by true ID
stats_2D.fail_pred <- as.data.frame(count(df.fails_2D, twoD_CNN_pred) )  ## by predicted ID
stats_2D.fail_RFpred <- as.data.frame(count(df.fails_2D, oneD_RF_pred) )  ## by RF predicted ID

## fails on sources
df.2D_fNeg <- subset(df.fails_2D, df.fails_2D$twoD_CNN_pred == 0)  ## 127 false negatives w/ CNN
df.1D_fNeg <- subset(df.fails_1D, df.fails_1D$oneD_RF_pred == 0)  ## 109 fNeg w/ RF
stats_2D.fNeg <- as.data.frame(count(df.2D_fNeg, SourceID) )  ## of background
stats_2D.fNeg_RFpred <- as.data.frame(count(df.2D_fNeg, oneD_RF_pred) )  ## of background
stats_2D.fNeg; stats_2D.fNeg_RFpred
stats_1D.fNeg <- as.data.frame(count(df.1D_fNeg, SourceID) )  ## of background
stats_1D.fNeg_CNNpred <- as.data.frame(count(df.1D_fNeg, twoD_CNN_pred) )  ## of background
stats_1D.fNeg; stats_1D.fNeg_CNNpred
df.fNeg_double <- subset(df.2D_fNeg, df.2D_fNeg$oneD_RF_pred == 0)

#df.1Dpred_BKG_also <- subset(df.2Dpred_BKG, df.2Dpred_BKG$oneD_RF_pred == 0)
stats_1D.fail_true <- as.data.frame(count(df.fails_1D, SourceID) )  ## by true ID
df.1Dpred_BKG <- subset(df.fails_1D, df.fails_1D$oneD_RF_pred == 0)
stats_1D.fail_predBKG <- as.data.frame(count(df.1Dpred_BKG, SourceID) )  ## of background
stats_1D.fail_predBKG

### combine failure when both predict background (double false-negative)

stats_double_fNeg_true <- as.data.frame(count(df.fNeg_double, SourceID) )
stats_double_fNeg_CNN <- as.data.frame(count(df.fNeg_double, twoD_CNN_pred) )

## main subsetting of data
#######################################################################################

#####################################################
## compare the two method stats
## visualization
plot(df.comb_data$oneD_RF_pred,df.comb_data$twoD_CNN_pred)

library(ggplot2)
library(plotly)
library(plyr)
# all
cnt <- with(df.comb_data, table(oneD_RF_pred, twoD_CNN_pred) )
p1 <- plot_ly(df.comb_data, x = ~oneD_RF_pred, y = ~twoD_CNN_pred, z = ~cnt) %>%
  add_histogram2d()

cnt <- cnt[2:6,2:6] 
tmp_data <- subset(df.comb_data, df.comb_data$oneD_RF_pred != 0 & df.comb_data$twoD_CNN_pred != 0)
p1a <- plot_ly(tmp_data, x = ~oneD_RF_pred, y = ~twoD_CNN_pred, z = ~cnt) %>%
  add_histogram2d()
# failed
cnt <- with(df.fails_both, table(oneD_RF_pred, twoD_CNN_pred) )
p2 <- plot_ly(df.fails_both, x = ~oneD_RF_pred, y = ~twoD_CNN_pred, z = ~cnt) %>%
  add_histogram2d()

# cnt <- cnt[2:5,2:3]
# p2a <- plot_ly(tmp_data, x = ~oneD_RF_pred, y = ~twoD_CNN_pred, z = ~cnt) %>%
#   add_histogram2d()
# 2D versus true failure
cnt <- with(df.fails_2D, table(SourceID, twoD_CNN_pred))
p3 <- plot_ly(df.fails_2D, x = ~SourceID, y = ~twoD_CNN_pred)#, z = ~cnt)

p4 <- subplot(
  p3 %>% add_markers(alpha=0.2),
  p3 %>% add_histogram2d()
)

p1;p2;p3;p1a


# cnt <- with(diamonds, table(cut, clarity))
# p <- plot_ly(diamonds, x = ~cut, y = ~clarity, z = ~cnt) %>%
#   add_histogram2d()
# 
# s <- matrix(c(1, -.75, -.75, 1), ncol = 2)
# obs <- mvtnorm::rmvnorm(500, sigma = s)
# p <- plot_ly(x = obs[,1], y = obs[,2])
# pp <- subplot(
#   p %>% add_markers(alpha = 0.2),
#   p %>% add_histogram2d()
# )
## compare the two method stats

########################################################3
## examine the CNN false negatives in detail
stats_double_fNeg_true; stats_double_fNeg_CNN  ## double fNeg

dim(df.2D_fNeg); stats_2D.fNeg

k = vector(); second_choice = vector()
for(j in 1:dim(df.2D_fNeg)[1]){
  k <- df.2D_fNeg[j,7:12]
  print(k)
  second_choice[j] <- which(k==max(k))
  print(second_choice[j])
}

second_choice <- as.data.frame(second_choice)
stats_2nd_choice <- as.data.frame(count(second_choice, second_choice))

df.2D_fNeg <- cbind(df.2D_fNeg,second_choice)
df.2D_fNeg_2nd_win <- subset(df.2D_fNeg, df.2D_fNeg$SourceID == df.2D_fNeg$second_choice)
df.2D_fNeg_2nd_loose <- subset(df.2D_fNeg, df.2D_fNeg$SourceID != df.2D_fNeg$second_choice)
df.2D_fNeg_2nd_loose_RFnotBKG <- subset(df.2D_fNeg_2nd_loose, df.2D_fNeg_2nd_loose$oneD_RF_pred != 0)
df.2D_fNeg_2nd_loo_RFwin <- subset(df.2D_fNeg_2nd_loose, df.2D_fNeg_2nd_loose$SourceID == df.2D_fNeg_2nd_loose$oneD_RF_pred)

histogram(df.2D_fNeg_2nd_loose$X1)  # softmax value of the ones where 2nd choice != SourceID
histogram(df.2D_fNeg_2nd_win$X1)    # softmax value of the ones where 2nd choice == SourceID

df.2D_fNeg_agreeRF <- subset(df.2D_fNeg, df.2D_fNeg$oneD_RF_pred == 0)
############################################
###  Look at all second choices

k = vector(); second_choice = vector()
for(j in 1:dim(df.fails_2D)[1]){
  k <- df.fails_2D[j,6:12]
  first <- which(k==max(k)); first <- as.integer(first)
  k <- k[c(-first)]
  second <- which(k==max(k)); second <- as.integer(second)
#  print(k)
  if(first > second){
    second_choice[j] <- second
    print(j)
  } else{
    second_choice[j] <- (second+1)
    print(j)
  }
#  second_choice[j] <- which(k==max(k))
#  print(second_choice[j])
}

second_choice <- as.data.frame(second_choice)
stats_2nd_choice <- as.data.frame(count(second_choice, second_choice))

df.2nd_choice <- cbind(df.fails_2D,second_choice)
## only 27 of 185 get it right the second time
df.first_two <- subset(df.2nd_choice, df.2nd_choice$SourceID == df.2nd_choice$second_choice)
stats_2nd_win <- as.data.frame(count(df.first_two, SourceID))
stats_2nd_win;stats_2D.fail_true  ## only WGPu helped significantly
stats_1D.fail_true

df.first_two_threat <- subset(df.2nd_choice, df.2nd_choice$second_choice == 1 | df.2nd_choice$second_choice == 2 |
                                df.2nd_choice$second_choice == 6 )  ## 69 evts
df.true_threat <- subset(df.first_two_threat, df.first_two_threat$SourceID == 1 | df.first_two_threat$SourceID == 2 |
                           df.first_two_threat$SourceID == 6 )
stats_poss_threat <- as.data.frame(count(df.first_two_threat, SourceID))
dim(df.true_threat)[1]; sum(stats_poss_threat$n)  ## more than 50% of the sources here are true threat, but that is not great

## examine the CNN false negatives in detail
#######################################
####
## plots specific to this 144x144 CNN
isotope_names <- c("Background", "HEU", "WGPu", "<sup>131</sup>I", 
                   "<sup>60</sup>Co", "<sup>99m</sup>Tc","HEU & <sup>99m</sup>Tc")
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_2D.fail_true$n)
data <- set_of_interest[, c('Categorie', 'Counts')]
#Type_evt <- c("false-negatives")  ## wrong these are just misclassifications
Type_evt <- c("Misclassifications")  ## wrong these are just misclassifications
method <- c("CNN")
#f_neg <- sum(stats_2D.fail_true$n)  ## wrong
fail_by_iso <- sum(stats_2D.fail_true$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_bad_CNN <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                     #              hole = 0.6,
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Counts, Type_evt, sep = " "),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
## by prediction
stats_temp <- stats_2D.fail_pred[1:2,]; stats_temp[3,1] <- 2; stats_temp[3,2] <- 0
stats_temp <- rbind(stats_temp, stats_2D.fail_pred[3:6,])
stats_temp <- stats_2D.fail_pred  ## for 120x144
rownames(stats_temp) <- seq(length=nrow(stats_temp))

set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n)
data <- set_of_interest[, c('Categorie', 'Counts')]
p_bad_CNN_pred <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                     #              hole = 0.6,
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Counts, Type_evt, sep = " "),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
## by prediction
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_2D.fail_RFpred$n)
data <- set_of_interest[, c('Categorie', 'Counts')]
p_bad_CNN_byRFpred <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                          #              hole = 0.6,
                          textposition = 'inside',
                          textinfo = 'label+percent',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = 'text',
                          text = ~paste(Counts, Type_evt, sep = " "),
                          marker = list(colors = colors,
                                        line = list(color = '#FFFFFF', width = 1)),
                          #The 'pull' attribute can also be used to create space between the sectors
                          showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by RF Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

###########
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_1D.fail_true$n)
data <- set_of_interest[, c('Categorie', 'Counts')]
#Type_evt <- c("misclassification")  ## wrong these are just misclassifications
method <- c("RF")
fail_by_iso <- sum(stats_1D.fail_true$n)  ## wrong

p_bad_RF <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                    #              hole = 0.6,
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~paste(Counts, Type_evt, sep = " "),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    #The 'pull' attribute can also be used to create space between the sectors
                    showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

###############
# isotope_names <- c("Background", "HEU", "WGPu", "<sup>131</sup>I", 
#                    "<sup>60</sup>Co", "<sup>99m</sup>Tc","HEU & <sup>99m</sup>Tc")
stats_temp <- stats_both.fail_true[1,]; stats_temp[1,] <- 0
stats_temp <- rbind(stats_temp,stats_both.fail_true)
stats_temp <- stats_both.fail_true  ## 120x144

set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
method <- c("Both CNN & RF")
fail_by_iso <- sum(stats_temp$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_intersect <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                       #              hole = 0.6,
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(Counts, Type_evt, sep = " "),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

stats_temp <- stats_both.fail_1Dpred[1:2,]; stats_temp[3,1] <- 2; stats_temp[3,2] <- 0  ## same 120x144
stats_temp <- rbind(stats_temp,stats_both.fail_1Dpred[3:5,])
stats_temp[7,1] <- 6; stats_temp[7,2] <- 0
rownames(stats_temp) <- seq(length=nrow(stats_temp))
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
fail_by_iso <- sum(stats_temp$n)  ## wrong
p_intersect_byRF <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                       #              hole = 0.6,
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(Counts, Type_evt, sep = " "),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by RF Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

stats_temp <- stats_both.fail_2Dpred[1:2,]; stats_temp[3:5,1] <- c(2:4); stats_temp[3:5,2] <- 0
stats_temp <- rbind(stats_temp,stats_both.fail_2Dpred[3,])
stats_temp[7,] <- 6; stats_temp[7,2] <- 0
stats_temp <- stats_both.fail_2Dpred[1:3,]; stats_temp[4,1] <- 3; stats_temp[4,2] <- 0  ## 120x144
stats_temp <- rbind(stats_temp,stats_both.fail_2Dpred[4:5,])     ## 120x144
stats_temp[7,] <- 6; stats_temp[7,2] <- 0    ## 120x144
rownames(stats_temp) <- seq(length=nrow(stats_temp))
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
fail_by_iso <- sum(stats_temp$n)  ## wrong
p_intersect_byCNN <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                            #              hole = 0.6,
                            textposition = 'inside',
                            textinfo = 'label+percent',
                            insidetextfont = list(color = '#FFFFFF'),
                            hoverinfo = 'text',
                            text = ~paste(Counts, Type_evt, sep = " "),
                            marker = list(colors = colors,
                                          line = list(color = '#FFFFFF', width = 1)),
                            showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by CNN Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
###############
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_union_fail$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
#Type_evt <- c("false-negatives")  ## wrong these are just misclassifications
#Type_evt <- c("misclassification")  ## wrong these are just misclassifications
method <- c("Either CNN | RF")
#f_neg <- sum(stats_2D.fail_true$n)  ## wrong
fail_by_iso <- sum(stats_union_fail$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_union <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                   #              hole = 0.6,
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(Counts, Type_evt, sep = " "),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

################# only CNN wrong
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_1D.success_true$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
method <- c("CNN Only")
#f_neg <- sum(stats_2D.fail_true$n)  ## wrong
fail_by_iso <- sum(stats_1D.success_true$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_only_CNN <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                      #              hole = 0.6,
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
#                      textposition = 'outside',
#                      textinfo = 'text',
                      hoverinfo = 'text',
                      text = ~paste(Counts, Type_evt, sep = " "),
#                      text = ~paste('(', Counts, ')', sep = ""),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# by predicted class
stats_temp <- stats_1D.success_2Dpred[1:2,]; stats_temp[3,1] <- 2; stats_temp[3,2] <- 0
stats_temp <- rbind(stats_temp, stats_1D.success_2Dpred[3:6,])

stats_temp <- stats_1D.success_2Dpred   ## 120x144
rownames(stats_temp) <- seq(length=nrow(stats_temp) )
                            
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n )
data <- set_of_interest[, c('Categorie', 'Counts')]

p_only_CNN_pred <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                      #              hole = 0.6,
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste(Counts, Type_evt, sep = " "),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_1D.success_1Dpred$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
p_only_CNN_byRFpred <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                           #              hole = 0.6,
                           textposition = 'inside',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF'),
                           hoverinfo = 'text',
                           text = ~paste(Counts, Type_evt, sep = " "),
                           marker = list(colors = colors,
                                         line = list(color = '#FFFFFF', width = 1)),
                           #The 'pull' attribute can also be used to create space between the sectors
                           showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by RF Prediction -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
###############
################# only RF wrong
set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_2D.success_true$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
method <- c("RF Only")
#f_neg <- sum(stats_2D.fail_true$n)  ## wrong
fail_by_iso <- sum(stats_2D.success_true$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_only_RF <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                     #              hole = 0.6,
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(Counts, Type_evt, sep = " "),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by True Class -', 'total', fail_by_iso, sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

###############
## examine the double false negative rate!!
stats_temp <- stats_double_fNeg_true[1,]; stats_temp[1,] <- 0
stats_temp <- rbind(stats_temp,stats_double_fNeg_true); stats_temp[7,] <- 0

set_of_interest <- data.frame("Categorie" = isotope_names, "Counts" = stats_temp$n )
data <- set_of_interest[, c('Categorie', 'Counts')]
method <- c("Double False-Negative")
#f_neg <- sum(stats_2D.fail_true$n)  ## wrong
fail_by_iso <- sum(stats_double_fNeg_true$n)  ## wrong

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)')

p_double_F_neg <- plot_ly(data, labels = ~Categorie, values = ~Counts, type = 'pie',
                          #              hole = 0.6,
#                          textposition = 'none',
                          textposition = 'inside',
                          textinfo = 'label+percent',
#                          textinfo = 'blank',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = 'text',
                          text = ~paste(Counts, Type_evt, sep = " "),
                          marker = list(colors = colors,
                                        line = list(color = '#FFFFFF', width = 1)),
                          #The 'pull' attribute can also be used to create space between the sectors
                          showlegend = FALSE) %>%
  layout(title = paste(method, Type_evt, 'by Prediction -', 'total', fail_by_iso, sep = ' '),
#  layout(title = paste('', sep = ' '),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# visual
p_double_F_neg
p_only_RF
p_only_CNN
p_only_CNN_pred
p_only_CNN_byRFpred
p_union
p_intersect
p_intersect_byRF
p_intersect_byCNN
p_bad_RF
p_bad_CNN
p_bad_CNN_pred
p_bad_CNN_byRFpred

p_sub <- subplot(p_only_CNN,p_only_RF)

Acc_120x144 <- 1 - ( dim(df.fails_2D)[1]/dim(df.comb_data)[1] )
fPos_rate_120x144 <- ( dim(df.fails_2D_onBKG)[1]/dim(df.comb_data)[1] )
df.fails_sig <- subset(df.fails_2D, df.fails_2D$SourceID != 0 & df.fails_2D$twoD_CNN_pred != 0)
fNeg_rate_120x144 <- ( dim(df.fails_sig)[1]/dim(df.comb_data)[1] )  ## No source present!
###############
## Bar charts

Source <- isotope_names  #  just for plot naming

RF_fail <- stats_1D.fail_true$n
CNN_fail <- stats_2D.fail_true$n
RF_only_fail <- stats_2D.success_true$n
CNN_only_fail <- stats_1D.suc_true$n
both_fail <- stats_both.fail_true$n; both_fail <- c(0, both_fail)  ## both never fail on SourceID = 0

df.bar_failure <- data.frame(Source, RF_fail, CNN_fail, both_fail)
df.bar_failure$Source <- factor(df.bar_failure$Source, levels = isotope_names)

p_failure <- plot_ly(df.bar_failure, x = ~Source, y = ~RF_fail, type = 'bar', name = 'RF') %>%
  add_trace(y = ~CNN_fail, name = 'CNN') %>%
  add_trace(y = ~both_fail, name = 'both') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p_failure

p_fail_wo_BKG <- plot_ly(df.bar_failure, x = ~Source[2:7], y = ~RF_fail[2:7], type = 'bar', name = 'RF') %>%
  add_trace(y = ~CNN_fail[2:7], name = 'CNN') %>%
  add_trace(y = ~both_fail[2:7], name = 'both') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p_fail_wo_BKG

## compare the overlaps
p_fail_overlaps <- plot_ly(df.bar_failure, x = ~Source, y = ~RF_fail, type = 'bar', name = 'RF') %>%
  add_trace(y = ~CNN_fail, name = 'CNN') %>%
  add_trace(y = ~CNN_only_fail, name = 'CNN only') %>%
  add_trace(y = ~RF_only_fail, name = 'RF only') %>%
  add_trace(y = ~both_fail, name = 'both') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p_fail_overlaps

p_fail_overlaps_wo_BKG <- plot_ly(df.bar_failure, x = ~Source[2:7], y = ~RF_fail[2:7], type = 'bar', name = 'RF') %>%
  add_trace(y = ~CNN_fail[2:7], name = 'CNN') %>%
  add_trace(y = ~CNN_only_fail[2:7], name = 'CNN only') %>%
  add_trace(y = ~RF_only_fail[2:7], name = 'RF only') %>%
  add_trace(y = ~both_fail[2:7], name = 'both') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p_fail_overlaps_wo_BKG

### Venn diagrams


library(VennDiagram)

venn.diagram(list(B = 1:1800, A = 1571:2020),fill = c("red", "green"),
             alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, fontfamily =3, 
             filename = "results/trial2.emf");



### Venn diagrams
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
# library(rJava)
# library(venneuler)
# v <- venneuler(c(A=450, B=1800, "A&B"=230))
# plot(v)
# 
# # library(VennDiagram)
# # 
# # venn.diagram(list(B = 1:1800, A = 1571:2020),fill = c("red", "green"),
# #              alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, fontfamily =3, 
# #              filename = "results/trial2.emf");
# library(VennDiagram)
# source("http://www.bioconductor.org/biocLite.R")
# biocLite("limma")
# hsb2 <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsb2-3.csv") 
# attach(hsb2)
# hw <- (write >= 60)
# hm <- (math >= 60)
# hr <- (read >= 60)
# c3 <- cbind(hw, hm, hr)
# a <- vennCounts(c3)


# ##########################
# library(ggplot2)
# library(dplyr)
# library(plotly)
# 
# library(gplots)
# venn( list(A=1:5,B=4:6,C=c(4,8:10)) )
# 
# venn(list(A=1:34, B=28:60, C=28:34))
# 
# library(VennDiagram)
# 
# # library(VennDiagram)
# # temp <- venn.diagram(list(B = 1:1800, A = 1571:2020),
# #                      fill = c("red", "green"), alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,
# #                      lty =2, fontfamily =3, filename = NULL)
# # grid.draw(temp)
# # library(grDevices)
# # 
# # pdf(file="venn.pdf")
# # grid.draw(temp)
# # dev.off()
# 
# # libraries
library(VennDiagram)
library(grid)
library(gridBase)
library(lattice)

# create the diagrams
temp1 <- venn.diagram(list(CNN = 1:185, RF = 66:302),
                      fill = c("red", "green"), alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,
                      lty =1, filename = NULL)
# temp2 <- venn.diagram(list(CNN = 1:1800, RF = c(198:1865, 2037:2168), True = c(81:1684, 1790:1985) ),
#                       fill = c("red", "green", "blue"), alpha = c(0.5, 0.5, 0.5), cex = 2,cat.fontface = 4,
#                       lty =1, filename = NULL)
temp2 <- venn.diagram(list(CNN = 1:1800, RF = c(183:1865, 2057:2173), True = c(66:1669, 1790:1985) ),
                      fill = c("red", "green", "blue"), alpha = c(0.5, 0.5, 0.5), cex = 2,cat.fontface = 4,
                      lty =1, filename = NULL)


# start new page
plot.new() 

pdf("test.pdf", width = 14, height = 7)
# setup layout
gl <- grid.layout(nrow=1, ncol=2)
# grid.show.layout(gl)

# setup viewports
vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 

# init layout
pushViewport(viewport(layout=gl))
# access the first position
pushViewport(vp.1)

# start new base graphics in first viewport
par(new=TRUE, fig=gridFIG())

grid.draw(temp1)

# done with the first viewport
popViewport()

# move to the next viewport
pushViewport(vp.2)

grid.draw(temp2)

# done with this viewport
popViewport(1)

dev.off()

######################################

venn.diagram()

#Then generate 3 sets of words.There I generate 3 times 200 SNPs names.
SNP_pop_1=paste(rep("SNP_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
SNP_pop_2=paste(rep("SNP_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
SNP_pop_3=paste(rep("SNP_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")

#The goal of the Venn Diagram is to count how many words are common between SNP_pop_1 and SNP_pop_2, between SNP_pop_1 and SNP_pop_3 and so on...
#The venn.diagram function do it automatically and draw it! (you will get a png file in your current working directory)

venn.diagram(
  x = list(SNP_pop_1 , SNP_pop_2 , SNP_pop_3),
  category.names = c("SNP pop 1" , "SNP pop 2 " , "SNP pop 3"),
  filename = '#14_venn_diagramm.png',
  output = TRUE ,
  imagetype="png" ,
  height = 480 ,
  width = 480 ,
  resolution = 300,
  compression = "lzw",
  lwd = 2,
  lty = 'blank',
  fill = c('yellow', 'purple', 'green'),
  cex = 1,
  fontface = "bold",
  fontfamily = "sans",
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

#########################################

df.union_fail <- subset(df.comb_data, df.comb_data$oneD_RF_pred != df.comb_data$SourceID | 
                             df.comb_data$twoD_CNN_pred != df.comb_data$SourceID)
ppf <- plot_ly(df.union_fail, x = ~twoD_CNN_pred, y = ~oneD_RF_pred, z = ~SourceID,
               marker = list(color = ~SourceID, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'CNN pred'),
                      yaxis = list(title = 'RF pred'),
                      zaxis = list(title = 'True')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

pf <- plot_ly(alpha = 0.6, name = c("what?") ) %>%
  add_histogram(x = ~df.fails_1D$oneD_RF_pred ) %>%
  add_histogram(x = ~df.fails_2D$twoD_CNN_pred ) %>%
  layout(barmode = "overlay")


library(ggplot2)
# Basic histogram
ggplot(df, aes(x=weight)) + geom_histogram()
# Change the width of bins
ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(df, aes(x=weight)) + 
  geom_histogram(color="black", fill="white")


# Change line colors by groups
ggplot(df, aes(x=weight, color=sex, fill=sex)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight histogram plot",x="Weight(kg)", y = "Density")+

    theme_classic()


set.seed(1234)
df <- data.frame(
  Algorithm=factor(rep(c("RF", "CNN"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)
head(df)

df.junk <- as.data.frame(df.fails_1D$oneD_RF_pred); df.junk$x2 <- "RF"
df.junk2 <- as.data.frame(df.fails_2D$twoD_CNN_pred); df.junk2$x2 <- "CNN"
colnames(df.junk2) <- colnames(df.junk) <- c("Prediction", "Algorithm")
df.junk <- rbind(df.junk,df.junk2)
library(plyr)
mu <- ddply(df.junk, "Algorithm", summarise, grp.mean=mean(Prediction))
head(mu)

p<-ggplot(df.junk, aes(x=Prediction, color=Algorithm)) +
  geom_histogram(fill="white", position="dodge")
#  stat_bin(bins = 10)
#  geom_vline(data=mu, aes(xintercept=grp.mean, color=Algorithm),
#             linetype="dashed")
# Gradient colors
p <- p + scale_color_brewer(palette="Dark2") + 
  theme_minimal()+theme(legend.position="top")


p


####################

#########################################
"Sin<sup>super</sup>"
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="pie-styled")
#chart_link

########################################
###################################
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-197):(ans_loc-40),])    # 5404
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-50):(ans_loc-20),])    # 5473
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-36):(ans_loc-7),])    # 5479
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-60):(ans_loc-15),])    # 5487
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-70):(ans_loc-25),])    # 5516
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-50):(ans_loc-15),])    # 5518
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+10):(ans_loc+36),])    # 5519
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-55):(ans_loc-15),])    # 5527
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+30):(ans_loc+150),])    # 5529
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-42):(ans_loc-15),])    # 5534
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-38):(ans_loc-10),])    # 5567
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-46):(ans_loc-15),])    # 5571
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-64):(ans_loc-25),])    # 5584
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-44):(ans_loc-15),])    # 5602
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+20):(ans_loc+50),])    # 5652
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-57):(ans_loc-20),])    # 5670
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-49):(ans_loc-20),])    # 5675
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-39):(ans_loc-10),])    # 5715
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+15):(ans_loc+44),])    # 5719
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+15):(ans_loc+44),])    # 5733
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+15):(ans_loc+38),])    # 5750
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+15):(ans_loc+40),])    # 5760
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-185):(ans_loc-40),])    # 5799
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-36):(ans_loc-15),])    # 5809
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-77):(ans_loc-25),])    # 5816
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-75):(ans_loc-25),])    # 5833
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-98):(ans_loc-35),])    # 5844
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-44):(ans_loc-15),])    # 5876
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-29):(ans_loc-10),])    # 5877
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-64):(ans_loc-20),])    # 5901
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-42):(ans_loc-10),])    # 5955
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-71):(ans_loc-20),])    # 5981
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-53):(ans_loc-15),])    # 6025
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-51):(ans_loc-15),])    # 6058
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc-41):(ans_loc-15),])    # 6061 & 6069 & 6083
# spec_bkg[1:bins] <- colSums(df.raw[(ans_loc+40):(ans_loc+200),])    # 6184
#spec_bkg[1:bins] <- colSums(df.raw[1:dim(df.raw)[1],])
#plot(1:bins,spec_src, log = "y", title("Source")); plot(1:bins,spec_bkg, log = "y", title("Background"))

#plot(1:bins,spec_bkg, log="y")


#################################################
## test documentation

# > fg <- as.data.frame(w)
# > fg <- cbind(fg,w2,KS_ans[1:9800],z)
# > colnames(fg) <- c("ver1","ver2","label","AD_pVal_ver2")
# > fg[fg==7]<-0
# > fg2 <- subset(fg, fg$ver1 == fg$label)
# > fg3 <- subset(fg, fg$ver2 == fg$label)
# > fg4 <- subset(fg, fg$ver1 == fg$ver2)
# > dim(fg);dim(fg2);dim(fg3);dim(fg4)
# [1] 9800    4
# [1] 4815    4
# [1] 5307    4
# [1] 8919    4
# > w <- vector();w2 <- vector(); z <- vector()
# > for(i in 1:9800){
#   +   v <- KS_stat_success[i,]
#   + #  v <- AD_ver1_tot[i,]
#     +   w[i] <- which(v==min(v))
#     +   v2 <- KS_pVal_success[i,]
#     + #  v2 <- AD_ver2_tot[i,]
#       +   w2[i] <- which(v2==max(v2))
#       +   z[i] <- max(v2)
#       + }
# There were 50 or more warnings (use warnings() to see the first 50)
# > fg <- as.data.frame(w)
# > fg <- cbind(fg,w2,KS_ans[1:9800],z)
# > #fg <- cbind(fg,answers_raw$SourceID[1:200])
#   > colnames(fg) <- c("stat","prob","label","pVal")
# > fg[fg==7]<-0
# > fg2 <- subset(fg, fg$stat == fg$label)
# > fg3 <- subset(fg, fg$prob != fg$label)
# > fg4 <- subset(fg, fg$prob == fg$stat)
# > dim(fg);dim(fg2);dim(fg3);dim(fg4)
# [1] 9800    4
# [1] 5038    4
# [1] 4762    4
# [1] 9800    4
# > 5038/9800
# [1] 0.5140816             ## success with KS test
# > 5307/9800
# [1] 0.5415306             ## success with AD test
##   NEED to check if this is all background !!!!!!!!!!!!!!!
## KS yields 1378/4800 = 29% success on sources &    ## 800/9800  8% would be random
## 3660/5000 = 73% success on background            ## 5000/9800  51%
# [1] 1529    4
# > 1529/4800
# [1] 0.3185417
# > 1521/4800
# [1] 0.316875
# > non_bkg_AD2 <- subset(fg3, fg3$label == 0)
# > non_bkg_AD1 <- subset(fg2, fg2$label == 0)
# > dim(non_bkg_AD1)
# [1] 3286    4
# > dim(non_bkg_AD2)
# [1] 3786    4
# > 3286/5000
# [1] 0.6572
# > 3786/5000
# [1] 
## AD yields 1529/4800 = 32% success on sources & 
## 3786/5000 = 76% success on background
##########  NEED to compare the two results KS & AD