## run some random forest business on my source features
library(caret)
library(randomForest)
library(dplyr)

#you need to reset the working directory
setwd("C:/Users/mooreet_la/projects/SDRD/competition/")
# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas


df.mod <- read.csv("data/TNG_set.csv")
df.mod$SourceID <- factor(df.mod$SourceID)

###
df.mod <- read.csv("data/TNG_set128_v2.csv")
df.mod$runID <- factor(df.mod$runID)
df.mod$SourceID <- factor(df.mod$SourceID)

###
df.mod <- read.csv("data/TNG_128Ch_roll_25s.csv")
df.mod <- read.csv("data/TNG_128Ch_roll_3sec.csv")
df.mod$runID <- factor(df.mod$runID)
df.mod$SourceID <- factor(df.mod$SourceID)
test <- df.mod

df.mod <- read.csv("data/TNG_128Ch_selected.csv")
df.mod$runID <- factor(df.mod$runID)
df.mod$SourceID <- factor(df.mod$SourceID)
#test <- df.mod


##############################################################CURRENT!!!!!!!!!!!!!!!!!!
df.mod <- read.csv("data/TNG_18Brm_selectedbyMetric.csv")
#df.mod <- read.csv("data/TNG_32Ch.csv")
#df.mod <- read.csv("data/TNG_56Ch.csv")
#df.mod <- read.csv("data/TNG_64Ch.csv")
#df.mod <- read.csv("data/TNG_144Ch.csv")
df.mod$runID <- factor(df.mod$runID)
df.mod$SourceID <- factor(df.mod$SourceID)

nbins <- 18
#nbins <- 32
#nbins <- 56
#nbins <- 64
#nbins <- 128
#nbins <- 144

# Split data into training and testing set
## randomly choose 60% of the data set as training data (Why 60% instead of 70%?)
set.seed(102)
#train.indices <- sample(1:nrow(df.mod), 0.1*nrow(df.mod))
train.indices <- c(1:(nrow(df.mod)/2) )
train <- df.mod[train.indices,]
test <- df.mod[-train.indices,]

# train <- train[1:(nrow(train)/2),]
# test <- test[1:(nrow(test)/2),]

#if you want to use all channel data, define the features as the channels
# x1 <- grep("Channel0",colnames(df.mod))
# x2 <- grep("Channel1023",colnames(df.mod))
#features <- c("SourceIDs",colnames(df.mod[,x1:x2]))


features <- colnames(df.mod[1:(nbins+1)])

# Set seed to ensure reproducibility between runs
set.seed(12345)

# Set up caret to perform 10-fold cross validation repeated 3 times
caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)

# Use caret to train a Mighty Random Forest using 10-fold cross 
# validation repeated 3 times and use 5 values for tuning the
# mtry parameter. Use 101 trees as our data is small. This code 
# returns the best model trained on all the data! Mighty!
st_time <- Sys.time()

rf.cv <- train(SourceID ~ ., 
               data = train[, features],
               method = "rf",
               trControl = caret.control,
               tuneLength =5,
               ntree = 201, 
               importance = TRUE,
               na.action=na.exclude)

cur_time <- Sys.time() - st_time

# Display cur_timethe results of the cross validation run
#rf_144ch_1st25per.cv <- rf.cv
rf.cv

# What is the standard deviation?
cat(paste("\nCross validation standard deviation:",  
          sd(rf.cv$resample$Accuracy), "\n", sep = " "))


# Pull out the the trained model using the best parameters on
# all the data! Mighty!
#rf_128ch_50per <- rf.best
rf.best <- rf.cv$finalModel
rf.best

# Look at the model - this model is trained on 100% of the data!
varImpPlot(rf.best)

# Create predictions for the test set

preds <- predict(rf.cv, test, type = "raw")
#############################################################
test_evt <- df.mod[-train.indices,]
test_evt <- test_evt[-1,]
test_evt <- test
dim(test_evt)
View(test_evt[,125:131])
length(preds)
preds_test <- preds#[2:length(preds)]
length(preds_test)
df_test_evt <- test_evt[c("SourceID","runID")]
test_preds <- preds[-1]
test_preds <- preds
df_test_evt$prediction <- test_preds



stats_test <- aggregate(prediction ~ runID, df_test_evt, Mode)
stats_test <- aggregate(prediction ~ runID, df_test_evt, Mode.woZ)
names(stats_test) <- c("runID","prediction")
#temp_test <- aggregate(SourceID ~ runID, df_test_evt, Mode)

true_test <- answers[4901:7350,]
#true_test <- answers[((dim(answers)[1])%/%2 + 1 ):(9800 - (dim(answers)[1])%/%4 ),]
#true_test <- answers[(9800 - (dim(answers)[1])%/%4 ):9800,]

pred_val <- stats_test$prediction
true_val <- as.factor(true_test$SourceID)

true_loc <- round(true_test$SourceTime)
true_loc <- (true_loc %/% 20) * 20

# make confusion!!!!!!!!!!!!!!!!!!
#display the confusion matrix for the test set
confMTX.evtsv2 <- confusionMatrix(pred_val,true_val)
confMTX.evtsv2

confMTX.evts <- confusionMatrix(as.factor(vec_ans),true_val)
confMTX.evts
vec_locV2 <- (vec_loc %/% 20) * 20
compare.loc <- as.data.frame(cbind(vec_locV2,true_loc))
compare.loc$diff <- true_loc - vec_locV2
stats_comp <- as.data.frame(count(compare.loc,diff) )
histogram(stats_comp$n)
plot(stats_comp$diff,stats_comp$n)
#####
sum(stats_comp$n)
stats_comp$diff
sum(stats_comp$n) - stats_comp$n[2]
sum(stats_comp$n) - stats_comp$n[20]
####

df_vec <- cbind(vec_ans,vec_loc,vec_lng)
names(df_vec) <- c("Class","loc", "length")
plot(vec_ans,vec_lng)
boxplot(length ~ Class, data = df_vec)


confMTX.evtsLoc <- confusionMatrix(as.factor(vec_locV2),as.factor(true_loc) )
confMTX.evtsLoc

confMTX.evts3sec <- confusionMatrix(as.factor(ans_3sec),true_val)
confMTX.evts3sec

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
###############################################################
# Create predictions test example dummy  TNG_set128_roll_25s.csv

#data_path <- "data/integrated_128Ch_3sec/"
#data_path <- "data/integrated_128Ch/"
#data_path <- "data/integrated_128Ch_bkgR/"
#data_path <- "data/128Ch_rolling/"
#data_path <- "data/trainingData/32Ch_rolling/"
#data_path <- "data/trainingData/56Ch_rolling/"
#data_path <- "data/trainingData/56Ch_rolling_4sec/"
data_path <- "data/trainingData/metric_1sec_bkgR/"
intrv <- 4  ## number of quarter seconds in the integration window

vec_loc <- as.numeric(vector() )
vec_ans <- as.integer(vector() )
vec_lng <- as.integer(vector() )

st_time <- Sys.time()

for(i in 4901:7350){
  evtID <- paste("10",as.character(i),sep = "")
  if(i < 1000){evtID <- paste("100",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("1000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("10000",as.character(i),sep = "")}
  
  dummy <- read.csv(paste(data_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  
#  dummy <- dummy / 4.
  
#dummy <- read.csv(paste(data_path, "104916", ".csv", sep = ""), header=FALSE, sep=",")
  names(dummy) <- colnames(df.mod[1:nbins])
#dim(dummy)
#  preds_dummy <- predict(rf.cv, dummy, type = "raw")  # 3 seconds
#  preds_dummy <- predict(mod, dummy, type = "response")  # 1 second
  preds_dummy <- predict(rf.cv, dummy, type = "raw")   #current bkg rm'ed
  preds_dummy[1:30] <- 0

  df_dummy <- data.frame(matrix(ncol = 2, nrow = length(preds_dummy)))
  names(df_dummy) <- c("second","prediction")
  df_dummy$second <- c(1:length(preds_dummy))
  df_dummy$prediction <- preds_dummy

  stats_dummy <- as.data.frame(count(df_dummy, prediction) )

  s <- stats_dummy[-1,]
  b <- which(s$n %in% c(max(s$n)))
  d <- s$prediction[b]


  if(max(s$n) >= 10 ){             ##  4 for the 3 sec integr set to 7 for 128Ch
    ans_dummy <- as.integer(d) - 1
    loc_dummy <- round(median( which(as.integer(preds_dummy) %in% c(as.integer(d))) ),2)
    vec_loc[(i-4900)] <- (((intrv*loc_dummy) + (1-intrv)*trunc(loc_dummy) + (intrv-1) )/4)
  } else {
    ans_dummy <- 0
    loc_dummy <- 0.00
    vec_loc[(i-4900)] <- loc_dummy
    }

#  vec_loc[(i-4900)] <- loc_dummy
  vec_ans[(i-4900)] <- ans_dummy
  vec_lng[(i-4900)] <- max(s$n)
  
  

#mask_dummy <- as.integer(preds_dummy) == ans_dummy
#length( which(as.integer(preds_dummy) %in% c(ans_dummy)) )
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time); print(max(s$n))}
}

#loc_3sec <- vec_loc
#ans_3sec <- vec_ans
################################################################

# Create dataframe of the KNOWN and PREDICTED sourceID for the test set
test.results <- data.frame(KnownSource = test$SourceIDs,
                         ModelPrediction = preds)

test.results <- as.data.frame(cbind(test$SourceID, preds))
colnames(test.results) <- c("true", "pred")

#only look at the cases it got wrong
test.false <- test.results[which(test.results$true != test.results$pred),]

#display the confusion matrix for the test set
confusionMatrix(preds,test$SourceID)

#save the random forest model to use later
# saveRDS(rf.best, file="RF_128Ch_1sec_BkgRMv2.rds")
# saveRDS(rf.cv, file="RF_128Ch_1sec_FULL_BkgRMv2.rds")
# saveRDS(rf.best, file="RF_128Ch_1sec_select.rds")
# saveRDS(rf.cv, file="RF_128Ch_1sec_FULL_select.rds")
#saveRDS(rf.best, file="RF_64Ch_1sec.rds")
#saveRDS(rf.cv, file="RF_64Ch_1sec_FULL.rds")
#saveRDS(rf.best, file="RF_32Ch_1sec.rds")
#saveRDS(rf.cv, file="RF_32Ch_1sec_FULL.rds")
saveRDS(rf.best, file="RF_18Brm_selectedbyMetric_1sec_50per.rds")
saveRDS(rf.cv, file="RF_18Brm_selectedbyMetric_1sec_FULL_50per.rds")

### read in a saved model
## I actually don't think this is working
## please fix it for me :)
mod <- readRDS("RF_50per_128ch.rds")
preds <- predict(mod, test, type = "response")
preds1 <- as.data.frame(preds)
preds1$SourceID <- test$SourceID; preds1$runID <- test$runID
test.results <- data.frame(Name = test$runID, KnownSource = test$SourceIDs,
                           ModelPrediction = preds1$preds)
test.false <- test.results[which(test.results$KnownSource!=test.results$ModelPrediction),]
r <- confusionMatrix(preds1$preds,test$SourceID)



# make a visualiztion of the confusion matrix
confusion <- as.data.frame(as.table(r$table[1:7,1:7]))
plot <- ggplot(confusion)
plot + geom_tile(aes(x=Prediction, y=Reference, fill=Freq)) + 
  scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(low = "white", high = "darkblue", 
                      breaks=seq(from=0, to=6000, by=1000)) +
  labs(fill="Normalized\nFrequency") 
  


###############################################################
# Create predictions for submission
##############################################################
#data_path <- "data/integrated_128Ch_3sec/"
#data_path <- "data/testingData/integrated_128Ch/"
#data_path <- "data/testingData/128Ch_rolling/"
data_path <- "data/testingData/56Ch_rolling/"
vec_loc <- as.numeric(vector() )
vec_ans <- as.integer(vector() )
vec_lng <- as.integer(vector() )

st_time <- Sys.time()
for(i in 1:15923){
  evtID <- paste("4",as.character(i),sep = "")
  if(i < 10000){evtID <- paste("40",as.character(i),sep = "")}
  if(i < 1000){evtID <- paste("400",as.character(i),sep = "")}
  if(i < 100){evtID <- paste("4000",as.character(i),sep = "")}
  if(i < 10){evtID <- paste("40000",as.character(i),sep = "")}
  
  dummy <- read.csv(paste(data_path, evtID, ".csv",sep = ""), header=FALSE, sep=",")
  names(dummy) <- colnames(df.mod[1:nbins])
#  preds_dummy <- predict(mod, dummy, type = "response")  # 1 second
  preds_dummy <- predict(rf.cv, dummy, type = "raw")
  preds_dummy[1:30] <- 0
  
  df_dummy <- data.frame(matrix(ncol = 2, nrow = length(preds_dummy)))
  names(df_dummy) <- c("second","prediction")
  df_dummy$second <- c(1:length(preds_dummy))
  df_dummy$prediction <- preds_dummy
  
  stats_dummy <- as.data.frame(count(df_dummy, prediction) )
  
  s <- stats_dummy[-1,]
  b <- which(s$n %in% c(max(s$n)))
  d <- s$prediction[b]
  
  
  if(max(s$n) >= 7 ){
    ans_dummy <- as.integer(d) - 1
    loc_dummy <- round(median( which(as.integer(preds_dummy) %in% c(as.integer(d))) ),2)
    vec_loc[(i)] <- (((4*loc_dummy) - 3*trunc(loc_dummy) - 1)/4)
  } else {
    ans_dummy <- 0
    loc_dummy <- 0.00
    vec_loc[(i)] <- loc_dummy
  }
  
  vec_ans[(i)] <- ans_dummy
  vec_lng[(i)] <- length(preds_dummy)
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
}

########################
vec_ans <- as.integer(vec_ans)

sub_temp <- read.csv("data/answerTemplateV4.csv", header = TRUE, sep = ",")
submission <- sub_temp
submission$SourceID <- vec_ans
submission$SourceTime <- vec_loc

prev_sub <- read.csv("results/sub1.csv", header=TRUE, sep=",")
#write.csv(submission,"results/sub2.csv", row.names = FALSE)
write.csv(submission,"results/sub3.csv", row.names = FALSE)

#loc_3sec <- vec_loc
#ans_3sec <- vec_ans
################################################################