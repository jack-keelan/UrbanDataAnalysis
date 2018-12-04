##  This is just the essentials frozen for submission numberr one.
## run some random forest business on my source features
library(caret)
library(randomForest)

# read in answers
answers <- read.csv("data/answers.csv", header = TRUE, sep = ",")
answers <- answers[,-4]  #  They are stupid about their header commas

#you need to reset the working directory
setwd("C:/Users/mooreet_la/projects/SDRD/competition/")
df.mod <- read.csv("data/TNG_set.csv")
df.mod$SourceID <- factor(df.mod$SourceID)

### Training Set  the first half or quater??
df.mod <- read.csv("data/TNG_set128_v2.csv")
df.mod$runID <- factor(df.mod$runID)
df.mod$SourceID <- factor(df.mod$SourceID)

nbins <- 128

# Split data into training and testing set
set.seed(102)
train.indices <- c(1:(nrow(df.mod)/2) )
train <- df.mod[train.indices,]
test <- df.mod[-train.indices,]

train <- train[1:(nrow(train)/2),]  ## Not sure the quartering happened!!
test <- test[1:(nrow(test)/2),]

## features are channels and the ID
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

# Display the results of the cross validation run
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

#save the random forest model to use later
#saveRDS(rf.best, file="RF_50per_128ch.rds")

### read in a saved model
## I actually don't think this is working
## please fix it for me :)  Needed to save rf.cv not rf.best!!!!!!!!
#mod <- readRDS("RF_50per_128ch.rds")
#preds <- predict(mod, test, type = "response")

#####################################################################
#####################################################################


# Create predictions for the test set  
# Not the submission
true_test <- answers[4901:7350,]

pred_val <- stats_test$prediction
true_val <- as.factor(true_test$SourceID)

true_loc <- round(true_test$SourceTime)
true_loc <- (true_loc %/% 20) * 20

confMTX.evts <- confusionMatrix(as.factor(vec_ans),true_val)
confMTX.evts
vec_locV2 <- (vec_loc %/% 20) * 20
compare.loc <- as.data.frame(cbind(vec_locV2,true_loc))
compare.loc$diff <- true_loc - vec_locV2
stats_comp <- as.data.frame(count(compare.loc,diff) )
plot(stats_comp$diff,stats_comp$n)
#####
sum(stats_comp$n)
stats_comp$diff
sum(stats_comp$n) - stats_comp$n[23]
####

###############################################################
# Create predictions test example dummy  TNG_set128_roll_25s.csv

#data_path <- "data/integrated_128Ch_3sec/"
#data_path <- "data/integrated_128Ch/"
#data_path <- "data/integrated_128Ch_bkgR/"
data_path <- "data/128Ch_rolling/"
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
  
#dummy <- read.csv(paste(data_path, "104916", ".csv", sep = ""), header=FALSE, sep=",")
  names(dummy) <- colnames(df.mod[1:128])
#dim(dummy)
#  preds_dummy <- predict(rf.cv, dummy, type = "raw")  # 3 seconds
  preds_dummy <- predict(mod, dummy, type = "response")  # 1 second
#  preds_dummy <- predict(rf.cv, dummy, type = "raw")   #current bkg rm'ed
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
    vec_loc[(i-4900)] <- (((4*loc_dummy) - 3*trunc(loc_dummy) - 1)/4)
  } else {
    ans_dummy <- 0
    loc_dummy <- 0.00
    vec_loc[(i-4900)] <- loc_dummy
    }

#  vec_loc[(i-4900)] <- loc_dummy
  vec_ans[(i-4900)] <- ans_dummy
  vec_lng[(i-4900)] <- length(preds_dummy)
  
  

#mask_dummy <- as.integer(preds_dummy) == ans_dummy
#length( which(as.integer(preds_dummy) %in% c(ans_dummy)) )
  
  if(i %% 200 == 0){cur_time <- Sys.time() - st_time; print(i); print(cur_time)}
}

#loc_3sec <- vec_loc
#ans_3sec <- vec_ans
###############################################################
# Create predictions for submission
##############################################################
#data_path <- "data/integrated_128Ch_3sec/"
#data_path <- "data/testingData/integrated_128Ch/"
data_path <- "data/testingData/128Ch_rolling/"
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
  names(dummy) <- colnames(df.mod[1:128])
  preds_dummy <- predict(mod, dummy, type = "response")  # 1 second
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
write.csv(submission,"results/sub2.csv", row.names = FALSE)

#loc_3sec <- vec_loc
#ans_3sec <- vec_ans
################################################################