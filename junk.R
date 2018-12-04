library(dplyr)
library(tidyr)
library(readr)
library(zoo)


# A selected time series of consumption data for selected meters within 3 different sites.
system.time(train <- read_csv("DrivenData_PowerLaws_DetectingAnomaliesInUsage/train.csv", col_types="ic?n"))
names(train)[1] <- "obs_id"
print(paste("Initial read of train.csv, dimensions:", dim(train)[1], "x", dim(train)[2]))

# Additional information about the included buildings.
building <- read_csv("DrivenData_PowerLaws_DetectingAnomaliesInUsage/metadata.csv")

# join
train <- left_join(train, building, by="meter_id")

# convert Wh to kWh
train$Values[train$units=="Wh" & 
               !is.na(train$units)] <- train$Values[train$units=="Wh" & 
                                                      !is.na(train$units)]/1000 
train$units[train$units=="Wh" & 
              !is.na(train$units)] <- "kWh" # and adjust the unit

# Historical Weather Data
weather <- read_csv("DrivenData_PowerLaws_DetectingAnomaliesInUsage/weather.csv", col_types="i?nnc")
weather$site_id[weather$site_id=="38"] <- "038" #the leading zero appears in the main dataset
# TODO group by site, average, join to train, check for NAs

# Public Holidays
holidays <- read_csv("DrivenData_PowerLaws_DetectingAnomaliesInUsage/holidays.csv")


# The format for the submission to the competition.
# We only are evaluating is_abnormal against a subset of the meter_ids that are in the consumption 
# dataset. Competitors can use the entire consumption dataset to create their algorithms, but 
# should only submit the meters and times that appear in the submission format.

test <- read_csv("DrivenData_PowerLaws_DetectingAnomaliesInUsage/submission_format.csv")
test_meters <- levels(as.factor(test$meter_id)) # OK, so what meters are in in submission_format file?print(test_meters)


for (i in 1:length(test_meters)) {
  plot(select(filter(train, meter_id==test_meters[i]), Timestamp, Values))
}


for (i in 1:length(test_meters)) {
  print(paste(test_meters[i],
              "has",
              sum(is.na(train$Values[train$meter_id==test_meters[i]])),
              "NAs"))
}

# We could look to another meter in the building for a good correlation:
filter(train, meter_id %in% c(test_meters[2], "2")) %>% select(Values, Timestamp, meter_id) %>% spread(meter_id, Values) %>% na.omit() %>% select(-Timestamp) %>% cor()

# But, the oher meter might also have NAs at the same Timestamp:
filter(train, meter_id %in% c(test_meters[2], "2")) %>% select(Values, Timestamp, meter_id) %>% spread(meter_id, Values) %>% tail()
# that there are NAs at the end of the dataset also presents problems for interpolating to replace NAs

# For thresholding on deltas, we can just fill the NAs with the preceding value:
train_locf <- select(train, meter_id, Timestamp, Values) %>% filter(meter_id %in% test_meters)
for (i in 1:length(test_meters)) {
  print(paste("meter_id: ", test_meters[i]))
  print(paste("NAs:", sum(is.na(train_locf$Values[train_locf$meter_id==test_meters[i]]))))
  train_locf$Values[train_locf$meter_id==test_meters[i]] <- na.locf(object=
                                                                      train_locf$Values[train_locf$meter_id==test_meters[i]],
                                                                    fromLast=FALSE, 
                                                                    na.rm=FALSE)
  print(paste("NAs after LOCF:", sum(is.na(train_locf$Values[train_locf$meter_id==test_meters[i]]))))
  train_locf$Values[train_locf$meter_id==test_meters[i]] <- na.locf(object=
                                                                      train_locf$Values[train_locf$meter_id==test_meters[i]],
                                                                    fromLast=TRUE, 
                                                                    na.rm=FALSE)
  print(paste("NAs after NOCB:", sum(is.na(train_locf$Values[train_locf$meter_id==test_meters[i]]))))