load("C:/Users/Public/Documents/Capstone Project/final_grouped.RData")
library(epiR)
#final_raw[sapply(final_grouped, is.character)] <- lapply(final_grouped[sapply(final_grouped, is.character)], 
#as.factor)

final_grouped$Language<-ifelse(final_grouped$Language%in%c("English", "Spanish"), 
                               final_grouped$Language, "Other")
final_grouped$sex<-ifelse(final_grouped$sex=="M", 1, 0)

#Check which variables are not numeric
take <- sapply(final_grouped, is.numeric)
which(take == FALSE) 

#Convert categorical variables to dummy variables
final_dummy <- data.frame(final_grouped[ , ! colnames(final_grouped) %in% "Plan Name"],       # Create dummy data
                          model.matrix( ~ `Plan Name` - 1, final_grouped))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "RSA_Region"],       # Create dummy data
                          model.matrix( ~ `RSA_Region` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "Language"],       # Create dummy data
                          model.matrix( ~ `Language` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "race"],       # Create dummy data
                          model.matrix( ~ `race` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "RISK"],       # Create dummy data
                          model.matrix( ~ `RISK` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "MER"],       # Create dummy data
                          model.matrix( ~ `MER` - 1, final_dummy))

#Make sure all variables are numeric
take <- sapply(final_dummy, is.numeric)
which(take == FALSE)


library(caret)
library(tidyverse)
library(dplyr)
library(glmnet)
library(emulator)
library(data.table)
library(MLmetrics)
library(pROC)

load("C:/Users/Public/Documents/Capstone Project/final_grouped.RData")

final_grouped$Language<-ifelse(final_grouped$Language%in%c("English", "Spanish"), 
                               final_grouped$Language, "Other")
final_grouped$sex<-ifelse(final_grouped$sex=="M", 1, 0)

#Check which variables are not numeric
take <- sapply(final_grouped, is.numeric)
which(take == FALSE) 

#Convert categorical variables to dummy variables
final_dummy <- data.frame(final_grouped[ , ! colnames(final_grouped) %in% "Plan Name"],       # Create dummy data
                          model.matrix( ~ `Plan Name` - 1, final_grouped))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "RSA_Region"],       # Create dummy data
                          model.matrix( ~ `RSA_Region` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "Language"],       # Create dummy data
                          model.matrix( ~ `Language` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "race"],       # Create dummy data
                          model.matrix( ~ `race` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "RISK"],       # Create dummy data
                          model.matrix( ~ `RISK` - 1, final_dummy))

final_dummy <- data.frame(final_dummy[ , ! colnames(final_dummy) %in% "MER"],       # Create dummy data
                          model.matrix( ~ `MER` - 1, final_dummy))

#Make sure all variables are numeric
take <- sapply(final_dummy, is.numeric)
which(take == FALSE)


set.seed(2022)
index<-createDataPartition(final_dummy$IP_IND, p=0.8, list=F, times=1)
train_df<-final_dummy[index, ]
test_df<-final_dummy[-index, ]

#concert certain columns to factors
cols <- c(11:19, 23:27, 29:84)
train_df[,cols] <- lapply(train_df[,cols] , factor)

#find numerical variables and scale them
train_num = train_df[,sapply(train_df, is.numeric)]
train_num<-scale(train_num)
train_num<-as.data.frame(train_num)

#combine numeric variables and factor variables together
train_df<-train_df[,c(11:19, 23:27, 29:84)]
train_df<-cbind(train_df, train_num)

#transform all columns back to numeric
i<-1:84
train_df[,i]<-apply(train_df[,i], 2, function(x) as.numeric(as.factor(x)))
#train_df<-train_df[, -c(16, 20, 28, 30:35)] 
#train_df<-train_df[, -c(15:26)]


#concert certain columns to factors
test_df[,cols] <- lapply(test_df[,cols] , factor)

#find numerical variables and scale them
test_num = test_df[,sapply(test_df, is.numeric)]
test_num<-scale(test_num)
test_num<-as.data.frame(test_num)

#combine numeric variables and factor variables together
test_df<-test_df[,c(11:19, 23:27, 29:84)]
test_df<-cbind(test_df, test_num)

#transform all columns back to numeric
test_df<-data.frame(sapply(test_df, function(x) as.numeric(as.factor(x))))
#test_df<-test_df[, -c(16, 20, 28, 30:35)]
#test_df<-train_df[, -c(15:26)]


fit.cv <- cv.glmnet(x=data.matrix(train_df[,-13]), train_df[,13], 
                    alpha=1, nlambda=100, family="binomial")

lasso.coef <- fit.cv$glmnet.fit$beta[,which(fit.cv$lambda == fit.cv$lambda.min)]

# predict on test 
prediction_test<-predict(fit.cv, newx=as.matrix(test_df[-13]), type="response")
prediction_test<-as.data.frame(prediction_test)
prediction_test$s2.test<-ifelse(prediction_test$lambda.1se>=0.095, 2, 1)
s2.test<-as.factor(prediction_test$s2)

actual<-as.factor(test_df$IP_IND)
confusionMatrix(s2.test, actual, mode = "everything", positive="2")

data <- as.table(matrix(c(367,1104,1170,33338), nrow = 2, byrow = TRUE))
rval <- epi.tests(data, conf.level = 0.95, digits=3)
print(rval)

auc(roc(test_df$IP_IND, prediction_test$s2))

# predict on training 
prediction_train<-predict(fit.cv, newx=as.matrix(train_df[-13]), type="response")
prediction_train<-as.data.frame(prediction_train)
prediction_train$s2.train<-ifelse(prediction_train$lambda.1se>=0.075, 2, 1)
s2.train<-as.factor(prediction_train$s2.train)

actual<-as.factor(train_df$IP_IND)
confusionMatrix(s2.train, actual, mode = "everything", positive="2")

#Apply CND
train_df$raceAsian<-train_df$raceAsian - 1
train_df$raceAmerican.Indian.or.Alaska.Native<-train_df$raceAmerican.Indian.or.Alaska.Native-1
train_df$raceBlack.or.African.American<-train_df$raceBlack.or.African.American-1
train_df$raceDeclined<-train_df$raceDeclined-1
train_df$raceHispanic.or.Latino<-train_df$raceHispanic.or.Latino-1
train_df$raceNative.Hawaiian.or.Other.Pacific.Islander<-train_df$raceNative.Hawaiian.or.Other.Pacific.Islander-1
train_df$raceOther<-train_df$raceOther-1
train_df$raceWhite<-train_df$raceWhite-1
train_df$IP_IND<-train_df$IP_IND-1


cndMassageData <- function(data, pred.p){
  
  # library requirement: 'dplyr', 'knitr'
  # input:
  #   data: the dataframe that needs massaging, i.e. the training data
  # return:
  #   CND.res: a list of two dataframes:
  #   1. data.new: massaged data
  #   2. race.comparison: a detailed table for race distribuion
  
  ### investigate the racial bias within training data
  race.cnt.overall <- sapply(select(data,contains("race")), sum)
  race.prop.overall <- sapply(select(data,contains("race")), mean)
  race.cnt.IP <- sapply(select(data[data$IP_IND == 1,],contains("race")), sum)
  race.expected.cnt.IP <- round(race.prop.overall * sum(data$IP_IND))
  diff <- race.cnt.IP - race.expected.cnt.IP
  race.comparison <- data.frame(overall.cnt = race.cnt.overall,
                                overall.prop = race.prop.overall,
                                IP.cnt = race.cnt.IP,
                                IP.expected.cnt = race.expected.cnt.IP,
                                diff = diff)
  
  ### get promotion and demotion group
  profiter <- race.comparison %>% filter(diff > 0) %>% select(diff)
  victim <- race.comparison %>% filter(diff < 0) %>% select(diff)
  
  ### label swapping
  # copy the data and attach predicted probability
  data.new <- data
  data.new$pred.p <- pred.p
  
  ### change labels from demotion group from 1 to 0
  # loop through all profiter races
  
  for (i in 1:nrow(profiter)){
    
    # column index for the race
    col_idx <- which(names(data.new) == rownames(profiter)[i]) 
    
    # number of swap needed for this race
    count <- profiter[i,] 
    
    # row index for demotion group
    row_idx <- which(data.new[,col_idx] == 1 & data.new$IP_IND == 1) 
    
    # set a new column and copy predicted probability only for promotion group
    data.new$use.p <- 1
    data.new$use.p[row_idx] <- data.new$pred.p[row_idx]
    
    # row index for demotion group member with top predicted probability
    swap_idx <- sort(data.new$use.p, index.return=TRUE, decreasing=FALSE)[["ix"]][1:count]  
    
    # change label from 1 to 0
    if (length(swap_idx) == count & sum(data.new[swap_idx,]$IP_IND) == count){
      data.new[swap_idx,]$IP_IND = 0
    } else {
      stop('error in CND')
    }
  }
  
  ### change labels from demotion group from 0 to 1
  # loop through all victim races
  
  for (i in 1:nrow(victim)){
    
    # column index for the race
    col_idx <- which(names(data.new) == rownames(victim)[i])
    
    # number of swap needed for this race
    count <- abs(victim[i,])
    
    # row index for promotion group
    row_idx <- which(data.new[,col_idx] == 1 & data.new$IP_IND == 0)
    
    # set a new column and copy predicted probability only for demotion group
    data.new$use.p <- 0 
    data.new$use.p[row_idx] <- data.new$pred.p[row_idx]
    
    # row index for demotion group member with top predicted probability
    swap_idx <- sort(data.new$use.p, index.return=TRUE, decreasing=TRUE)[["ix"]][1:count]
    
    # change label from 0 to 1
    if (length(swap_idx) == count & sum(data.new[swap_idx,]$IP_IND) == 0){
      data.new[swap_idx,]$IP_IND = 1
    } else {
      stop('error in CND')
    }
  }
  
  # drop the pred.p and use.p columns
  data.new <- data.new %>% select(-pred.p, -use.p)
  
  ### count in each race in the corrected
  race.corrected.cnt.IP <- sapply(select(data.new[data.new$IP_IND == 1,],contains("race")), sum)
  race.comparison <- cbind(race.comparison, IP.corrected.cnt = race.corrected.cnt.IP)
  race.comparison <- rbind(race.comparison, Total = colSums(race.comparison))
  
  CND.res <- list('data.new' = data.new, 'race.comparison' = race.comparison)
  
  return(CND.res)
  
}

prediction_train<-predict(fit.cv, newx=as.matrix(train_df[-13]), type="response")
CND.res <- cndMassageData(train_df, prediction_train)
train_df.new <- CND.res$data.new

test_df$raceAsian<-test_df$raceAsian - 1
test_df$raceAmerican.Indian.or.Alaska.Native<-test_df$raceAmerican.Indian.or.Alaska.Native-1
test_df$raceBlack.or.African.American<-test_df$raceBlack.or.African.American-1
test_df$raceDeclined<-test_df$raceDeclined-1
test_df$raceHispanic.or.Latino<-test_df$raceHispanic.or.Latino-1
test_df$raceNative.Hawaiian.or.Other.Pacific.Islander<-test_df$raceNative.Hawaiian.or.Other.Pacific.Islander-1
test_df$raceOther<-test_df$raceOther-1
test_df$raceWhite<-test_df$raceWhite-1
test_df$IP_IND<-test_df$IP_IND-1

fit.cv.new <- cv.glmnet(x=data.matrix(train_df.new[,-13]), train_df.new[,13], 
                        alpha=1, nlambda=100, family="binomial")
lasso.coef.new <- fit.cv.new$glmnet.fit$beta[,which(fit.cv.new$lambda == fit.cv.new$lambda.min)]


#prediction on test set
prediction_test.new<-predict(fit.cv.new, newx=as.matrix(test_df[-13]), type="response")
prediction_test.new<-as.data.frame(prediction_test.new)
prediction_test.new$s2.new<-ifelse(prediction_test.new$lambda.1se>=0.115, 1, 0)
s2.new<-as.factor(prediction_test.new$s2)

actual<-as.factor(test_df$IP_IND)
confusionMatrix(s2.new, actual, mode = "everything", positive="1")
data <- as.table(matrix(c(373,1197,1164,33245), nrow = 2, byrow = TRUE))
rval <- epi.tests(data, conf.level = 0.95, digits=3)
print(rval)

auc(roc(test_df$IP_IND, prediction_test.new$s2.new))

#prediction on training set
prediction_training.new<-predict(fit.cv.new, newx=as.matrix(train_df.new[-13]), type="response")
prediction_training.new<-as.data.frame(prediction_training.new)
prediction_training.new$s2.new<-ifelse(prediction_training.new$lambda.1se>=0.075, 1, 0)
s2.new<-as.factor(prediction_training.new$s2)

actual<-as.factor(train_df.new$IP_IND)
confusionMatrix(s2.new, actual, mode = "everything", positive="1")

auc(roc(test_df$IP_IND, prediction_test.new$s2.new))


lasso.coef.new <- coef(fit.cv.new, s=fit.cv.new$lambda.min)
var<-rownames(lasso.coef.new)
coef<-lasso.coef.new[,1]
coef<-cbind(var, coef)
coef<-as.data.frame(coef)
rownames(coef)<-NULL

coef<-coef[-1,]
coef$coef<-as.numeric(coef$coef)
coef<-coef%>%arrange(desc(abs(coef)))
coef$abs<-abs(coef$coef)
coef<-coef[1:10,]

ggplot(coef, aes(x=reorder(var, +abs), y=abs, main="Importance of Predictors")) +
  geom_bar(stat="identity", fill="lightblue") + coord_flip() + 
  scale_y_continuous(name="Absolute Value of Coefficient Estimate") +
  scale_x_discrete(name="Variable Name") + theme_classic()

