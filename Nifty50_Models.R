niftydata <- read.csv("/home/abhishek/Documents/DataScience/Projects/Nifty_50_Prediction/Data Sets/NiftyDataV1.csv", header = T)
niftydata$X <- NULL
head(niftydata)
str(niftydata)

colSums(is.na(niftydata))
colSums(is.na(ms_data))

library(dplyr)
fun_checkClassification <- function(x,y){
  val <- 0.5
  while(val >= 0.1){
    a<-table(x, y > val)
    a<-data.frame(a)
    
    falseTotal <- sum(a[a$x==0,]$Freq) 
    trueTotal <- sum(a[a$x==1,]$Freq) 
    
    a1<-a %>% filter(x == 0 & Var2 == FALSE) %>% select(Freq) /falseTotal 
    a2<-a %>% filter(x == 1 & Var2 == TRUE) %>% select(Freq) /trueTotal 
    
    cat("For CutOff Value",val)
    cat("\n FALSE Accurate Prediction% :", round(a1$Freq,2))
    cat("\n TRUE Accurate Prediction% :", round(a2$Freq,2))
    cat("\n---------------------------------------------\n")
    val <- val - 0.1
    rm(a)
    rm(falseTotal)
    rm(trueTotal)
    rm(a1)
    rm(a2)
  }
}

#Lets build the model
niftymodel_blr <- glm(NSEI.Direction~NSEI.High+NSEI.Low+NSEI.Close+INR.X.LogPVTVReturn+GBPINR.X.LogPVTVReturn+
                        JPYINR.X.LogPVTVReturn+GOLD.LogPVTVReturn+SILVER.LogPVTVReturn+
                        GSPC.LogPCTCReturn+N225.LogTOPCReturn+NSEI.LogTOPCReturn+
                        SGX.LogTOPCReturn, data=niftydata, family="binomial")  

summary(niftymodel_blr)

#Null model
niftymodel_blr_null <- glm(NSEI.Direction~1, data=niftydata, family="binomial")  

#Applying stepwise to find the final model
step(niftymodel_blr, scope = list(lower=niftymodel_blr_null, upper=niftymodel_blr), direction = "both")

#Final model got from stepwise
niftymodel_blr <- glm(formula = NSEI.Direction ~ NSEI.High + NSEI.Low + JPYINR.X.LogPVTVReturn + 
                        SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, family = "binomial", 
                      data = niftydata)

summary(niftymodel_blr)

#Checking multi-colinearity
library(car)
vif(niftymodel_blr)

#NSEI.High is Multicolinear

niftymodel_blr <- glm(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
                        SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, family = "binomial", 
                      data = niftydata)

summary(niftymodel_blr)

#Now dividing the data into train and test 
library(caret)
index <- createDataPartition(niftydata$NSEI.Direction, p=0.7, list = F)
nifty_train_data <- niftydata[index,]
nifty_test_data <- niftydata[-index,]

############# MODEL 1 ##############

nifty_train_model_blr <- glm(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
                        SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, family = "binomial", 
                      data = nifty_train_data)
summary(nifty_train_model_blr)

#Lets Start with Prediction with Train Data
library(ROCR)
nifty_train_data$Prob_Direction <- fitted(nifty_train_model_blr)
pred <- prediction(nifty_train_data$Prob_Direction, nifty_train_data$NSEI.Direction)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

#Lets check the Train model Goodness of Fit
auc <- performance(pred, "auc")
auc@y.values

fun_checkClassification(nifty_train_data$NSEI.Direction, nifty_train_data$Prob_Direction)

#From O/P 0.5 is the recommended cutoff
nifty_train_data$Pred_Direction <- ifelse(nifty_train_data$Prob_Direction > 0.5, 1, 0)
confusionMatrix(factor(nifty_train_data$Pred_Direction), factor(nifty_train_data$NSEI.Direction))

#Lets predict the test data
nifty_test_data$Prob_Direction <- predict(nifty_train_model_blr, newdata=nifty_test_data, type = "response")
nifty_test_data$Pred_Direction <- ifelse(nifty_test_data$Prob_Direction > 0.5, 1, 0)

#Lest Check the MOdel Goodness of Fit on Test Data
predTest <- prediction(nifty_test_data$Prob_Direction, nifty_test_data$NSEI.Direction)
perfTest <- performance(predTest, "tpr", "fpr")
plot(perfTest)
abline(0,1)
aucTest <- performance(predTest, "auc")
aucTest@y.values # 58%

confusionMatrix(factor(nifty_test_data$Pred_Direction), factor(nifty_test_data$NSEI.Direction)) 
#Accuracy : 60%

#Model 1
nifty_train_model_blr_model1 <- nifty_train_model_blr

################### MODEL 2 #####################
nifty_train_model_blr_model2 <- glm(formula = NSEI.Direction ~ NSEI.Low + NSEI.LogTOPCReturn, family = "binomial", 
                             data = nifty_train_data)

summary(nifty_train_model_blr_model2)

step(nifty_train_model_blr_model2, scope = list(lower=niftymodel_blr_null, upper=nifty_train_model_blr_model2), direction = "both")

nifty_train_model_blr_model2 <- glm(formula = NSEI.Direction ~ NSEI.LogTOPCReturn, family = "binomial", 
                             data = nifty_train_data)

Prob_Direction_m2 <- fitted(nifty_train_model_blr_model2)
predM2 <- prediction(Prob_Direction_m2, nifty_train_data$NSEI.Direction)
perfM2 <- performance(predM2, "tpr", "fpr")
plot(perfM2)
abline(0,1)

#Lets check the Train model Goodness of Fit
aucM2 <- performance(predM2, "auc")
aucM2@y.values

fun_checkClassification(nifty_train_data$NSEI.Direction, Prob_Direction_m2)

#Lets predict the test data
Prob_Direction_test_m2 <- predict(nifty_train_model_blr_model2, newdata=nifty_test_data, type = "response")
Pred_Direction_m2 <- ifelse(Prob_Direction_test_m2 > 0.5, 1, 0)

#Lest Check the MOdel Goodness of Fit on Test Data
predTest <- prediction(Prob_Direction_test_m2, nifty_test_data$NSEI.Direction)
perfTest <- performance(predTest, "tpr", "fpr")
plot(perfTest)
abline(0,1)
aucTest <- performance(predTest, "auc")
aucTest@y.values # 66% with model 2

confusionMatrix(factor(Pred_Direction_m2), factor(nifty_test_data$NSEI.Direction)) 
#Accuracy : 63% with model 2


############# MODEL 3 ################

#Convert all independent variables to binary
nifty_data01 <- niftydata

nifty_data01$NSEI.Close             <- ifelse(nifty_data01$NSEI.Close > 0, 1 ,0)
nifty_data01$NSEI.High              <- ifelse(nifty_data01$NSEI.High > 0, 1 ,0)
nifty_data01$NSEI.Low               <- ifelse(nifty_data01$NSEI.Low > 0, 1 ,0)
nifty_data01$NSEI.LogPCTCReturn     <- ifelse(nifty_data01$NSEI.LogPCTCReturn > 0, 1 ,0)
nifty_data01$NSEI.LogTOPCReturn     <- ifelse(nifty_data01$NSEI.LogTOPCReturn > 0, 1 ,0)
nifty_data01$INR.X.LogPVTVReturn    <- ifelse(nifty_data01$INR.X.LogPVTVReturn > 0, 1 ,0)
nifty_data01$GBPINR.X.LogPVTVReturn <- ifelse(nifty_data01$GBPINR.X.LogPVTVReturn > 0, 1 ,0)
nifty_data01$JPYINR.X.LogPVTVReturn <- ifelse(nifty_data01$JPYINR.X.LogPVTVReturn > 0, 1 ,0)
nifty_data01$CrudeOil.LogPVTVReturn <- ifelse(nifty_data01$CrudeOil.LogPVTVReturn > 0, 1 ,0)
nifty_data01$GOLD.LogPVTVReturn     <- ifelse(nifty_data01$GOLD.LogPVTVReturn > 0, 1 ,0)
nifty_data01$SILVER.LogPVTVReturn   <- ifelse(nifty_data01$SILVER.LogPVTVReturn > 0, 1 ,0)
nifty_data01$GSPC.LogPCTCReturn     <- ifelse(nifty_data01$GSPC.LogPCTCReturn > 0, 1 ,0)
nifty_data01$N225.LogTOPCReturn     <- ifelse(nifty_data01$N225.LogTOPCReturn > 0, 1 ,0)
nifty_data01$SGX.LogTOPCReturn      <- ifelse(nifty_data01$SGX.LogTOPCReturn > 0, 1 ,0)
nifty_data01$N225.LogTOPCReturn     <- ifelse(nifty_data01$N225.LogTOPCReturn > 0, 1 ,0)

index01 <- createDataPartition(nifty_data01$NSEI.Direction, p = 0.7, list = F)
nifty_train_data01 <- nifty_data01[index01,]
nifty_test_data01 <- nifty_data01[-index01,]

nifty_train_blr_model3 <- glm(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
                               SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, family = "binomial", 
                             data = nifty_train_data01)

summary(nifty_train_blr_model3) #NSEI.Low,JPYINR.X.LogPVTVReturn is not significant
 
step(nifty_train_blr_model3, scope = list(lower=niftymodel_blr_null, upper=nifty_train_blr_model3),
     direction = "both")

nifty_train_blr_model3 <- glm(formula = NSEI.Direction ~ SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, 
                             family = "binomial", data = nifty_train_data01)

#Lets start the prediction on test data
nifty_train_data01$Prob_Direction <- fitted(nifty_train_blr_model3)
pred01 <- prediction(nifty_train_data01$Prob_Direction, nifty_train_data01$NSEI.Direction)
perf01 <- performance(pred01, "tpr", "fpr")
plot(perf01)
abline(0,1)

#Lets check the Train model Goodness of Fit
auc01 <- performance(pred01, "auc")
auc01@y.values #71% on train data

fun_checkClassification(nifty_train_data01$NSEI.Direction, nifty_train_data01$Prob_Direction)

#From O/P 0.5 is the recommended cutoff
nifty_train_data01$Pred_Direction <- ifelse(nifty_train_data01$Prob_Direction > 0.5, 1, 0)
confusionMatrix(factor(nifty_train_data01$Pred_Direction), factor(nifty_train_data01$NSEI.Direction))
#Acccuracy : 68 % on Train Data

#Lets predict the test data
nifty_test_data01$Prob_Direction <- predict(nifty_train_blr_model3, newdata=nifty_test_data01, type = "response")
nifty_test_data01$Pred_Direction <- ifelse(nifty_test_data01$Prob_Direction > 0.5, 1, 0)

#Lest Check the MOdel Goodness of Fit on Test Data
predTest <- prediction(nifty_test_data01$Prob_Direction, nifty_test_data01$NSEI.Direction)
perfTest <- performance(predTest, "tpr", "fpr")
plot(perfTest)
abline(0,1)
aucTest <- performance(predTest, "auc")
aucTest@y.values # 62% 0n Test Data

confusionMatrix(factor(nifty_test_data01$Pred_Direction), factor(nifty_test_data01$NSEI.Direction)) 
#Accuracy : 62%

################## MODEL 4 ####################
nifty_train_model_blr_model4 <- glm(formula = NSEI.Direction ~ NSEI.Low + NSEI.LogTOPCReturn, family = "binomial", 
                                    data = nifty_train_data01)

summary(nifty_train_model_blr_model4)

step(nifty_train_model_blr_model4, scope = list(lower=niftymodel_blr_null, upper=nifty_train_model_blr_model4), direction = "both")

nifty_train_model_blr_model4 <- glm(formula = NSEI.Direction ~ NSEI.LogTOPCReturn, family = "binomial", 
                                    data = nifty_train_data)

Prob_Direction_m4 <- fitted(nifty_train_model_blr_model4)
predM4 <- prediction(Prob_Direction_m4, nifty_train_data01$NSEI.Direction)
perfM4 <- performance(predM4, "tpr", "fpr")
plot(perfM4)
abline(0,1)

#Lets check the Train model Goodness of Fit
aucM4 <- performance(predM4, "auc")
aucM4@y.values #48% on Train Data

fun_checkClassification(nifty_train_data01$NSEI.Direction, Prob_Direction_m4)

#Lets predict the test data
Prob_Direction_test_m4 <- predict(nifty_train_model_blr_model4, newdata=nifty_test_data01, type = "response")
Pred_Direction_m4 <- ifelse(Prob_Direction_test_m4 > 0.5, 1, 0)

#Lest Check the MOdel Goodness of Fit on Test Data
predTest <- prediction(Prob_Direction_test_m4, nifty_test_data01$NSEI.Direction)
perfTest <- performance(predTest, "tpr", "fpr")
plot(perfTest)
abline(0,1)
aucTest <- performance(predTest, "auc")
aucTest@y.values # 64% on test data

confusionMatrix(factor(Pred_Direction_m2), factor(nifty_test_data$NSEI.Direction)) 
#Accuracy : 63% with model 4

#################### MODEL 5 #################
library(e1071)
nifty_train_nb_model5 <- naiveBayes(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
                           SILVER.LogPVTVReturn + NSEI.LogTOPCReturn,data = niftydata)

nifty_train_nb_model5

#Finding the Goodness of Fit using ROCR
Prob <- predict(nifty_train_nb_model5, nifty_train_data, type="raw")
Prob_nb <- Prob[,2]  
predNB <- prediction(Prob_nb, nifty_train_data$NSEI.Direction)
perfNB <- performance(predNB, "tpr", "fpr")
plot(perfNB)
abline(0,1)
aucNB <- performance(predNB, "auc")
aucNB@y.values # 69% on train data

Pred_Dir_NB<-ifelse(Prob_nb>0.25,1,0)
confusionMatrix(factor(Pred_Dir_NB),factor(nifty_train_data$NSEI.Direction))
#Accuracy 59% on train data

#Check the test data
Prob_test <- predict(nifty_train_nb_model5, nifty_test_data, type="raw")
Prob_nb_test <- Prob_test[,2]  
predNB_test <- prediction(Prob_nb_test, nifty_test_data$NSEI.Direction)
perfNB_test <- performance(predNB_test, "tpr", "fpr")
plot(perfNB_test)
abline(0,1)
aucNB_test <- performance(predNB_test, "auc")
aucNB_test@y.values # 74% on test data

Pred_Dir_NB_test<-ifelse(Prob_nb_test>0.25,1,0)
confusionMatrix(factor(Pred_Dir_NB_test),factor(nifty_test_data$NSEI.Direction))
#Acccuracy 56% on test data

################# MODEL 6 ###################
str(nifty_train_data)
nifty_data_knn <- subset(niftydata, select = c(-date))
str(nifty_data_knn)

index_knn <- createDataPartition(nifty_data_knn$NSEI.Direction, p=0.7, list=F)
nifty_data_knn_scale <- scale(nifty_data_knn)
#After Scaling CrudeOil.LogPVTVReturn is getting Nan- need to check this, buy the mean time we are removing crude oil from this.
nifty_data_knn$CrudeOil.LogPVTVReturn <- NULL
nifty_data_knn_scale <- scale(nifty_data_knn)

knn_train <- nifty_data_knn_scale[index_knn,]
knn_test  <- nifty_data_knn_scale[-index_knn,]

#Creating class vectors
YTrain <- niftydata$NSEI.Direction[index_knn]
YTest <- niftydata$NSEI.Direction[-index_knn]

sqrt(nrow(knn_train))

#Lets find the value of K
trctrl_knn <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(NSEI.Direction ~., data = knn_train, method = "knn",
                 trControl=trctrl_knn ,preProcess = c("center", "scale"),tuneLength = 10)
knn_fit

library(class)
niftymodel_knn_model6 <- knn(knn_train, knn_test, k=7, cl=YTrain)
table(YTest, niftymodel_knn_model6)

################### MODEL 7 #######################
install.packages("partykit")
install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
library(CHAID)
library(partykit)

nifty_train_data$NSEI.Direction <- as.factor(nifty_train_data$NSEI.Direction)
nifty_test_data$NSEI.Direction <- as.factor(nifty_test_data$NSEI.Direction)
ctree<-ctree(formula=NSEI.Direction ~ NSEI.LogTOPCReturn+NSEI.LogPCTCReturn+NSEI.Low+NSEI.High+NSEI.Close+
                         INR.X.LogPVTVReturn+GBPINR.X.LogPVTVReturn+JPYINR.X.LogPVTVReturn+CrudeOil.LogPVTVReturn+
                         GOLD.LogPVTVReturn+SILVER.LogPVTVReturn+GSPC.LogPCTCReturn+N225.LogTOPCReturn+SGX.LogTOPCReturn,
                       data=nifty_train_data)

ctree<-partykit::ctree(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
               SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, 
             data = nifty_train_data)

plot(ctree, type="simple")

predtree<-predict(ctree,nifty_test_data,type="prob")
library(ROCR)
pred<-prediction(predtree[,2],nifty_test_data$NSEI.Direction)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
## Area under ROC Curve in R (AUC)
auc<-performance(pred,"auc")
auc@y.values #61%

################### MODEL 8 #######################
install.packages("randomForest")
library(randomForest)

rf<-randomForest(formula = NSEI.Direction ~ NSEI.Low + JPYINR.X.LogPVTVReturn + 
                         SILVER.LogPVTVReturn + NSEI.LogTOPCReturn, 
                       data = nifty_train_data, mtry=2, ntree=100, 
                       importance = T, cutoff =c(0.6,0.4))
rf

plot(rf)

predict(rf, nifty_train_data)

predrf <- predict(rf,nifty_test_data, type = "vote", norm.votes = TRUE)
library(ROCR)
pred<-prediction(predrf[,2],nifty_test_data$NSEI.Direction)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
## Area under ROC Curve in R (AUC)
auc<-performance(pred,"auc")
auc@y.values #58%

