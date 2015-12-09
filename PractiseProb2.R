setwd("D:/OnlineHack/Practise Problem/Practise Problem2")

train <- read.csv("train_u6lujuX.csv")
test <- read.csv("test_Y3wMUE5.csv")


### missing values
table(is.na(train))
table(is.na(test))


### train data
str(train)
summary(train)

### setting missing label as 999
table(train$Gender)
levels(train$Gender) <- c("999","Female","Male" )
levels(test$Gender) <- c("999","Female","Male" )

table(train$Married)
levels(train$Married)[levels(train$Married)==""] <- c("Yes")


table(train$Dependents)
levels(train$Dependents) <- c("999","0","1","2","3+")
levels(test$Dependents) <- c("999","0","1","2","3+")

table(train$Self_Employed)
levels(train$Self_Employed) <- c("999", "No","Yes")
levels(test$Self_Employed) <- c("999", "No","Yes")

### Tagging obs having Na's
train1 <- train
test1  <- test
train1$LoanAmountMiss <- 0
train1$LoanAmountMiss <- ifelse(is.na(train1$LoanAmount),1,0)
test1$LoanAmountMiss <- 0
test1$LoanAmountMiss <- ifelse(is.na(test1$LoanAmount),1,0)


train1$Loan_Amount_TermMiss <- 0
train1$Loan_Amount_TermMiss <- ifelse(is.na(train1$Loan_Amount_Term),1,0)
test1$Loan_Amount_TermMiss <- 0
test1$Loan_Amount_TermMiss <- ifelse(is.na(test1$Loan_Amount_Term),1,0)



train1$Credit_HistoryMiss <- 0
train1$Credit_HistoryMiss <- ifelse(is.na(train1$Credit_History),1,0)
test1$Credit_HistoryMiss <- 0
test1$Credit_HistoryMiss <- ifelse(is.na(test1$Credit_History),1,0)


train2 <- train1[,c(2:12,14:16)]
test2  <- test1[,c(2:15)]
library(mice)
set.seed(98765)
imp <- complete(mice(train2))
set.seed(98765)
imp1 <- complete(mice(test2))

train3 <- cbind(Loan_ID=train1[,c(1)],imp,Loan_Status=train2[,c(13)])
train3 <- data.frame(train3)
summary(train3)
test3 <- cbind(Loan_ID=test1[,c(1)],imp1)

##### creating dummy variable
trG <- model.matrix(~0+Gender, data=train3)
teG <- model.matrix(~0+Gender, data=test3)

trMa <-model.matrix(~0+Married, data=train3)
teMa <-model.matrix(~0+Married, data=test3)

trDep <-model.matrix(~0+Dependents, data=train3)
teDep <-model.matrix(~0+Dependents, data=test3)

trEd <-model.matrix(~0+Education, data=train3)
teEd <-model.matrix(~0+Education, data=test3)

trSE <-model.matrix(~0+Self_Employed, data=train3)
teSE <-model.matrix(~0+Self_Employed, data=test3)

trLAT <-model.matrix(~0+Loan_Amount_Term, data=train3)
teLAT <-model.matrix(~0+Loan_Amount_Term, data=test3)

trCH <-model.matrix(~0+Credit_History, data=train3)
teCH <-model.matrix(~0+Credit_History, data=test3)

trPA <-model.matrix(~0+Property_Area, data=train3)
tePA <-model.matrix(~0+Property_Area, data=test3)



train4 <- cbind(Loan_ID=train1[,c(1)],trG,trMa,trDep,trEd,trSE,train3[,c(7:9)],
                trLAT,trCH,trPA,train3[,c(13:16)])

test4 <- cbind(Loan_ID=test1[,c(1)],teG,teMa,teDep,teEd,teSE,test3[,c(7:9)],
                teLAT,teCH,tePA,test3[,c(13:15)])

#### making valid names

names(train4)<- make.names(names(train4))
names(test4)<- make.names(names(test4))

train4$Loan_Status <- as.factor(train$Loan_Status)

##### dividing in train and test set

library(caTools)
set.seed(98765)

spl <- sample.split(train4$Loan_Status, SplitRatio=0.7)
tr <- subset(train4 , spl==TRUE)
te <- subset(train4 , spl==FALSE)



#### applying logistic regression
tr1 <- tr[,c(2:28)]
te1 <- te[,c(2:28)]


modglm <- glm(Loan_Status ~ ., data=tr1 , family="binomial")

predglm <- predict(modglm, newdata=te1, type="response")

# confusion matrix
table(te1$Loan_Status, predglm > 0.5)

accuracy <- (24+122)/nrow(te1)
# 0.7891892
Train <- train4[,c(2:28)]
Test  <- test4[,c(2:27)]

modglmF <- glm(Loan_Status ~ ., data= Train , family="binomial", control=list(maxit=50))
predglmF <- predict(modglmF , newdata= Test, type="response")
predglmFS <- ifelse(predglmF >0.5, "Y" , "N")

sub <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predglmFS)
write.csv(sub, "sub.csv")
# 0.784722222222

#### rpart
library(rpart)

modCART <- rpart(Loan_Status ~ ., data=tr1)
predCART <- predict(modCART, newdata=te1, type="class")

# confusion matrix
table(te1$Loan_Status, predCART)
accuracy = (23+126)/nrow(te1)
# 0.8054054

modCARTF <- rpart(Loan_Status ~ ., data=Train)
predCARTF <- predict(modCART, newdata=Test, type="class")

sub1 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predCARTF)
write.csv(sub1, "sub1.csv")
# 0.784722222222

######### randomForest
library(randomForest)
set.seed(9847)
modRF1 <- randomForest(Loan_Status ~ ., data=tr1, method="class", mtry=12, ntree=1000, importance=TRUE)
predRF1 <- predict(modRF1, newdata=te1, type="class")

# confusion matrix
table(te1$Loan_Status, predRF1)
accuracy = (28+125)/nrow(te1)
# 0.827027

set.seed(9847)
modRFF <- randomForest(Loan_Status ~ ., data=Train, method="class", mtry=12, ntree=1000, importance=TRUE)
predRFF <- predict(modRFF, newdata=Test, type="class")

sub2 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predRFF)
write.csv(sub2, "sub2.csv")
# 0.777777777778


library(gbm)
table(tr1$Loan_Status)
tr2<- tr1

tr2$Loan_Status <- ifelse(tr1$Loan_Status=="Y",1,0)

modgbm <- gbm(Loan_Status ~ ., data=tr2, distribution="bernoulli", n.trees=1000, interaction.depth=3)
predgbm <- predict(modgbm, newdata=te1, n.trees=1000, type="response")

## confusion matrix
table (te1$Loan_Status, predgbm > 0.5)

accuracy = (23+126)/nrow(te1)
# 0.8054054

Train1 <- Train
Train1$Loan_Status <- ifelse(Train$Loan_Status=="Y",1,0)
modgbmF <- gbm(Loan_Status ~ ., data=Train1, distribution="bernoulli", n.trees=1000, interaction.depth=3)
predgbmF <- predict(modgbmF, newdata=Test, n.trees=1000, type="response")
predgbmFS <- ifelse(predgbmF >0.5, "Y" , "N")

sub3 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predgbmFS)
write.csv(sub3, "sub3.csv")


### xgb
library(xgboost)
df = as.data.frame(model.matrix(~0 + Gender999+GenderFemale+GenderMale+MarriedYes+MarriedNo+Dependents999+Dependents0+Dependents1+Dependents2+Dependents3.+EducationGraduate+EducationNot.Graduate+Self_Employed999+Self_EmployedNo+Self_EmployedYes+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_AreaRural+Property_AreaSemiurban+Property_AreaUrban+LoanAmountMiss+Loan_Amount_TermMiss+Credit_HistoryMiss, tr1))
dfT= as.data.frame(model.matrix(~0 + Gender999+GenderFemale+GenderMale+MarriedYes+MarriedNo+Dependents999+Dependents0+Dependents1+Dependents2+Dependents3.+EducationGraduate+EducationNot.Graduate+Self_Employed999+Self_EmployedNo+Self_EmployedYes+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_AreaRural+Property_AreaSemiurban+Property_AreaUrban+LoanAmountMiss+Loan_Amount_TermMiss+Credit_HistoryMiss, te1))

lab <- as.numeric(tr1$Loan_Status)-1


xgb <- xgboost(data = as.matrix(df),
                label = lab,
                nrounds = 1930, max_depth = 2,eta = 0.01,
                objective = "binary:logistic", verbose=1)

m = xgb.importance(feature_names = colnames(df),model = xgb)
xgb.plot.importance(m)

predxgb <- predict(xgb, newdata=as.matrix(dfT))

table(te1$Loan_Status,predxgb > 0.5)


Accuracy = (124+22)/nrow(te1)

# 0.7837838


df1 = as.data.frame(model.matrix(~0 + Gender999+GenderFemale+GenderMale+MarriedYes+MarriedNo+Dependents999+Dependents0+Dependents1+Dependents2+Dependents3.+EducationGraduate+EducationNot.Graduate+Self_Employed999+Self_EmployedNo+Self_EmployedYes+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_AreaRural+Property_AreaSemiurban+Property_AreaUrban+LoanAmountMiss+Loan_Amount_TermMiss+Credit_HistoryMiss, Train))
dfT1= as.data.frame(model.matrix(~0 + Gender999+GenderFemale+GenderMale+MarriedYes+MarriedNo+Dependents999+Dependents0+Dependents1+Dependents2+Dependents3.+EducationGraduate+EducationNot.Graduate+Self_Employed999+Self_EmployedNo+Self_EmployedYes+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_AreaRural+Property_AreaSemiurban+Property_AreaUrban+LoanAmountMiss+Loan_Amount_TermMiss+Credit_HistoryMiss, Test))

lab1 <- as.numeric(Train$Loan_Status)-1


xgbF <- xgboost(data = as.matrix(df1),
               label = lab1,
               nrounds = 1930, max_depth = 2,eta = 0.01,
               objective = "binary:logistic", verbose=1)

m = xgb.importance(feature_names = colnames(df),model = xgb)
xgb.plot.importance(m)

predxgb <- predict(xgbF, newdata=as.matrix(dfT1))
predxgbS <- ifelse(predxgb >0.5, "Y" , "N")

sub4 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predxgbS)
write.csv(sub4, "sub4.csv")































































































































