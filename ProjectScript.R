library(arules)
library(arulesViz)
library(kernlab)
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
Cancellations=read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv");
view(Cancellations)
dim(Cancellations)
str(Cancellations)
summary(Cancellations$LeadTime)
table(Cancellations$LeadTime) 
table(Cancellations$Meal)

table(Cancellations$Country) %>% as.data.frame() %>% arrange(desc(Freq))

table(Cancellations$CustomerType)
table(Cancellations$LeadTime)

autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t")

# Concatenate the three vectors
leadtimep <- c(Cancellations$LeadTime)

# Compute the largest y value used in the autos
max_num <- max(leadtimep)
max_num

hist(leadtimep, color='blue', breaks=70, 
     xlim=c(0,500), right=F, main="Lead Time Histogram", las=1)
h <- hist(leadtimep, breaks = 10, xaxt = 'n')
hist(axis(1, h$mids, paste(h$breaks[1:3], h$breaks[2:4], sep=' - ')))

dfCountryCanceled <- Cancellations %>% group_by(Country) %>% 
  filter(IsCanceled == 1) %>%summarize(IsCanceled = n()) %>%
  arrange(desc(IsCanceled))

##Map data
dfCountryCanceled <- subset(dfCountryCanceled, Country != 'GIB' )
dfCountryCanceled <- subset(dfCountryCanceled, Country != 'NULL' )
dfCountryCanceled$Country[dfCountryCanceled$Country == "CN"] <- "CHN"
# sum(table(dfCountryCanceled$Country))
# table(dfCountryCanceled$Country)
# 70 out of 72 were corrected hence plotting the map
# taking only values greater than 10 cancellations
dfCountryCanceled <- dfCountryCanceled %>% filter(IsCanceled > 10)
#tail(dfCountryCanceled)
par(mar=c(2,2,2,2))
worldmap <- joinCountryData2Map(dfCountryCanceled, joinCode="ISO3",
                                nameJoinColumn="Country",)
map<-mapCountryData(worldmap, nameColumnToPlot='IsCanceled',
                    catMethod="logFixedWidth",
                    colourPalette=c('white',
                                    'lightblue','purple','blue','grey','black','red'))
table(hotel_booking$MarketSegment)
table(hotel_booking$Country) %>% as.data.frame() %>% arrange(desc(Freq))

sapply(Cancellations,function(x) sum(is.na(x)))
sapply(Cancellations,function(x) sum(is.null(x)))
cancellations=as.data.frame(Cancellations)


#Percentage of cancelled bookings 
table(df$IsCanceled)/nrow(df)
cancelled <- c("is_cancelled","not_cancelled")
percent <- c(72.24,27.76)
cancel_percent <- data.frame(cancelled, percent)
cancel_percent$cancelled <- factor(cancel_percent$cancelled, levels=cancel_percent$cancelled) 
mylabel<-paste(cancel_percent[,2],"%")  
library("ggplot2")
praph1<-ggplot(data=cancel_percent,mapping=aes(x=cancelled,y=percent,fill=cancelled,group=factor(1)))+
  geom_bar(stat="identity")+geom_text(aes(y= 79, x= 1),label="72.24%",size=5)+
  geom_text(aes(y= 35, x= 2),label="27.76%",size=5)+
  labs(x="Cancelled or Not",y="Percentage",title='Percentage of the Cancellation')+
  ylim(0,100)
praph1


#Average lead time for cancelled and not cancelled 
library("RSQLite")
library("sqldf")
a = 'select avg(LeadTime) from df where iscanceled=1'   
cancel_cnt <- sqldf(a)  
print(cancel_cnt)

b = 'select avg(LeadTime) from df where iscanceled=0'   
cancel_cnt <- sqldf(b)  
print(cancel_cnt)

cancelled <- c("is_cancelled","not_cancelled")
percent <- c(128.68,78.83)
cancel_LeadTime <- data.frame(cancelled, percent)
cancel_LeadTime$cancelled <- factor(cancel_LeadTime$cancelled, levels=cancel_LeadTime$cancelled) 
mylabel<-paste(cancel_LeadTime[,2],"%")  
graph2<-ggplot(data=cancel_LeadTime,mapping=aes(x=cancelled,y=percent,fill=cancelled,group=factor(1)))+
  geom_bar(stat="identity")+geom_text(aes(y= 138, x= 1),label="128.68",size=5)+
  geom_text(aes(y= 88, x= 2),label="78.83",size=5)+
  labs(x="Cancelled or Not",y="Lead Time",title='Average lead time for cancelled and not cancelled')+
  ylim(0,150)

graph2

#percentage of special request for cancelled and not cancelled

cancel_SpecialRequest <- data.frame(table(df$TotalOfSpecialRequests,df$IsCanceled))
cancel_SpecialRequest$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph3<-ggplot(cancel_SpecialRequest,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Special Requests with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.75, x= 2),label="52.34%",size=5)+
  geom_text(aes(y= 0.33, x= 2),label="31.82%",size=5)+
  geom_text(aes(y= 0.10, x= 2),label="12.79%",size=5)+
  geom_text(aes(y= 0.65, x= 1),label="64.88%",size=5)+
  geom_text(aes(y= 0.25, x= 1),label="23.35%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="10.13%",size=5)
graph3 

#Percentage booking for deposit type for cancellation and no cancellation 
cancel_DepositType <- data.frame(table(df$DepositType,df$IsCanceled))
cancel_DepositType
cancel_DepositType$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph4<-ggplot(cancel_DepositType,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Deposit Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.51, x= 2),label="99.35%",size=5)+
  geom_text(aes(y= 0.65, x= 1),label="84.97%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="14.84%",size=5)
graph4 

#Percentage booking doe type of customer segmentation cancellation and no cancellation
cancel_CustomerType <- data.frame(table(df$CustomerType,df$IsCanceled))
cancel_CustomerType
cancel_CustomerType$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph4<-ggplot(cancel_CustomerType,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Customer Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.975, x= 2),label="5.60%",size=5)+
  geom_text(aes(y= 0.60, x= 2),label="71.85%",size=5)+
  geom_text(aes(y= 0.12, x= 2),label="21.67%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="13.66%",size=5)+
  geom_text(aes(y= 0.60, x= 1),label="84.66%",size=5)
graph4 
#Country specific cancellation
cancel_Country <- data.frame(table(df$Country,df$IsCanceled))
cancel_Country
cancel_Country$Is_Canceled <- c(1)
b = 'update cancel_Country set Is_Canceled="Is_Canceled"  where Var2=1 ' 
sqldf(c("
       UPDATE cancel_Country
       set Is_Canceled='s_Canceled'
       where Var2=1
       ", "select * from main.cancel_Country"))
cancel_cnt <- sqldf(b)  
print(cancel_cnt)
graph5<-ggplot(cancel_Country,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Customer Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.975, x= 2),label="5.60%",size=5)+
  geom_text(aes(y= 0.60, x= 2),label="71.85%",size=5)+
  geom_text(aes(y= 0.12, x= 2),label="21.67%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="13.66%",size=5)+
  geom_text(aes(y= 0.60, x= 1),label="84.66%",size=5)
graph5 

#percentage of parking space for cancelled and not cancelled
library(ggplot2)
cancel_ParkingSpaces <- data.frame(table(df$RequiredCarParkingSpaces,df$IsCanceled))
cancel_ParkingSpaces
cancel_ParkingSpaces$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph6<-ggplot(cancel_ParkingSpaces,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Parking Spaces with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.55, x= 2),label="81.03%",size=5)+
  geom_text(aes(y= 0.09, x= 2),label="18.87%",size=5)+
  geom_text(aes(y= 0.50, x= 1),label="100%",size=5)
graph6 


cancellations=as.data.frame(Cancellations)
cancellations$IsCanceled=as.factor(cancellations$IsCanceled)
cancellations$Meal=as.factor(cancellations$Meal)
cancellations$Country=as.factor(cancellations$Country)
cancellations$MarketSegment=as.factor(cancellations$MarketSegment)
cancellations$ReservedRoomType=as.factor(cancellations$ReservedRoomType)
cancellations$AssignedRoomType=as.factor(cancellations$AssignedRoomType)
cancellations$DepositType=as.factor(cancellations$DepositType)
cancellations$CustomerType=as.factor(cancellations$CustomerType)

length(sort(unique(c(which(cancellations$Meal=="Undefined"),which(cancellations$Country=="NULL")))))
length(sort(unique(c(which(cancellations$Meal=="Undefined"),which(cancellations$Country=="NULL")))))/nrow(cancellations)


##Regression tre for entire data
set.seed(1)
trainList=createDataPartition(y=cancellations$IsCanceled,p=2/3,list=FALSE)
trainSet=cancellations[trainList,]
testSet=cancellations[-trainList,]



#Regression Tree (all of the variables) to diagnose the important variables.
cartTree=rpart(IsCanceled~.,data=trainSet)
cartTree
prp(cartTree,faclen=0,cex=0.8,extra=1)

varImp(cartTree)

CartPrediction=predict(cartTree,newdata=testSet)
CartPrediction[CartPrediction>0.5]=1
CartPrediction[CartPrediction!=1]=0
confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)

Accuracy=confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)$overall[1]

CIV=cancellations[,c(1,2,19,10,12,9,4)] #"CIV" stands for CancellationsImportantVariables.

#Linear Model
CIV[,1]=(as.numeric(CIV[,1]))-1

LinearModel=lm(IsCanceled~.,data=CIV)
summary(LinearModel)
LinearModelPrediction=predict(LinearModel,CIV[,-1])
LinearModelPrediction[LinearModelPrediction>0.5]=1
LinearModelPrediction[LinearModelPrediction!=1]=0
confusionMatrix(as.factor(LinearModelPrediction),as.factor(CIV[,1]))

LinearModelAccuracy=confusionMatrix(as.factor(LinearModelPrediction),as.factor(CIV[,1]))$overall[1]

#Regression Trees (important variables only).
#Pre-processing the data so the training doesn't have to do it on the SVM and to have data uniformity across all models.
#The pre-processed data was not used in Linear Regression because it makes the interpretation more complicated with the same results.
CIV2=CIV
CIV2$LeadTime=as.numeric(scale(CIV2$LeadTime))
CIV2$RequiredCarParkingSpaces=as.numeric(scale(CIV2$RequiredCarParkingSpaces))
CIV2$PreviousCancellations=as.numeric(scale(CIV2$PreviousCancellations))
CIV2$StaysInWeekNights=as.numeric(scale(CIV2$StaysInWeekNights))

CIV[,1]=as.factor(CIV[,1]) #Important for the rest
CIV2[,1]=as.factor(CIV2[,1]) #Important for the rest of the models.

library(caret)

set.seed(1)
trainList=createDataPartition(y=CIV2$IsCanceled,p=2/3,list=FALSE)
trainSet=CIV2[trainList,]
testSet=CIV2[-trainList,]

cartTree=rpart(IsCanceled~.,data=trainSet)
cartTree
prp(cartTree,faclen=0,cex=0.8,extra=1)

varImp(cartTree)

CartPrediction=predict(cartTree,newdata=testSet)
CartPrediction[CartPrediction>0.5]=1
CartPrediction[CartPrediction!=1]=0
confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)

Accuracy1=confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)$overall[1]

c(Accuracy,Accuracy1) #19 predictors (original) vs 6 predictors (important variables).

#Treebag:
library(caret)

set.seed(1)
trainList=createDataPartition(y=CIV2$IsCanceled,p=2/3,list=FALSE)
trainSet=CIV2[trainList,]
testSet=CIV2[-trainList,]

TreebagFit=train(IsCanceled~.,data=trainSet,method="treebag",preProcess=NULL)

varImp(TreebagFit)

TreebagPrediction=predict(TreebagFit,newdata=testSet)
confusionMatrix(as.factor(TreebagPrediction),testSet$IsCanceled)

TreebagAccuracy=confusionMatrix(as.factor(TreebagPrediction),testSet$IsCanceled)$overall[1]

TreebagAccuracy

#Association Rules:
CIVAR=CIV[,c(1,4,6)] #The data that will be used for the Association Rules.

Support=0.012 #Real, rounded value for the support, not made up. 
Confidence=0.172 #Real, rounded value for the confidence, not made up.

#Stands for Cancellations Important Variables Association Rules.
library(arules)
library(arulesViz)
Transaction=as(CIVAR,"transactions")
itemFrequencyPlot(Transaction,topN=10)
crossTable(Transaction,sort=TRUE)[1:5,1:5]
ruleset=apriori(Transaction,parameter=list(supp=0.012,conf=0.172),control=list(verbose=FALSE),appearance=list(default="lhs",rhs="IsCanceled=1"))
inspect(ruleset)
AcceptableLiftRuleset=ruleset[quality(ruleset)$lift>1]
inspect(AcceptableLiftRuleset)
plot(ruleset)

inspect(ruleset[c(7,11)]) #Rules which are the most likely to be true.

#"The rules...with an empty LHS mean that no matter what other items are involved the item in the RHS will appear with the probability given by the rule’s confidence (which equals the support).
#Source: https://www.kirenz.com/post/2020-05-14-r-association-rule-mining/

#The Association Rules for the data:

#There's 27.763355% that a customer will cancel the a hotel booking.

#There's a 5.434348% chance that a customer has "Groups" as their Market Segment, "PRT" as their country, and cancelled their hotel booking. If a customer has "Groups" as its Market Segment and "PRT" as their country, there's a 75.45927% chance the customer will cancel their hotel booking.

#There's a 7.798303% chance that a customer has "Online TA" as their Market Segment, "PRT" as their country, and cancelled their hotel booking. If a customer has "Online TA" as its Market Segment and "PRT" as their country, there's a 48.15785% chance the customer will cancel their hotel booking.

#Lab's version of SVM.
library(caret)

set.seed(1)
trainList=createDataPartition(y=CIV2$IsCanceled,p=2/3,list=FALSE)
trainSet=CIV2[trainList,]
testSet=CIV2[-trainList,]

library(kernlab)
SVM=ksvm(IsCanceled~.,data=trainSet,C=5,cross=3,prob.model=TRUE,scaled=FALSE)
SVM
Prediction=predict(SVM,newdata=testSet,type="response")
confusionMatrix(Prediction,testSet$IsCanceled)

AccuracySVM=confusionMatrix(Prediction,testSet$IsCanceled)$overall[1]

#Overall model comparison:

ModelComparison=c(LinearModelAccuracy,Accuracy1,TreebagAccuracy,AccuracySVM)
names(ModelComparison)=c("Linear.Regression","Regression.Tree.Cart","Treebag","SVM")
ModelComparison
which.max(ModelComparison)

tabledf <- data.frame(Model = c("Linear Regression Model","CART","Bagtree", "SupportVectorMachine"),
                      Accuracy_Percent = c(81.19, 82.26, 84.16, 83.58),
                      NIR_Percent = c(72.24, 72.24, 72.24, 72.24),
                      Kappa_Percent = c(47.58,58.77,52.40,56.36))
head(tabledf)

