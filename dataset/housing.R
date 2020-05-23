library(readr)
library(ggplot2)
library(caTools)
library(caret)
library(factoextra)
library(cluster)
library(NbClust)
library(corrplot)
library(rpart)
library(klaR)
library(e1071)
housingdata <- read.csv("Melbourne_housing_FULL.csv")
View(housingdata)
str(housingdata)

#Data Cleaning

newhousingdata = data.frame(lapply(housingdata, function(x) { gsub("#N/A", NA, x) } ) )
housing <- na.omit(newhousingdata)
is.na(housing)
str(housing)

#PLotting latitudes and longitudes

housing$Longtitude = as.numeric( as.character(housing$Longtitude) )
housing$Lattitude = as.numeric( as.character(housing$Lattitude) )
housing$Price = as.numeric( as.character(housing$Price) )

ggplot(data = housing, aes(y=Lattitude, x=Longtitude)) + 
  geom_point(aes(colour=Price)) + 
  scale_colour_gradient(low = "pink", high = "blue") +
  theme_bw()




# Dropping postcode as suburbs and postcode are two dependent variables
housing$Postcode = NULL

View(housing)

#correlation between Rooms and Bedroom2
cor(as.numeric(as.character(housing$Rooms)), as.numeric(as.character(housing$Bedroom2)), use="pairwise.complete.obs")

#Dropping Bedroom2 due to high correlation

housing$Bedroom2 <- NULL

#Replacing the 0 vaues of landsize with median values
median_landsize = median(as.numeric(as.character(housing$Landsize)))
housing$Landsize[which(housing$Landsize == 0)] <- median_landsize

View(housing)

#Dropping address since it is unique to each row and does not add any value to the regression process
housing$Address <- NULL

#To find which all variables are numeric 
lapply(housing,class) 

#converting Factors into Numeric Value 

housing$Landsize=as.numeric(as.character(housing$Landsize))
housing$Distance=as.numeric(as.character(housing$Distance)) 
housing$Rooms=as.numeric(as.character(housing$Rooms)) 
housing$Bathroom=as.numeric(as.character(housing$Bathroom)) 
housing$Car=as.numeric(as.character(housing$Car)) 
housing$YearBuilt=as.numeric(as.character(housing$YearBuilt)) 
housing$BuildingArea=as.numeric(as.character(housing$BuildingArea)) 
#Plots
par(mfrow = c(2,2), mar=c(3.1,3.1,0.95,0))
hist(housing$Distance, breaks = 40, xlim = c(0,50), ylim = c(0,800),xlab = "Distance", col = "Red", main = "Histogram of Distances", las =1)
hist(housing$Rooms, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Rooms", col = "Blue", main = "Histogram of no. of rooms in the houses", las =1)
hist(housing$Bathroom, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Bathrooms", col = "Green", main = "Histogram of no. of bathrooms in houses", las =1)
hist(housing$Car, breaks = 40, xlim = c(0,10), ylim = c(0,4000),xlab = "No. of Carslots", col = "Orange", main = "Histogram of no. of carslots in houses", las =1)


#correlation

round(cor(housing[c(2,4,8,9,10,11,12,13)]),2)
correlation_housing<-round(cor(housing[,c(2,4,8,9,10,11,12,13)]),2)


#Plotting Correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation_housing, method="color", col=col(200),  
order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45) 



#####Correlation Between Price and Rooms######

round(cor(housing[,c(4,2)]),2)
corr_Price_Room<-round(cor(housing[,c(4,2)]),2)

cor(housing$Rooms,housing$Price, use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_Price_Room, method="color", col=col(200),  
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45) 




#####Correlation Between Price and Distance######

round(cor(housing[,c(4,8)]),2)
corr_Price_Distance<-round(cor(housing[,c(4,8)]),2)

cor(housing$Distance,housing$Price, use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_Price_Distance, method="color", col=col(200),  
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45) 



##Regression
#Removing building area from our regression model since rooms and building area are highly correlated
#Also removing Yearbuild
Reg_model <- lm(Price ~ Distance + Rooms + Bathroom + Car + Landsize, data = housing)
summary(Reg_model)

#Plot of the regression model

par(mfrow = c(2,2))
plot(Reg_model)

#clustering

HousingCLUS = housing

housingData1<-subset(HousingCLUS, select = c("Rooms","Price","Distance","Bathroom","Landsize","BuildingArea","YearBuilt"))

View(housingData1)
#Standardize the data
housingData2<-scale(housingData1)
View(housingData2)
#determining the number of clusters


#elbow method
set.seed(100)
wss<-(nrow(housingData2)-1)*sum(apply(housingData2,2,var)) 
for(i in 2:20) wss[i]<-sum(kmeans(housingData2,centers = i)$withinss)
plot(1:20,wss,type = "b", xlab ="number of clusters"  ,ylab ="within group sum of squares")

#Kmeans cluster analysis
Kmeans_housingData1<-kmeans(housingData2,10)
aggregate(housingData2,by=list(Kmeans_housingData1$cluster),FUN=mean) #median of each cluster
KhousingData2<-data.frame(housingData2,Kmeans_housingData1$cluster)  #attaching the cluster information into the dataset
View(KhousingData2)
table(KhousingData2$Kmeans_housingData1.cluster) #number of obserbations in each cluster

#cluster plot
plot(KhousingData2,col=Kmeans_housingData1$cluster)
points(Kmeans_housingData1$cluster,col=4:22,pch=8,cex=2)
clusplot(KhousingData2,Kmeans_housingData1$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#plotting all the attributes by reducing into two principal component
clusplot(KhousingData2,Kmeans_housingData1$cluster,color =TRUE) 

#Plot of variious attributes
par(mfrow = c(2,2))
plot(Rooms~Landsize, KhousingData2, col=Kmeans_housingData1$cluster)
plot(Rooms~Bathroom, KhousingData2, col=Kmeans_housingData1$cluster)
plot(BuildingArea~Landsize, KhousingData2, col=Kmeans_housingData1$cluster)


#Naive Bayes Classification

#classification

write.csv(housing, "housing2.csv")
housing_classification <- read.csv("C:\\Users\\kalya\\Desktop\\housing2.csv")
View(housing_classification)

housing_classification$Class[housing_classification$Price < 500000] <- "C1"
housing_classification$Class[housing_classification$Price >= 500000 & housing_classification$Price<1000000] <- "C2"
housing_classification$Class[housing_classification$Price >= 1000000 & housing_classification$Price<1500000] <- "C3"
housing_classification$Class[housing_classification$Price >= 1500000 & housing_classification$Price<2000000] <- "C4"
housing_classification$Class[housing_classification$Price >= 2000000 & housing_classification$Price<2500000] <- "C5"
housing_classification$Class[housing_classification$Price >= 2500000 & housing_classification$Price<3000000] <- "C6"
housing_classification$Class[housing_classification$Price >= 3000000 & housing_classification$Price<3500000] <- "C7"
housing_classification$Class[housing_classification$Price >= 3500000 ] <- "C8"
View(housing_classification)



housing_classification$Suburb <- NULL
housing_classification$SellerG <- NULL
housing_classification$Method <- NULL
housing_classification$Regionname <- NULL
housing_classification$Propertycount <- NULL
housing_classification$CouncilArea <- NULL
housing_classification$Date <- NULL
housing_classification$Type <- NULL
housing_classification$X <- NULL
View(housing_classification)



set.seed(100)
modelnaive <- naiveBayes(as.factor(Class) ~ ., data = housing_classification)
modelnaive
prednaive <- predict(modelnaive, housing_classification, type = "class")
table(prednaive,housing_classification$Class)
#str(housing_classification)
new <- data.frame(Rooms = 5, Price = 6000000, Distance = 7.5, Bathroom = 3, Car = 0, Landsize = 300, BuildingArea = 150, YearBuilt = 2000, Lattitude = -37.8, Longtitude = 145)
z <- predict(modelnaive,new )
z

new1 <- data.frame(Rooms = 2, Price = 3000000, Distance = 9, Bathroom = 3, Car = 0, Landsize = 300, BuildingArea = 150, YearBuilt = 2000, Lattitude = -37.8, Longtitude = 145)
z <- predict(modelnaive,new1 )
z


summary(modelnaive)



# plotting the Decision tree
CT.Plot <- rpart(Class~.,data = housing_classification, method = "class")
plot(CT.Plot)
text(CT.Plot, pretty = 0)



#SVM LINEAR MODEL
library(kernlab)
SVMFit <- svm(as.factor(Class)~.,data=housing_classification, method="svmLinear",trControl = BCtrlL,preProcess = c("center","scale"))
SVMFit
SVMTest <- predict(SVMFit, housing_classification, type = "class")
table(SVMTest, housing_classification$Class)
set.seed(100)
SVMFit2 <- svm(as.factor(Class)~.,data=housing_classification, method="svmPoly",trControl = BCtrlL,preProcess = c("center","scale"))
SVMFit2
SVMTest2 <- predict(SVMFit2, housing_classification, type = "class")
table(SVMTest2, housing_classification$Class)
set.seed(100)
SVMFit3 <- svm(as.factor(Class)~., data = housing_classification, method = "svmRadial", trControl = BCtrlL, preProcess = c("center","scale"))
SVMFit3
SVMTest3 <- predict(SVMFit3, housing_classification, type = "class")
table(SVMTest3, housing_classification$Class)



# Summarizing all the methods based on their accuracy
confusionMatrix(SVMTest,as.factor(housing_classification$Class))
confusionMatrix(SVMTest2,as.factor(housing_classification$Class))
confusionMatrix(SVMTest3,as.factor(housing_classification$Class))
confusionMatrix(prednaive,as.factor(housing_classification$Class))
