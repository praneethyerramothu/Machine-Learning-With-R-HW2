irismain<-read.csv("/Users/praneeththomas/Downloads/School Stuff/R/iris-species/Iris.csv")
library(class)

#immputation Method:Mean starts

iris<-irismain
drop<-c("Id","Species")
irisnew<-iris[,!(names(iris) %in% drop)]
naelem<-function(x){
  (nrow(iris)*(ncol(iris)-2)*x)/100 #we are calculating (columns-2) because we are not considering the two columns.
}
inds<-as.matrix(expand.grid(1:nrow(irisnew),1:ncol(irisnew))) # convert to matrix to impute values
inds<-matrix(inds[!is.na(irisnew[inds])],ncol=2)
selected<-inds[sample(nrow(inds),naelem(25)),]
irisnew[selected]<-NA
irisnew # has NA values imputed
irisold<-iris[,!(names(iris) %in% drop)]
meanval<-mean(irisnew$SepalLengthCm,na.rm=TRUE)
meanval1<-mean(irisnew$SepalWidthCm,na.rm=TRUE)
meanval2<-mean(irisnew$PetalLengthCm,na.rm=TRUE)
meanval3<-mean(irisnew$PetalWidthCm,na.rm=TRUE)
irisnew$SepalLengthCm[is.na(irisnew$SepalLengthCm)]<-meanval
irisnew$SepalWidthCm[is.na(irisnew$SepalWidthCm)]<-meanval1
irisnew$PetalLengthCm[is.na(irisnew$PetalLengthCm)]<-meanval2
irisnew$PetalWidthCm[is.na(irisnew$PetalWidthCm)]<-meanval3
irisnew


# Imputation method:Mean ends



#RMSE code starts

irisold<-as.matrix(irisold)
irisnew<-as.matrix(iris_amelia_new) #differs with the type of method that we are using
error<-irisnew-irisold    
rmse<-function(error){
  sqrt(mean(error^2))
}
rmse(error)
irisnew

# RMSE code ends

#K-NN code starts

irisnew12<-as.data.frame((irisnew))
iris<-iris[,-1]
table(iris$Species)
iris$Species <- factor(iris$Species, levels = c("Iris-setosa", "Iris-versicolor","Iris-virginica"), labels = c("Iris-setosa", "Iris-versicolor","Iris-virginica"))
round(prop.table(table(iris$Species)) * 100, digits = 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
iris_new_train <- as.data.frame(lapply(iris[1:4], normalize))
iris_new_test <- as.data.frame(lapply(irisnew12, normalize))
iris_train<-iris_new_train
iris_test<-iris_new_test
#iris_train_target<-iris$Species 
#iris_test_target<-iris$Species
iris_test_pred <- knn(train = iris_train, test = iris_test,cl =iris$Species, k=30)
iris_test_pred 

round(prop.table(table(iris_test_pred )) * 100, digits = 1)

#K-NN code ends




# Imputation method:MICE starts


install.packages("missForest")
install.packages("mice")
#install.packages("VIM")
#library(VIM)
library(mice)
library(missForest)
irismice<-irismain
iris.mis <- prodNA(irismice[,2:5], noNA = 0.15) # to impute 25% of the data
iris.mis
summary(iris.mis)
md.pattern(iris.mis)
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

imputed_Data$imp$SepalWidthCm
iris_mice <- complete(imputed_Data,1)
iris_mice

# Imputation method:MICE ends


#Imputation method: Amelia starts

install.packages("Amelia")
library(Amelia)
iris_amelia<-irismain[,-1]
iris_amelia_mis<-prodNA(iris_amelia, noNA=0.15)
amelia_fit<-amelia(iris_amelia_mis,m=5,parallel="multicore",noms="Species")
iris_amelia_new<-amelia_fit$imputations[[1]]
amelia_fit$imputations[[2]]
amelia_fit$imputations[[3]]
amelia_fit$imputations[[4]]
amelia_fit$imputations[[5]]
iris_amelia_new<-iris_amelia_new[,-5]

#Imputation method: Amelia ends

#RMSE data frame for 3 imputation methods and 6 different percentages

RMSEdata<-matrix(c(0.08,0.191,0.04,0.10,0.18,0.07,0.14,0.30,0.12,0.21,0.44,0.14,0.23,0.47,0.19,0.32,0.521,0.23),ncol=3)
colnames(RMSEdata)<-c("MICE","Mean","Amelia")
dataofRMSE <- data.matrix(RMSEdata, rownames.force = NA)
barplot(dataofRMSE, main=" RMSE Comparision", ylab="RMSE", col=c("black","red", "green","white","yellow","grey"))

# Missing values not Randomly generated code begins(Use mean)

iris_mnar<-irismain[,2:5]#Keeping just the columns with numerical values and excluding id column
tempval<-ncol(iris_mnar)
numvalmis<-naelem(2)
mnar<-ceiling(numvalmis/tempval) #calculating 'mnar' to equally distribute the missing values to every column
iris_mnar_new<-as.matrix(expand.grid(1:nrow(iris_mnar),1:ncol(iris_mnar))) 
iris_mnar_new<-matrix(iris_mnar_new[!is.na(iris_mnar[iris_mnar_new])],ncol=2)
selected<-inds[sample(nrow(iris_mnar_new),mnar),] # Generating missing values to impute in the data
iris_mnar[selected]<-NA
iris_mnar # has NA values imputed
#Using mean method to impute values at the missing values
meanval_mnar<-mean(iris_mnar$SepalLengthCm,na.rm=TRUE)
meanval1_mnar<-mean(iris_mnar$SepalWidthCm,na.rm=TRUE)
meanval2_mnar<-mean(iris_mnar$PetalLengthCm,na.rm=TRUE)
meanval3_mnar<-mean(iris_mnar$PetalWidthCm,na.rm=TRUE)
#substituting missing values in each column
iris_mnar$SepalLengthCm[is.na(iris_mnar$SepalLengthCm)]<-meanval_mnar
iris_mnar$SepalWidthCm[is.na(iris_mnar$SepalWidthCm)]<-meanval1_mnar
iris_mnar$PetalLengthCm[is.na(iris_mnar$PetalLengthCm)]<-meanval2_mnar
iris_mnar$PetalWidthCm[is.na(iris_mnar$PetalWidthCm)]<-meanval3_mnar
iris_mnar #calculate error using RMSE


## Missing values not Randomly generated code ends






