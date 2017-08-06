rm(list = ls())
library('ggplot2')
library('lattice')
library('data.table')
library('plyr')
library('randomForest')
library('e1071')
library('caret')

coverData=read.csv('D:\\DataScience_Drive\\Forest Type\\train.csv')
#Structure of Data
str(coverData)

for(i in 12:56){
  if(class(coverData[,i]) %in% c('numeric','integer')){
    coverData[,i]=as.factor(coverData[,i])
  }
}
#Using Data.Table(which is fast and efficient. And also it is a inherit property of Data Frame)
coverDT=as.data.table(coverData)
class(coverDT)
#The wilderness areas are-
#1.Rawah Wilderness Area
#2.Neota Wilderness Area
#3.Comanche Peak Wilderness Area
#4.Cache la Poudre Wilderness Area


#Funtion to club/regroup the data in one column
reGroup = function(oldColumns, newLabels, columnName){
  for(i in 1:length(newLabels)) {
    coverDT=coverDT[get(oldColumns[i])==1,paste(columnName):=newLabels[i]]
  }
}

# Dummy old columns and new labels of Wilderness Area
newLabels = c("Rawah","Neota","Comanche Peak","Cache la Poudre")
oldColumns = c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")
columnName = "Wilderness_Area"

# Regrouping Wilderness Area
reGroup(oldColumns, newLabels, columnName)

newLabels=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40')
oldColumns = c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
                "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
                "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
                "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
                "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
columnName = "Soil_Type"

# Regrouping Soil Types under one column
reGroup(oldColumns, newLabels, columnName)


# Removing the dummy columns
coverDT = coverDT[,colnames(coverDT[,12:55,with=F]):=NULL]

#Cover_Type Names:
#Spruce/Fir
#Lodgepole Pine
#Ponderosa Pine
#Cottonwood/Willow
#Aspen
#Douglas-fir
#Krummholz"
coverDT$Cover_type=mapvalues(coverDT$Cover_Type, from = c(1:7), to = c('Spruce/Fir','Lodgepole Pine','Ponderosa Pine','Cottonwood/Willow','Aspen','Douglas-fir','Krummholz'))

coverData=as.data.frame(coverDT)
rm(coverDT)

par(mfrow=c(3,4))
for(i in 2:11){
  hist(coverData[,i], xlab = '', col = "steelblue", main = names(coverData[i]))
}
names(coverData)
head(coverData)

# Checking cover type distribution
table(coverData$Cover_Type)

# Plotting Cover Type distribution
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(coverData, aes(Cover_Type, fill=as.factor(Cover_Type))) +
  geom_bar() +
  labs(title="Cover Type Distribution", x="Cover Type", y="Count") +
  scale_fill_discrete(name="Cover Type")

# Boxplot of Elevation by Cover Type
ggplot(coverData, aes(x=Cover_Type, y=Elevation, fill = as.factor(Cover_Type))) +
  geom_boxplot() +
  labs(title="Elevation by Cover Type", x="Cover Type", y="Elevation") +
  scale_fill_discrete(name = "Cover Type")

# Density plot of elevation by Cover Type
ggplot(coverData, aes(Elevation, fill=as.factor(Cover_Type))) +
  geom_density(alpha=0.4) +
  labs(title="Elevation Density by Cover Type", x="", y="") +
  scale_fill_discrete(name="Cover Type")

# Wilderness Area by Cover Type
ggplot(coverData, aes(Wilderness_Area, fill=as.factor(Cover_Type))) +
  geom_bar(position = "dodge") +
  labs(title="Wilderness Area by Cover Type", x="Wilderness Area", y="Count") +
  scale_fill_discrete(name="Cover Type")

# Soil Types by Cover Type
ggplot(coverData, aes(Soil_Type, fill=as.factor(Cover_Type))) +
  geom_bar(position = "dodge") +
  labs(title="Soil Type by Cover Type", x="Soil Type", y="Count") +
  scale_fill_discrete(name="Cover Type")

# Removing data frame from memory
rm(coverData)

# Read training data
coverDataTrain = data.table::fread("../input/train.csv",header=T)

  # Read test data
  coverDataTest  = data.table::fread("../input/test.csv",header=T)

  # Summary of train data
  summary(coverDataTrain)
  
#Soil_Type7 and Soil_Type15 have zero variance. So, lets not consider them for model building exercise.
  # Removing Soil_Type7 and Soil_Type15
  coverDataTrain = coverDataTrain[,Soil_Type7:=NULL]
  coverDataTrain = coverDataTrain[,Soil_Type15:=NULL]
  
  # Converting categorical to factors
  coverDataTrain[,12:54] = lapply(coverDataTrain[,12:54], as.factor)
  coverDataTest[,12:55] = lapply(coverDataTest[,12:55], as.factor)
  
  # Let create samples of devlopment and validation set from the training data
  set.seed(123)
  sample = sample(2, nrow(coverDataTrain), replace = T, prob = c(0.8,0.2))
  coverDataDev = coverDataTrain[sample==1,]
  coverDataVal = coverDataTrain[sample==2,]
  
  # Checking balance of distribution between Development and Validation set
  table(coverDataDev$Cover_Type)/nrow(coverDataDev)
  table(coverDataVal$Cover_Type)/nrow(coverDataVal)
  
  # Creating a Random Forest model for Cover Type classification
  mod = randomForest(Cover_Type ~ .-Id, data=coverDataDev, mtry=sqrt(ncol(coverDataDev)), ntree = 300, importance =T, do.trace=25)

#Printing RF model info
print(mod)

# Feature importance
importance(mod, type = 2)

# Feature importance plot
par(mfrow=c(1,1))
varImpPlot(mod, type=1, main="Feature Importance", col="steelblue", pch=20)

varImpPlot(mod, type=2, main="Feature Importance", col="steelblue", pch=20)

# Plot the RF model
plot(mod)


# Predicting Cover Type on validation set
coverDataVal$predictedCoverType = predict(mod,coverDataVal)

  # Creating Confusion Matrix
  confusionMatrix(data=coverDataVal$predictedCoverType,
                  reference=coverDataVal$Cover_Type,
                  positive='yes')
  
  # Prediciton on test data set
  coverDataTest$CoverType = predict(mod,coverDataTest)
  
  # Creating submission file for upload
  submission = as.data.frame(cbind(Id = coverDataTest$Id, Cover_Type = coverDataTest$CoverType))

  write.csv(submission, "output.csv", row.names=FALSE)
