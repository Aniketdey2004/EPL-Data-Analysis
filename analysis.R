library(dplyr)
library(ggplot2)

mydata<-read.csv("England_csv.csv")
head(mydata,10)
#getting a table view of the dataset
View(mydata)

#checking the dimesions of our data
dim(mydata)

#checking the datatypes of the variables
str(mydata)

colnames(mydata)

#checking the statistical summary of each column
summary(mydata)



mydata$Date<-as.Date(mydata$Date,format="%d/%m/%Y")

str(mydata)

#checking the na values in columns
colSums(is.na(mydata))


#filling na values of integer with 0 and chr with D
mydata$HTH.Goals[is.na(mydata$HTH.Goals)]<-0
mydata$HTA.Goals[is.na(mydata$HTA.Goals)]<-0
mydata$H.Shots[is.na(mydata$H.Shots)]<-0
mydata$A.Shots[is.na(mydata$A.Shots)]<-0
mydata$H.Shots[is.na(mydata$H.Shots)]<-0
mydata$H.SOT[is.na(mydata$H.SOT)]<-0
mydata$A.SOT[is.na(mydata$A.SOT)]<-0
mydata$H.Fouls[is.na(mydata$H.Fouls)]<-0
mydata$A.Fouls[is.na(mydata$A.Fouls)]<-0
mydata$H.Yellow[is.na(mydata$H.Yellow)]<-0
mydata$A.Yellow[is.na(mydata$A.Yellow)]<-0
mydata$H.Red[is.na(mydata$H.Red)]<-0
mydata$A.Red[is.na(mydata$A.Red)]<-0
mydata$H.Corners[is.na(mydata$H.Corners)]<-0
mydata$A.Corners[is.na(mydata$A.Corner)]<-0


colSums(is.na(mydata))

#which team played the most home matches 

