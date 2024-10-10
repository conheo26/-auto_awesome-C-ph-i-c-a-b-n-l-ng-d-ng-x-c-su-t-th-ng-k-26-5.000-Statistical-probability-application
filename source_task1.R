library(tidyverse)
library(nortest)
library(car)
library(pgirmess)
#remove workspace and plot before running the code
rm(list = ls())
if(!is.null(dev.list())) dev.off()

load("flights.rda")
dataset <- flights[,c("year", "month", "day", "carrier", "origin", "dest", "dep_time","dep_delay", "arr_time", "arr_delay", "distance")]
head(dataset,3)

apply(is.na(dataset),2,sum)
apply(is.na(dataset),2,mean)

dataset <- na.omit(dataset)

#So quan sat tu du lieu goc
nrow(flights)
#So quan sat sau khi lam sach
nrow(dataset)
#Thong ke so quan sat da xoa
nrow(flights)-nrow(dataset)
#Thong ke ty le quan sat
(nrow(flights)-nrow(dataset))/nrow(flights)

dataset$dep_time <- sapply(dataset$dep_time, function(x) {
  sprintf("%02d:%02d", floor(x / 100), x %% 100)
})
dataset$dep_time <- as.POSIXct(dataset$dep_time, format = "%H:%M", tz = "UTC")
dataset$dep_time <- format(dataset$dep_time, "%H:%M")

#Thong ke mo ta cho bien dep_delay theo carrier
length = tapply(dataset$dep_delay, dataset$carrier,length)
mean = tapply(dataset$dep_delay, dataset$carrier,mean)
sd = tapply(dataset$dep_delay, dataset$carrier,sd)
min = tapply(dataset$dep_delay, dataset$carrier,min)
max = tapply(dataset$dep_delay, dataset$carrier,max)
Q1 = tapply(dataset$dep_delay, dataset$carrier,quantile,probs = 0.25)
Q2 = tapply(dataset$dep_delay, dataset$carrier,quantile,probs = 0.5)
Q3 = tapply(dataset$dep_delay, dataset$carrier,quantile,probs = 0.75)
t(data.frame(length,mean,sd,min,max,Q1,Q2,Q3))

#Thong ke mo ta cho bien arr_delay theo carrier
length = tapply(dataset$arr_delay, dataset$carrier,length)
mean = tapply(dataset$arr_delay, dataset$carrier,mean)
sd = tapply(dataset$arr_delay, dataset$carrier,sd)
min = tapply(dataset$arr_delay, dataset$carrier,min)
max = tapply(dataset$arr_delay, dataset$carrier,max)
Q1 = tapply(dataset$arr_delay, dataset$carrier,quantile,probs = 0.25)
Q2 = tapply(dataset$arr_delay, dataset$carrier,quantile,probs = 0.5)
Q3 = tapply(dataset$arr_delay, dataset$carrier,quantile,probs = 0.75)
t(data.frame(length,mean,sd,min,max,Q1,Q2,Q3))

#ve boxplot the hien phan phoi cua bien arr_delay va dep_delay theo carrier
boxplot(arr_delay~carrier, xlab = "carrier", ylab = "arr_delay", main = "Boxplot of arr_delay for a category of Carrier", data = dataset, col = 2:7) 
boxplot(dep_delay~carrier, xlab = "carrier", ylab = "dep_delay", main = "Boxplot of dep_delay for a category of Carrier", data = dataset, col = 2:7) 

#ham chuyen cac outlier thanh NA
rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs = c(.25,.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#chuyen cac outliers thanh NA cho tung carrier
AA = subset(dataset,dataset$carrier =="AA")
AA$dep_delay = rm.out(AA$dep_delay)
AA$arr_delay = rm.out(AA$arr_delay)
AS = subset(dataset,dataset$carrier =="AS")
AS$dep_delay = rm.out(AS$dep_delay)
AS$arr_delay = rm.out(AS$arr_delay)
B6 = subset(dataset,dataset$carrier =="B6")
B6$dep_delay = rm.out(B6$dep_delay)
B6$arr_delay = rm.out(B6$arr_delay)
DL = subset(dataset,dataset$carrier =="DL")
DL$dep_delay = rm.out(DL$dep_delay)
DL$arr_delay = rm.out(DL$arr_delay)
F9 = subset(dataset,dataset$carrier =="F9")
F9$dep_delay = rm.out(F9$dep_delay)
F9$arr_delay = rm.out(F9$arr_delay)
HA = subset(dataset,dataset$carrier =="HA")
HA$dep_delay = rm.out(HA$dep_delay)
HA$arr_delay = rm.out(HA$arr_delay)
OO = subset(dataset,dataset$carrier =="OO")
OO$dep_delay = rm.out(OO$dep_delay)
OO$arr_delay = rm.out(OO$arr_delay)
UA = subset(dataset,dataset$carrier =="UA")
UA$dep_delay = rm.out(UA$dep_delay)
UA$arr_delay = rm.out(UA$arr_delay)
US = subset(dataset,dataset$carrier =="US")
US$dep_delay = rm.out(US$dep_delay)
US$arr_delay = rm.out(US$arr_delay)
VX = subset(dataset,dataset$carrier =="VX")
VX$dep_delay = rm.out(VX$dep_delay)
VX$arr_delay = rm.out(VX$arr_delay)
WN = subset(dataset,dataset$carrier =="WN")
WN$dep_delay = rm.out(WN$dep_delay)
WN$arr_delay = rm.out(WN$arr_delay)

#ghep cac subset theo carrier lai
new_dataset <- rbind(AA,AS,B6,DL,F9,HA,OO,UA,US,VX,WN)
#kiem tra lai so luong va ty le NA trong dataset moi
apply(is.na(new_dataset),2,sum)  
apply(is.na(new_dataset),2,mean)

#chuyen cac NA cua outliers thanh gia tri trung binh
AA$dep_delay[is.na(AA$dep_delay)] = mean(AA$dep_delay, na.rm = T)
AS$dep_delay[is.na(AS$dep_delay)] = mean(AS$dep_delay, na.rm = T)
B6$dep_delay[is.na(B6$dep_delay)] = mean(B6$dep_delay, na.rm = T)
DL$dep_delay[is.na(DL$dep_delay)] = mean(DL$dep_delay, na.rm = T)
F9$dep_delay[is.na(F9$dep_delay)] = mean(F9$dep_delay, na.rm = T)
HA$dep_delay[is.na(HA$dep_delay)] = mean(HA$dep_delay, na.rm = T)
OO$dep_delay[is.na(OO$dep_delay)] = mean(OO$dep_delay, na.rm = T)
UA$dep_delay[is.na(UA$dep_delay)] = mean(UA$dep_delay, na.rm = T)
US$dep_delay[is.na(US$dep_delay)] = mean(US$dep_delay, na.rm = T)
VX$dep_delay[is.na(VX$dep_delay)] = mean(VX$dep_delay, na.rm = T)
WN$dep_delay[is.na(WN$dep_delay)] = mean(WN$dep_delay, na.rm = T)
AA$arr_delay[is.na(AA$arr_delay)] = mean(AA$arr_delay, na.rm = T)
AS$arr_delay[is.na(AS$arr_delay)] = mean(AS$arr_delay, na.rm = T)
B6$arr_delay[is.na(B6$arr_delay)] = mean(B6$arr_delay, na.rm = T)
DL$arr_delay[is.na(DL$arr_delay)] = mean(DL$arr_delay, na.rm = T)
F9$arr_delay[is.na(F9$arr_delay)] = mean(F9$arr_delay, na.rm = T)
HA$arr_delay[is.na(HA$arr_delay)] = mean(HA$arr_delay, na.rm = T)
OO$arr_delay[is.na(OO$arr_delay)] = mean(OO$arr_delay, na.rm = T)
UA$arr_delay[is.na(UA$arr_delay)] = mean(UA$arr_delay, na.rm = T)
US$arr_delay[is.na(US$arr_delay)] = mean(US$arr_delay, na.rm = T)
VX$arr_delay[is.na(VX$arr_delay)] = mean(VX$arr_delay, na.rm = T)
WN$arr_delay[is.na(WN$arr_delay)] = mean(WN$arr_delay, na.rm = T)

new_dataset <- rbind(AA,AS,B6,DL,F9,HA,OO,UA,US,VX,WN)
apply(is.na(new_dataset), 2, which)

#Thong ke mo ta cho bien dep_delay theo carrier
length = tapply(new_dataset$dep_delay, new_dataset$carrier,length)
mean = tapply(new_dataset$dep_delay, new_dataset$carrier,mean)
sd = tapply(new_dataset$dep_delay, new_dataset$carrier,sd)
min = tapply(new_dataset$dep_delay, new_dataset$carrier,min)
max = tapply(new_dataset$dep_delay, new_dataset$carrier,max)
Q1 = tapply(new_dataset$dep_delay, new_dataset$carrier,quantile,probs = 0.25)
Q2 = tapply(new_dataset$dep_delay, new_dataset$carrier,quantile,probs = 0.5)
Q3 = tapply(new_dataset$dep_delay, new_dataset$carrier,quantile,probs = 0.75)
t(data.frame(length,mean,sd,min,max,Q1,Q2,Q3))

#Thong ke mo ta cho bien arr_delay theo carrier
length = tapply(new_dataset$arr_delay, new_dataset$carrier,length)
mean = tapply(new_dataset$arr_delay, new_dataset$carrier,mean)
sd = tapply(new_dataset$arr_delay, new_dataset$carrier,sd)
min = tapply(new_dataset$arr_delay, new_dataset$carrier,min)
max = tapply(new_dataset$arr_delay, new_dataset$carrier,max)
Q1 = tapply(new_dataset$arr_delay, new_dataset$carrier,quantile,probs = 0.25)
Q2 = tapply(new_dataset$arr_delay, new_dataset$carrier,quantile,probs = 0.5)
Q3 = tapply(new_dataset$arr_delay, new_dataset$carrier,quantile,probs = 0.75)
t(data.frame(length,mean,sd,min,max,Q1,Q2,Q3))

#ve boxplot the hien phan phoi cua bien arr_delay va dep_delay theo carrier
boxplot(arr_delay~carrier, xlab = "carrier", ylab = "arr_delay", main = "Boxplot of arr_delay for a category of Carrier", data = new_dataset, col = 2:7) 
boxplot(dep_delay~carrier, xlab = "carrier", ylab = "dep_delay", main = "Boxplot of dep_delay for a category of Carrier", data = new_dataset, col = 2:7)

origin_freq <- table(new_dataset$origin)
origin_percent <- round(origin_freq/sum(origin_freq)*100, 1)


# Plot the pie chart with percentages
pie(origin_freq, main = "Origin Distribution", col = rainbow(length(origin_freq)),
    labels = paste0(names(origin_freq), " (", origin_percent, "%)"))
hist(new_dataset$arr_delay, main = "Arrival Delay Distribution", xlab = "Delay (minutes)", 
     ylab = "Frequency", breaks = seq(-60, 60, by = 2), col = "blue")
hist(new_dataset$dep_delay, main = "Departure Delay Distribution", xlab = "Delay (minutes)", 
     ylab = "Frequency", breaks = seq(-30, 50, by = 2), col = "blue")

AA= subset (new_dataset, new_dataset$carrier == "AA")
qqnorm(AA$dep_delay)
qqline(AA$dep_delay)
ad.test(AA$dep_delay)
 
kruskalmc(new_dataset$dep_delay,new_dataset$carrier)

dataset_lr1 <- lm(arr_delay~carrier+origin+dep_delay+distance,dataset)
summary(dataset_lr1)
dataset_lr2 <- lm(arr_delay~origin+dep_delay+distance,dataset)
summary(dataset_lr2) #Summarize
anova(dataset_lr1,dataset_lr2) #Apply anova

