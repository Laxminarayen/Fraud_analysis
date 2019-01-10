set.seed(06)
library(caret)
library(stats)
library(magrittr)
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
install.packages("Metrics")
library(Metrics)
library(e1071)
install.packages("rpart.plot")
library(rpart.plot)
library(class)
library(ROCR)
install.packages("pROC")
library(pROC)
library("gridExtra")
library("grid")
install.packages("plot_ly")
install.packages("corrplot")
library("corrplot")
r <- read.csv("E://PS_20174392719_1491204439457_log.csv")
colSums(is.na(r))
View(factor(r$step))

##Part1- Visualizations
a_small=sample_frac(r,size=0.05,replace = FALSE)
a_small %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_density(fill = "brown",alpha="0.2")
plot1=ggplot(a_small,aes(x = type))+geom_bar()+theme(axis.text.x = element_text(angle = 45))+
  xlab("Types of transaction")+  ylab("Count of transacion")+ggtitle("All transactions by type")
f=subset(r,r$isFraud==1)
plot2=ggplot(f,aes(x = type))+geom_bar()+
  xlab("Types of transaction")+  ylab("Count of transacion")+ggtitle("Fraud transactions by type")
grid.arrange(plot1,plot2,nrow=1)
ggplot(a_small,aes(x = step))+geom_bar()+theme(axis.text.x = element_text(angle = 45))
a$step=a$step %%24
plot3=ggplot(r,aes(x = step))+geom_bar()+theme(axis.text.x = element_text(angle = 45))+
  xlab("Hour of the day")+ylab("Number of transactions")+ggtitle("All transactions distribution thorughout the day")
f=subset(r,r$isFraud==1)
plot4=ggplot(f,aes(x = step))+geom_bar()+ggtitle("Fraud transactions distribution thorughout the day")+theme(axis.text.x = element_text(angle = 45))+xlab("Hour of the day")+ylab("Number of fraud transactions")
grid.arrange(plot3,plot4,nrow=1)
ggplot(a_small,aes(x = amount))+geom_histogram(binwidth = 1000000 )+theme(axis.text.x = element_text(angle = 45))
mean(r$amount)
min(r$amount)
max(r$amount)
ggplot(f,aes(x = amount))+geom_histogram(binwidth = 1000000 )+theme(axis.text.x = element_text(angle = 45))+
  xlab("fraud transaction amount")
ggplot(f,aes(x = amount))+geom_histogram(binwidth = 1000000 )+theme(axis.text.x = element_text(angle = 45))+
  xlab("fraud transaction amount")
corr <- sapply(r, is.numeric)
corrchart <- cor(r[,corr])
corrplot(corrchart, main = '\n\n Correlation Chart',method = "number")


##PART-2 Modelling
new<-r[,c(3,5,6,8,9,10)]
library(corrgram)
library(lattice)
new$isFraud<-factor(new$isFraud,  levels = c(0,1))
d <- createDataPartition(new$isFraud,p =0.8, list = FALSE) %>% c()
Train <- new[d,]
Test <- new[-d,]
rep_set <- Test[0:1000,]
corrgram(rep_set, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corregram for isfraud")
table(new$isFraud)
model <- glm(isFraud ~ .,Train, family = "binomial")
summary(model)
predict <- predict(model,Test)
prediction <-prediction(predict, Test$isFraud)
performance<-performance(prediction, "tpr", "fpr")
plot(performance, colorize=T)#Time Consuming so eliminating this
performance(prediction, "auc")@y.values

model_dt <- rpart(isFraud ~ ., data = Train)
prp(model_dt) 
predict_dt <- predict(model_dt, Test, type = "class")
p_test_dt<-prediction(as.numeric(predict_dt), as.numeric(Test$isFraud))
perf_tree<-performance(p_test_dt, "tpr", "fpr")
plot(perf_tree)
performance(p_test_dt, "auc")@y.values