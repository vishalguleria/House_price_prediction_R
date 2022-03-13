Rdataset<-read.table(file="clipboard",sep="\t",header=TRUE)
#to see the data
View(Rdataset)

#to see the structure
str(Rdataset)

#to see the summary
summary(Rdataset)
nrow(Rdataset)
ncol(Rdataset)
colnames(Rdataset)

#data cleaning
library(dplyr)
Rdataset%>% select(c(-1,-2,-17,-18,-19))->houses
houses$waterfront<-factor(houses$waterfront,labels=c("No","yes"))
houses$view<-factor(houses$view,labels=c("road facing","mountain facing"," trees facing","garden facing","city facing"))
houses$condition<-factor(houses$condition,labels=c("excellent","average","good"))
#After data cleaning
nrow(houses)
ncol(houses)
View(houses)
colnames(houses)

#data visualization
library(ggplot2)
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40) #skewed normal distribution
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40,fill="lightblue",col="blue")
ggplot(data=houses,aes(y=price,x=waterfront,fill=waterfront))+geom_boxplot()
ggplot(data=houses,aes(y=price,x=condition,fill=condition))+geom_boxplot()
ggplot(houses,aes(y=price,x=sqft_living))+geom_point()+geom_smooth(method="lm",se=F) 
ggplot(houses,aes(y=price,x=yr_built))+geom_point()+geom_smooth(method="lm",se=F) 
ggplot(houses,aes(y=price,x=floors))+geom_point()+geom_smooth(method="lm",se=F) 
ggplot(houses,aes(x=sqft_living,y=price,col=factor(bedrooms)))+geom_point()+geom_smooth(method="lm",se=F)

#t-test on price feature(As the histogram is not normally distributed so we need to convert it to normal)
hist(log(houses$price))
mean(houses$price)
t.test(houses$price,mu=59600)


#splitting Data
 library(catools)
sample.split(houses$price,SplitRatio=0.80)->split_index
train<-subset(houses,split_index==T)
test<-subset(houses,split_index==F)
nrow(train)
nrow(test)

#model building
lm(price~.,data=train)->mod1
predict(mod1,test)->result
cbind(actual=test$price,predicted=result)->compare_result
View(compare_result)
as.data.frame(compare_result)->compare_result
compare_result$actual-compare_result$predicted->error
cbind(compare_result,error)->compare_result
View(compare_result)
sqrt(mean(compare_result$error^2))->rmse1
summary(mod1)

#model 2
lm(price~.-sqft_lot-sqft_basement-yr_renovated-sqft_lot15,data=train)->mod2
predict(mod2,test)->result2
cbind(actual=test$price,predicted=result2)->compare_result2
View(compare_result2)
as.data.frame(compare_result2)->compare_result2
cbind(compare_result2,error)->compare_result2
sqrt(mean(compare_result2$error^2))->rmsel2
rmsel2
summary(mod2)




