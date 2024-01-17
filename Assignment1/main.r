#Mahmoud Khaled Helmy
#20 18 80 45 
#B1
install.packages("eeptools")
library("eeptools")
data = read.csv("C:/Users/Abo El-Magd/Desktop/B1-20188045-Assignment1/data.csv")
#Q1
print("first ten rows")
head(data,10)
print("last ten rows")
tail(data,10)

#Q2
slice= data[order(data$dob,decreasing=TRUE),]
output=slice[1:3, c("gender", "avg_commute", "ancestry")]
print(output)

#Q3
data[data$children > 2, c("gender", "daily_internet_use","avg_commute", "ancestry","disease")]

#Q4
table(complete.cases(data))



#Q5
summary1 = data[c("zipcode","children","avg_commute","daily_internet_use")]
summary(summary1)
table1=data[c("disease","ancestry","marital_status","education","employment_status","gender","id")]
for (i in colnames(table1)){
  print(table(table1[[i]]))
}



#Q6
#there is no any missing values so nothing will be droped


#Q7
#ava usage ber education
output = tapply(data$daily_internet_use,data$education,mean,na.rm=t)
barplot(output)

#Q8
hist(data$children,breaks = 7)

#Q9
male=subset(data, gender="male")
female=subset(data, gender="female")
plot(male$avg_commute,type = "l")
plot(female$avg_commute,type = "l")



#Q10
barplot(table(data$gender))


#Q11
zz <- table(data$gender, data$disease)
barplot(zz,beside = TRUE,las=2)

#Q12
temp = as.Date(data$dob)
days = age_calc(temp)
hist(days)


#Q13
barplot(table(data$disease),ylim = c(0, 350),xlab="diseases",ylab="count",las=2)


#Q14
barplot(table(data$ancestry),las=2)
        