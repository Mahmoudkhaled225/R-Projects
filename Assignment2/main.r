#name:Mahmoud Khaled Helmy
#ID: 20 18 80 45
#name:Nourhan Fahmy tamam
#ID: 20198117
#installation And Setup 
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.16")
BiocManager::install("antiProfilesData")
install.packages("corrplot")
library(corrplot)



#loading antiProfilesData in var called apColonData
apColonData<-antiProfilesData::apColonData
pdata=pData(apColonData)
edata=exprs(apColonData)
fdata = fData(apColonData)












#1.a Show the type of each column
sapply(pdata, class)
#1.b Show column names and rows name
colnames(pdata)
rownames(pdata)
#1.c Calculate summary of each column
summary(edata)
#1.d Show frequency of categorical data, taking into the consideration, NA values frequency if any.
table(pdata$Status,useNA="ifany")
#1.e Calculate the correlation and covariance between the first 10 columns only of our data set and draw full correlation matrix.
my_data <- edata[, c(1:10)]
mydata.cov<-cov(my_data, y = NULL)
print(mydata.cov)
mydata.cor<-cor(my_data)
print(mydata.cor)

# Drawing full correlation matrix.
result <- cor(my_data)
corrplot(result, type = "upper", tl.col = "black", tl.srt = 85)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = result, col = col, symm = TRUE, cexCol = 0.99)
#1.f For both genes: GSM95478,GSM95473 show the plot with a line of their relation.
fit1 = lm(edata[,"GSM95478"]~edata[,"GSM95473"])
plot(edata[,"GSM95478"],edata[,"GSM95473"])
abline(fit1)



#Q2 nourhan's solution 
Pc1 = prcomp(edata)
Pc1$rotation[,1]
edataCenter = t(t(edata) - colMeans(edata))
Svd2 = svd(edataCenter)
Svd2$v[,1]
color_list = c("deeppink2", "darkslategray4")
plot(Pc1$rotation[,1], Svd2$v[,1], col=color_list)


#Q3
observed <-c (29, 24, 22, 19, 21, 18, 19, 20, 23, 18, 20, 23)
chisq.test(observed)
#Hypothesis H0 is uniformly births distribution where H1 is not 

#Q4
col10=edata[,1:10]
dist1 = dist(t(col10))
hclust1 = hclust(dist1)
plot(hclust1,hang = -1) # hang = - 1 to make labels written on the same level
kmeans1 = kmeans(edata,3)
kmeans1$centers

xxx <- seq(-10,10,by = 0.01)
yy <- dnorm(xxx,0,1)
plot(yy)