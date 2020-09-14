# Importing packages
library(tidyverse)
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)
# Reproduce the same results always
set.seed(123)
# Reading in the data file
InsuranceData <- read.csv("C:/Users/SAYAN/Desktop/Marketing-Customer-Value-Analysis.csv")

head(InsuranceData) # Checking top 6 observations of dataset
tail(InsuranceData) # Checking bottom 6 observations of dataset
# Remove Customer ID from data set.
InsuranceData <- InsuranceData[,-c(1)] 
#Cleaning the data
colnames(InsuranceData)
colnames(InsuranceData) <- str_replace_all(colnames(InsuranceData),"[.]","")
colnames(InsuranceData)
# Data Understanding
dim(InsuranceData)
str(InsuranceData)

# Checking null values in each column and storing the value in a data frame na_counts
na_counts <- sapply(InsuranceData, function(y) sum(is.na(y)))
na_counts <- data.frame(na_counts)
na_counts
# Unique Values of each column
sapply(InsuranceData, data.table::uniqueN)
range(InsuranceData$CustomerLifetimeValue)
mean(InsuranceData$CustomerLifetimeValue)
sd(InsuranceData$CustomerLifetimeValue)
summary(InsuranceData$CustomerLifetimeValue)

var(InsuranceData$CustomerLifetimeValue)
skewness(InsuranceData$CustomerLifetimeValue)
kurtosis(InsuranceData$CustomerLifetimeValue) 

#hist(InsuranceData$CustomerLifetimeValue, col = "#FF5733", xlab = "CLV")
hist(InsuranceData$CustomerLifetimeValue, breaks = (max(InsuranceData$CustomerLifetimeValue) - min(InsuranceData$CustomerLifetimeValue))/100, freq = FALSE, main = "CLV Histogram", xlab = "CLV", border = "#FF5733")

range(InsuranceData$MonthlyPremiumAuto)
mean(InsuranceData$MonthlyPremiumAuto)
sd(InsuranceData$MonthlyPremiumAuto)
summary(InsuranceData$MonthlyPremiumAuto)
var(InsuranceData$MonthlyPremiumAuto)
skewness(InsuranceData$MonthlyPremiumAuto)
kurtosis(InsuranceData$MonthlyPremiumAuto)
cor(InsuranceData$MonthlyPremiumAuto,InsuranceData$CustomerLifetimeValue)

#hist(InsuranceData$MonthlyPremiumAuto, col = "#00AFBB", xlab = "Monthly Premium Auto")
hist(InsuranceData$MonthlyPremiumAuto, breaks = (max(InsuranceData$MonthlyPremiumAuto) - min(InsuranceData$MonthlyPremiumAuto))/1, freq = FALSE, main = "Monthly Premium Histogram", xlab = "Monthly Premium", border = "#00AFBB")

plot(x=InsuranceData$MonthlyPremiumAuto, y=InsuranceData$CustomerLifetimeValue, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of MPA vs CLV")

range(InsuranceData$TotalClaimAmount)
mean(InsuranceData$TotalClaimAmount)
sd(InsuranceData$TotalClaimAmount)
summary(InsuranceData$TotalClaimAmount)
var(InsuranceData$TotalClaimAmount)
skewness(InsuranceData$TotalClaimAmount)
kurtosis(InsuranceData$TotalClaimAmount) 
cor(InsuranceData$TotalClaimAmount,InsuranceData$CustomerLifetimeValue)

#hist(InsuranceData$TotalClaimAmount, col = "#FC4E07", xlab = "Total Claim Amount")
hist(InsuranceData$TotalClaimAmount, breaks = (max(InsuranceData$TotalClaimAmount) - min(InsuranceData$TotalClaimAmount))/10, freq = FALSE, main = "Total Claim Amount Histogram", xlab = "Total Claim Amount", border = "#FC4E07")

plot(x=InsuranceData$TotalClaimAmount, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="TotalClaimAmount", ylab="CustomerLifetimeValue",
     main="Scatterplot of TCA vs CLV")

cor(InsuranceData$Income,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$Income, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="Income", ylab="CustomerLifetimeValue",main="Scatterplot of Income vs CLV")

cor(InsuranceData$MonthsSinceLastClaim,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$MonthsSinceLastClaim, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSinceLastClaim vs CLV")

cor(InsuranceData$MonthsSincePolicyInception,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$MonthsSincePolicyInception, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSincePolicyInception vs CLV")

cor(InsuranceData$NumberofOpenComplaints,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$NumberofOpenComplaints, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="NumberofOpenComplaints", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofOpenComplaints vs CLV")

cor(InsuranceData$NumberofPolicies,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$NumberofPolicies, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="NumberofPolicies", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofPolicies vs CLV")

ggplot(InsuranceData, aes(x=Coverage, y= CustomerLifetimeValue, fill = Coverage)) + 
  geom_boxplot() + 
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") + 
  ggtitle("Visualization of CLV wrt Coverage")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Coverage = InsuranceData$Coverage), FUN = sum)
aggData
ggplot(data = aggData, aes(x = Coverage, y = prop.table(stat(aggData$x)), fill = Coverage, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Coverage', y = 'CLV in Percentage', fill = 'Coverage') + 
  ggtitle("CLV Distribution by Coverage")

ggplot(InsuranceData, aes(x=Education, y= CustomerLifetimeValue, fill = Education)) + 
  geom_boxplot() + 
  labs(x="Education",y = "Customer Life Time Value", fill="Education") + 
  ggtitle("Visualization of CLV wrt Education")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Education = InsuranceData$Education), FUN = sum)

ggplot(data = aggData, aes(x = Education, y = prop.table(stat(aggData$x)), fill = Education, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Education') + 
  ggtitle("CLV Distribution by Education")


ggplot(InsuranceData, aes(x=EmploymentStatus, y= CustomerLifetimeValue, fill = EmploymentStatus)) + 
  geom_boxplot() + 
  labs(x="Employment Status",y = "Customer Life Time Value", fill="Employment Status") + 
  ggtitle("Visualization of CLV wrt Employment Status")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(EmploymentStatus = InsuranceData$EmploymentStatus), FUN = sum)

ggplot(data = aggData, aes(x = EmploymentStatus, y = prop.table(stat(aggData$x)), fill = EmploymentStatus, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'EmploymentStatus', y = 'CLV in Percentage', fill = 'EmploymentStatus') + 
  ggtitle("CLV Distribution by EmploymentStatus")


ggplot(InsuranceData, aes(x=Gender, y= CustomerLifetimeValue, fill = Gender)) + 
  geom_boxplot() + 
  labs(x="Gender",y = "Customer Life Time Value", fill="Gender") + 
  ggtitle("Visualization of CLV wrt Gender")


aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Gender = InsuranceData$Gender), FUN = sum)

ggplot(data = aggData, aes(x = Gender, y = prop.table(stat(aggData$x)), fill = Gender, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Gender', y = 'CLV in Percentage', fill = 'Gender') + 
  ggtitle("CLV Distribution by Gender")

ggplot(InsuranceData, aes(x=LocationCode, y= CustomerLifetimeValue, fill = LocationCode)) + 
  geom_boxplot() + 
  labs(x="Location",y = "Customer Life Time Value", fill="Location") + 
  ggtitle("Visualization of CLV wrt Location")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(LocationCode = InsuranceData$LocationCode), FUN = sum)

ggplot(data = aggData, aes(x = LocationCode, y = prop.table(stat(aggData$x)), fill = LocationCode, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'LocationCode', y = 'CLV in Percentage', fill = 'LocationCode') + 
  ggtitle("CLV Distribution by LocationCode")

ggplot(InsuranceData, aes(x=MaritalStatus, y= CustomerLifetimeValue, fill = MaritalStatus)) + 
  geom_boxplot() + 
  labs(x="Marital Status",y = "Customer Life Time Value", fill="Marital Status") + 
  ggtitle("Visualization of CLV wrt Marital Status")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(MaritalStatus = InsuranceData$MaritalStatus), FUN = sum)

ggplot(data = aggData, aes(x = MaritalStatus, y = prop.table(stat(aggData$x)), fill = MaritalStatus, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'MaritalStatus', y = 'CLV in Percentage', fill = 'MaritalStatus') + 
  ggtitle("CLV Distribution by MaritalStatus")


ggplot(InsuranceData, aes(x=PolicyType, y= CustomerLifetimeValue, fill = PolicyType)) + 
  geom_boxplot() + 
  labs(x="Policy Type",y = "Customer Life Time Value", fill="Policy Type") + 
  ggtitle("Visualization of CLV wrt Policy Type")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(PolicyType = InsuranceData$PolicyType), FUN = sum)

ggplot(data = aggData, aes(x = PolicyType, y = prop.table(stat(aggData$x)), fill = PolicyType, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'PolicyType', y = 'CLV in Percentage', fill = 'PolicyType') + 
  ggtitle("CLV Distribution by PolicyType")


ggplot(InsuranceData, aes(x=RenewOfferType, y= CustomerLifetimeValue, fill = RenewOfferType)) + 
  geom_boxplot() + 
  labs(x="Renew Offer Type",y = "Customer Life Time Value", fill="Renew Offer Type") + 
  ggtitle("Visualization of CLV wrt Renew Offer Type")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(RenewOfferType = InsuranceData$RenewOfferType), FUN = sum)

ggplot(data = aggData, aes(x = RenewOfferType, y = prop.table(stat(aggData$x)), fill = RenewOfferType, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'RenewOfferType', y = 'CLV in Percentage', fill = 'RenewOfferType') + 
  ggtitle("CLV Distribution by RenewOfferType")


ggplot(InsuranceData, aes(x=SalesChannel, y= CustomerLifetimeValue, fill = SalesChannel)) + 
  geom_boxplot() + 
  labs(x="Sales Channel",y = "Customer Life Time Value", fill="Sales Channel") + 
  ggtitle("Visualization of CLV wrt Sales Channel")


aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(SalesChannel = InsuranceData$SalesChannel), FUN = sum)

ggplot(data = aggData, aes(x = SalesChannel, y = prop.table(stat(aggData$x)), fill = SalesChannel, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'SalesChannel', y = 'CLV in Percentage', fill = 'SalesChannel') + 
  ggtitle("CLV Distribution by SalesChannel")


ggplot(InsuranceData, aes(x=VehicleClass, y= CustomerLifetimeValue, fill = VehicleClass)) + 
  geom_boxplot() + 
  labs(x="Vehicle Class",y = "Customer Life Time Value", fill="Vehicle Class") + 
  ggtitle("Visualization of CLV wrt Vehicle Class")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(VehicleClass = InsuranceData$VehicleClass), FUN = sum)

ggplot(data = aggData, aes(x = VehicleClass, y = prop.table(stat(aggData$x)), fill = VehicleClass, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'VehicleClass', y = 'CLV in Percentage', fill = 'VehicleClass') + 
  ggtitle("CLV Distribution by VehicleClass")


ggplot(InsuranceData, aes(x=VehicleSize, y= CustomerLifetimeValue, fill = VehicleSize)) + 
  geom_boxplot() + 
  labs(x="Vehicle Size",y = "Customer Life Time Value", fill="Vehicle Size") + 
  ggtitle("Visualization of CLV wrt Vehicle Size")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(VehicleSize = InsuranceData$VehicleSize), FUN = sum)

ggplot(data = aggData, aes(x = VehicleSize, y = prop.table(stat(aggData$x)), fill = VehicleSize, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'VehicleSize', y = 'CLV in Percentage', fill = 'VehicleSize') + 
  ggtitle("CLV Distribution by VehicleSize")


ggplot(InsuranceData, aes(x=State, y= CustomerLifetimeValue, fill = State)) + 
  geom_boxplot() + 
  labs(x="State",y = "Customer Life Time Value", fill="State") + 
  ggtitle("Visualization of CLV wrt State")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(State = InsuranceData$State), FUN = sum)

ggplot(data = aggData, aes(x = State, y = prop.table(stat(aggData$x)), fill = State, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'State', y = 'CLV in Percentage', fill = 'State') + 
  ggtitle("CLV Distribution by State")


ggplot(InsuranceData, aes(x=Policy, y= CustomerLifetimeValue, fill = Policy)) + 
  geom_boxplot() + 
  labs(x="Policy",y = "Customer Life Time Value", fill="State") + 
  ggtitle("Visualization of CLV wrt Policy")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Policy = InsuranceData$Policy), FUN = sum)

ggplot(data = aggData, aes(x = Policy, y = prop.table(stat(aggData$x)), fill = Policy, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Policy', y = 'CLV in Percentage', fill = 'Policy') + 
  ggtitle("CLV Distribution by Policy")




dataContinous <- dplyr::select_if(InsuranceData, ~!is.factor(.))
str(dataContinous)
dim(dataContinous)
trainIndex <- createDataPartition(dataContinous$CustomerLifetimeValue, p=0.80, list = FALSE)
#print(trainIndex)
# 80% Train dataset for regression analysis
insurncTrain <- dataContinous[trainIndex,]
# Remaining 30% Test dataset for testing
insurncTest <- dataContinous[-trainIndex,]
dim(dataContinous)
dim(insurncTrain)
dim(insurncTest)
#Regression
#lm is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance.

# Creating Linear Regression Model using all the continues indepedent variables.
fit <- lm(insurncTrain$CustomerLifetimeValue ~., data = insurncTrain) 
summary(fit) 
#Re Run Model
new_fit <- lm(insurncTrain$CustomerLifetimeValue ~ 
                MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount, 
              data = insurncTrain) 
summary(new_fit)

predictedCLV <- predict(new_fit)  
#print predicted CLV.
print(predictedCLV[1:10])

#print actual CLV to compare it with above calculated predicted CLV.
print(insurncTrain$CustomerLifetimeValue[1:10])

residualsCLV <- residuals(new_fit)
print(residualsCLV[1:10])

predicatedTestData=predict(new_fit,insurncTest)
print(predicatedTestData[1:10])

InsuranceTrainData <- cbind(insurncTrain,predictedCLV,residualsCLV)
head(InsuranceTrainData)
#Calculate Error Rate
ErrorRate <- abs((InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$predictedCLV)/(InsuranceTrainData$CustomerLifetimeValue)*100)
print(ErrorRate[1:10])

InsuranceTrainData <- cbind(InsuranceTrainData, ErrorRate)
head(InsuranceTrainData)

mean(InsuranceTrainData$ErrorRate, na.rm = TRUE)

hist(ErrorRate, col = "blue")
boxplot(ErrorRate)
shapiro.test(residualsCLV[0:5000])
hist(residualsCLV,col = "green")
plot(new_fit, which=1, col=c("blue"))
cor(InsuranceTrainData)

# Variance Inflation Factors
car::vif(new_fit)
bptest(new_fit)
dwt(new_fit)
ErrorRate <- mean(abs((InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$predictedCLV)/InsuranceTrainData$CustomerLifetimeValue) *100 )
print(ErrorRate)


ggplot(InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +     # regression line  
  geom_segment(aes(xend = MonthlyPremiumAuto, yend = predictedCLV), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residualsCLV), size = abs(residualsCLV))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predictedCLV), shape = 1) +
  theme_bw()

ggplot(InsuranceTrainData,aes(x=MonthlyPremiumAuto,y=CustomerLifetimeValue))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Monthly Premium")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Monthly Premium")


ggplot(InsuranceTrainData,aes(x=TotalClaimAmount,y=CustomerLifetimeValue))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Total Claim Amount")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Total Claim Amount")