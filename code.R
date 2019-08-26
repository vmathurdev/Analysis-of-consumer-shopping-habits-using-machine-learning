#Checking the default work directory
getwd()

#Setting the work directory
setwd('C:/Users/samsung/Desktop/Update')
getwd()

#Loading the dataset
data <- read.csv('C:/Users/samsung/Desktop/Update/dataset.csv', stringsAsFactors = FALSE)

#Check for missing values
colSums(is.na(data))

# Replace missing values on age with the mean age.
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# Replace missing values on income with the mean income.
data$Estimated.Income[is.na(data$Estimated.Income)] <- mean(data$Estimated.Income,na.rm=T)

# Replace missing values on expenditure with the mean expenditure.
data$Amount.Spent.on.Goods[is.na(data$Amount.Spent.on.Goods)] <- mean(data$Amount.Spent.on.Goods,na.rm=T)

Age <- data$Age

#Check for missing values to validate the above conditions
colSums(is.na(data))

#Plot to estimate the age and income effect
plot(data$Estimated.Income, data$Age, main="Scatterplot to assess Age and income relation", xlab='Income', ylab='Age', pch=19, col="Yellow")
#Adding a regression line so as to make the analysis more clear
abline(lm(data$Age ~ data$Estimated.Income), col="red")

#Plot to estimate the age and amount spent on shopping
plot(data$Amount.Spent.on.Goods, data$Age, main="Plot to assess age and the amount spent", xlab='Amount spent', ylab='Age', pch=19, col="green")
abline(lm(data$Age ~ data$Amount.Spent.on.Goods), col="red")


#Plot to verify the relation between income and amount spent on shopping

ggplot(data, aes(x=data$Estimated.Income, y=data$Amount.Spent.on.Goods)) + 
  geom_point()+ ggtitle("Estimate income expenditure on shopping") +
  geom_smooth(method=lm)

#Stats of different product purchases
library(ggplot2)
ggplot(data, aes(data$Retail.Purchases.â...Most.Frequent.Category)) +
  geom_bar(fill = "#0073C2FF")


#Age, method of payment and gender wise distribution
ggplot(data, aes(x = data$Age, y = data$Method.of.Payment)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = data$Gender, shape = data$Gender), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL) 



# Preference of products among gender
ggplot(data, aes(x = factor(1), y = data$Retail.Purchases.â...Most.Frequent.Category)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = data$Gender, shape = data$Gender), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   

#Age and product choice 
ggplot(data, aes(x = data$Age, y = data$Retail.Purchases.â...Most.Frequent.Category)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = data$Gender, shape = data$Gender), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL) 


# Income and Gender
ggplot(data, aes(x = factor(1), y = data$Estimated.Income)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = data$Gender, shape = data$Gender), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL) 


#Correlation between different variables
# Subsetting the data to include continuous variables like Age, Income and expenditure
drop.vars <- names(data) %in% c("X","Retail.Purchases.â...Most.Frequent.Category", "Method.of.Payment","Address","Gender") 
data.2 <- data[!drop.vars]
library(corrplot)
cor(data.2,method="pearson")

#correlation plot
require(corrplot)
corrplot(cor(data.2),type = "upper", order = "hclust")


#Determining the number of clusters- Clustering can only be done on continuous variables namely "Age", "Income" and "Expenditure"

str(data.2)

# This centers and scales all variables
customer <- scale(data.2) 
str(customer)

# Cluster with Ward's method (the most efficient method to determine the distance between clusters is ward's method)
hc <- hclust(dist(customer, method="euclidean"), method="ward.D2")
hc
# plot to see the dendrogram of clusters
plot(hc, hang = -0.01, cex = 0.7)
#Nbclust helps us to determine the number of cluster based on various parameters
install.packages("NbClust")
#Factoextra gives us methods like elbow curve and the silhouette method to determine the number of clusters
install.packages("factoextra")
library(factoextra)
#Elbow curve method
fviz_nbclust(customer, FUN = hcut, method = "wss")
# Silhouette method
fviz_nbclust(customer, FUN = hcut, method = "silhouette")

# Graphical representation of the clusters
fit <- cutree(hc, k=2)
table(fit)
plot(hc, hang = -0.01, cex = 0.7)
rect.hclust(hc, k=2, border = "red")

library(NbClust)
set.seed(1234)
nc<-NbClust(customer,min.nc=2,max.nc=15,method="ward.D2")
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),
        xlab="Number of clusters",ylab="Number of criteria",
        main="Number of Clusters Chosen by up to 26 Criteria")


#Recommendation system

data.3 <- data


#converting the gender categorical variable to dummy variable
data.3$gender <- ifelse(data.3$Gender == "Male", 1, 0)

n.train <- round(nrow(data.3)* 0.7,0) 

set.seed(123)
# 70% Training data and 30% for validation data
sub = round(sample(nrow(data.3)*0.7), 0)
data.3.train = data.3[sub, ]
data.3.test = data.3[-sub, ]

#Re-aligning the variables
Age <- data.3$Age
Address <- data.3$Address
Expenditure <- data.3$Amount.Spent.on.Goods
Income <- data.3$Estimated.Income
Method <- data.3$Method.of.Payment
Item <- data.3$Retail.Purchases.â...Most.Frequent.Category

#Using logistic regression by converting gender into dummy variable
full.model = glm(gender ~., data = data.3.train,family = binomial("logit"))

summary(full.model)

# Validation data
validation.predictions <- predict(full.model, newdata = data.3.test, type = "response")
validation.class.predictions = as.numeric(validation.predictions > 0.5)
predicted.validation <- as.factor(validation.class.predictions)
actual.validation <- as.factor(data.3.test$gender)

#Assessing the accuracy of the model
confusionMatrix(predicted.validation,actual.validation,positive="1")


#Verifying with decision tree
install.packages("rpart", dependencies=TRUE)
library(rpart)
fit.tree <- rpart(gender ~ .,data = data.3,method = "class")
summary(fit.tree)
plotcp(fit.tree) # This is the cross-validation plot.
fit.tree$variable.importance
barplot(rev(fit.tree$variable.importance),col="black")
printcp(fit.tree)


# Extracting the location based data and anlysing the clusters 
#These packages are used for string manipulation
library(stringr)
install.packages("tm")
library(tm)
#Extract the location from the address
data.3$Location <- str_sub(Address, -16)
# Remove any wanted alphanumeric/numeric characters in the location
data.3$locname <- removeNumbers(data.3$Location)

# Write a new working file in the destination directory
write.csv(data.3,file="data.3.csv")
newdata <- read.csv("data.3.csv")

#Location based analytics 

# Location and method of payment based summary statistics
xtabs(~locname+Method, data= data.3)
# Location and gender wise summary statistics
xtabs(~locname+Gender, data= data.3)
# Location and Item purchased based summary statistics
xtabs(~locname+Item, data= data.3)

library(Hmisc)
# To create the intervals in the variables as income, age and expenditure are continous variables
a <- cut2(Income,c(10000,20000,30000,40000,50000,60000,70000,8000))
b <- cut2(Age,c(15,30,45,60,75,90))
c <- cut2(Expenditure,c(0,10,20,30,40,50,60,70,80,90,100))

# Location and Income based summary statistics
xtabs(~locname+a, data= data.3)
# Location and Age based summary statistics
xtabs(~locname+b, data= data.3)
# Location and Expenditure based summary statistics
xtabs(~locname+c, data= data.3)


