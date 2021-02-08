
library(knitr)
library(kableExtra)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(tidyverse)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(corrplot)
library(caTools)
library(party)
library(ISLR)
library(readxl)
library(pROC)
library(lattice)
library(e1071) 
library(ggplot2)
library(multiROC)
library(MLeval)
library(AppliedPredictiveModeling)
library(Hmisc)
library(quantmod) 
library(nnet)
library(caret)


blackfriday_raw <- read.csv(file = "/Users/nselvarajan/Desktop/R/Assignment4/Book1.csv", header = T,stringsAsFactors = F)
str(blackfriday_raw)

sapply(blackfriday_raw , function(x) sum(is.na(x)))

#Imputing 0 for NA in Product_Category_2
blackfriday_raw$Product_Category_2 <- as.numeric(blackfriday_raw$Product_Category_2)
blackfriday_raw[is.na(blackfriday_raw$Product_Category_2), "Product_Category_2"] <- 0

#Imputing 0 For NA in Product_Category_3
blackfriday_raw$Product_Category_3 <- as.numeric(blackfriday_raw$Product_Category_3)
blackfriday_raw[is.na(blackfriday_raw$Product_Category_3), "Product_Category_3"] <- 0


blackfriday_raw$User_ID <- as.factor(blackfriday_raw$User_ID)
blackfriday_raw$Product_ID <- as.factor(blackfriday_raw$Product_ID)
blackfriday_raw$Marital_Status <- as.factor(ifelse(blackfriday_raw$Marital_Status == 1, 'Married', 'Single'))
blackfriday_raw$Age <- as.factor(blackfriday_raw$Age)
blackfriday_raw$Gender <- as.factor(ifelse(blackfriday_raw$Gender=='M', 'Male', 'Female'))
blackfriday_raw$Occupation <- as.factor(blackfriday_raw$Occupation)
blackfriday_raw$City_Category <- as.factor(blackfriday_raw$City_Category)
blackfriday_raw$Stay_In_Current_City_Years <- as.factor(blackfriday_raw$Stay_In_Current_City_Years)


library(ggplot2)
ggplot(data=blackfriday_raw, aes(x = Gender,y = Purchase, fill = Gender)) +
  geom_bar(stat="identity")+theme_minimal()+
  scale_fill_manual("legend",values = c("Female"= "#c4d8ba","Male"="#d8ceba"))+
  labs(x = 'Gender') +
  labs(y = 'Purchase') +
  ggtitle("Gender vs Purchase")

#Occupation vs purchase barplot
ggplot(data=blackfriday_raw, aes(x = Occupation,y = (..count..), fill = Purchase)) +
  geom_bar(position = "dodge",width = .5,fill="#c4d8ba") +
  labs(x = 'Ocupation') +
  labs(y = 'Purchase') +
  ggtitle("Occupation vs Purchase")

#Buyers according to Occupation, Marital Status and Age
ggplot(data=blackfriday_raw,aes(x=Occupation, fill=Age))+
  geom_bar(position = "stack")+
  facet_grid(Gender~Marital_Status)+
  labs(y="Number of buyers")+
  ggtitle("Buyers according to Occupation, Marital Status and Age")


#city category vs purchase barplot
table(blackfriday_raw$City_Category,blackfriday_raw$Stay_In_Current_City_Years)

ggplot(data=blackfriday_raw, aes(x = City_Category,y = Purchase, fill = City_Category)) +
  geom_bar(stat="identity")+theme_minimal()+
  scale_fill_manual("legend",values = c("A"= "#FF3333","B"="#3366FF","C"="#CCFF00"))+
  labs(x = 'City Category') +
  labs(y = 'Purchase') +
  ggtitle("City Category vs Purchase")

ggplot(data=blackfriday_raw,aes(x=Stay_In_Current_City_Years,fill=City_Category,col=Marital_Status))+
  geom_bar()+
  scale_fill_manual("City Category",values = c("A"= "lightpink","B"="lightblue","C"="lightgreen"))+
  scale_color_manual("Marital Status",values = c("darkgreen","red"))+
  ggtitle("Number of people staying in Current city according to Marital Status")+
  labs(x="Stay in Current City")+
  labs(y="Number of people")

#Marital status vs purchase barplot
ggplot(data=blackfriday_raw, aes(x = Marital_Status,y = Purchase, fill = Marital_Status)) +
  geom_bar(stat="identity")+theme_minimal()+
  scale_fill_manual("legend",values = c("Married"= "blue","Unmarried"="orange"))+
  labs(x = 'Marital Status') +
  labs(y = 'Purchase') +
  ggtitle("Marital Status vs Purchase")

#Stay in current city vs purchase barplot
ggplot(data=blackfriday_raw, aes(x = Stay_In_Current_City_Years,y = (..count..), fill = Purchase)) +
  geom_bar(position = "dodge",width = .5,fill="pink") +
  labs(x = 'Stay in city') +
  labs(y = 'Purchase') +
  ggtitle("Stay in current city vs Purchase")

# Age vs Purchase boxplot

table(blackfriday_raw$Marital_Status,blackfriday_raw$Gender,blackfriday_raw$Age)

ggplot(data=blackfriday_raw,aes(x= Age,y=Purchase, fill=Age)) + 
  geom_boxplot() + 
  facet_grid(Gender~Marital_Status) + 
  labs(x="Age",y="Purchase")+
  ggtitle("Age vs Purchase")

#No of buyers according to age,gender and marital status

ggplot(data=blackfriday_raw,aes(x=Age,fill=Gender,col=Marital_Status))+
  geom_bar(position = "dodge")+
  labs(x="Age")+
  labs(y="No of buyers")+
  scale_fill_manual("Gender",values = c("Female"= "pink","Male"="lightblue"))+
  scale_color_manual("Marital Status",values = c("darkgreen","red"))+
  ggtitle("Number of buyers according to age,gender and marital status")


#Number of purcahses made by each user -USER_ID
userIDcount<-as.data.frame(table(train_BFdata$User_ID))
names(userIDcount)<-c("User_ID","Number_of_Purchases")
train_BFdata<-as.data.frame(merge(train_BFdata,userIDcount,by='User_ID',all.x = TRUE))
test_BFdata<- as.data.frame(merge(test_BFdata,userIDcount,by='User_ID',all.x = TRUE))


# Number of purchases vs Age,gender and occupation

ggplot(data=train_BFdata,aes(x=Age,y=Number_of_Purchases,fill=Occupation))+
  geom_bar(position = "dodge",stat = "identity")+
  facet_grid(Gender~.)+
  scale_fill_discrete("Occupation")+
  ggtitle("")+
  labs(y="Number of purchases")

# Number of purchases vs Age,gender and stay in current city
ggplot(data=train_BFdata,aes(x=Age,y=Number_of_Purchases,fill=Stay_In_Current_City_Years))+
  geom_bar(position = "dodge",stat = "identity")+
  facet_grid(Gender~.)+
  scale_fill_discrete("Stay_In_Current_City_Years")+
  ggtitle("")+
  labs(y="Number of purchases")


str(blackfriday_raw)

correlated_data <- blackfriday_raw


correlated_data$User_ID <-as.numeric(as.factor(correlated_data$User_ID))
correlated_data$Product_ID <- as.numeric(as.factor(correlated_data$Product_ID))
correlated_data$Marital_Status <- as.numeric(as.factor(correlated_data$Marital_Status ))
correlated_data$Age <- as.numeric(as.factor(correlated_data$Age))
correlated_data$Gender <- as.numeric(as.factor(correlated_data$Gender))
correlated_data$Occupation <- as.numeric(as.factor(correlated_data$Occupation))
correlated_data$Stay_In_Current_City_Years <- as.numeric(as.factor(correlated_data$Stay_In_Current_City_Years))
correlated_data$City_Category <- as.numeric(as.factor(correlated_data$City_Category))                                                                                                                                                                                                      
str(correlated_data)

library(ggplot2)
# counts
p <- ggplot(data.frame(correlated_data$Product_Category_1), aes(x=correlated_data$Product_Category_1)) +
  ggtitle("Frequency distribution by Product Category 1") +
  xlab("Product Category 1") +
  geom_bar(colour="black", fill="#008080")
p + theme(
  plot.title = element_text(color="black", size=10, face="bold.italic"),
  axis.title.x = element_text(color="black", size=10, face="bold.italic"),
)

# counts
p <- ggplot(data.frame(correlated_data$Product_Category_2), aes(x=correlated_data$Product_Category_2)) +
  ggtitle("Frequency distribution by Product Category 2") +
  xlab("Product Category 2") +
  geom_bar(colour="black", fill="#008080")
p + theme(
  plot.title = element_text(color="black", size=10, face="bold.italic"),
  axis.title.x = element_text(color="black", size=10, face="bold.italic"),
)

# counts
p <- ggplot(data.frame(correlated_data$Product_Category_3), aes(x=correlated_data$Product_Category_3)) +
  ggtitle("Frequency distribution by Product Category 3") +
  xlab("Product Category 3") +
  geom_bar(colour="black", fill="#008080")
p + theme(
  plot.title = element_text(color="black", size=10, face="bold.italic"),
  axis.title.x = element_text(color="black", size=10, face="bold.italic"),
)








library(arules)
groceries <- read.transactions("/Users/nselvarajan/Desktop/R/Assignment4/Book1.csv", sep = " ")
summary(groceries)


customers_products = blackfriday_raw %>%
  select(User_ID, Product_ID) %>% #selecting the columns
  group_by(User_ID) %>% #grouping by "User_ID"
  arrange(User_ID) %>% #arranging by "User_ID"
  mutate(id = row_number()) %>% #defining a key column for each "Product_ID" and its corresponding "User_ID"
  spread(User_ID, Product_ID) %>% 
  t() #transposing the dataset from columns of "User_ID" to rows of "User_ID"
customers_products = customers_products[-1,]

write.csv(customers_products, file = 'customers_products.csv')
customers_products = read.transactions('customers_products.csv', sep = ',', rm.duplicates = T) #remove duplicates with rm.duplicates

summary(customers_products)


itemFrequencyPlot(customers_products, topN = 25)
itemFrequencyPlot(customers_products,topN = 18,type = "relative",col = "#FFFACD",xlab = "Item ID",ylab = "Frequency(relative)")


itemFrequencyPlot(customers_products, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")




rules = apriori(data = customers_products, parameter = list(support =
                                                              0.006, confidence = 0.25, minlen = 3))
#maxtime = 0 will allow our algorithm to run until completion with no time limit

inspect(sort(rules, by = 'lift'))

plot(rules, method = 'graph')

plot(rules, method = 'grouped', max = 25)











# 
# 
# getwd()
# 
# # Support and confidence values
# #supportLevels <- c(0.1, 0.05, 0.01, 0.005)
# #confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
# 
# getwd()
# 
# supportLevels <- c(0.006, 0.008)
# confidenceLevels <- c(0.25, 0.50)
# 
# 
# # Empty integers 
# rules_sup10 <- integer(length=9)
# rules_sup5 <- integer(length=9)
# rules_sup1 <- integer(length=9)
# rules_sup0.5 <- integer(length=9)
# 
# # Apriori algorithm with a support level of 10%
# for (i in 1:length(confidenceLevels)) {
#   
#   rules_sup10[i] <- length(apriori(customers_products, parameter=list(sup=supportLevels[1], 
#                                                          conf=confidenceLevels[i], minlen = 2, target="rules")))
# }
# 
# # Apriori algorithm with a support level of 5%
# for (i in 1:length(confidenceLevels)){
#   
#   rules_sup5[i] <- length(apriori(customers_products, parameter=list(sup=supportLevels[2], 
#                                                         conf=confidenceLevels[i], minlen = 2, target="rules")))
#   
# }
# 
# # # Apriori algorithm with a support level of 1%
# # for (i in 1:length(confidenceLevels)){
# #   
# #   rules_sup1[i] <- length(apriori(customers_products, parameter=list(sup=supportLevels[3], 
# #                                                         conf=confidenceLevels[i], minlen = 3, target="rules")))
# # }
# # 
# # 
# # # Apriori algorithm with a support level of 0.5%
# # for (i in 1:length(confidenceLevels)){
# #   
# #   rules_sup0.5[i] <- length(apriori(customers_products, parameter=list(sup=supportLevels[4], minlen = 3,
# #                                                           conf=confidenceLevels[i], target="rules")))
# #  
# # }
# 
# setwd("/Users/nselvarajan/Desktop/R/Assignment4")
# getwd()
# library(qplot)
# # Number of rules found with a support level of 10%
# plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
#                xlab="Confidence level", ylab="Number of rules found", 
#                main="Apriori with a support level of 10%") +
#   theme_bw()
# 
# ggsave(plot=plot1, filename="gtest.png", width=4, height=4)
# 
# # Number of rules found with a support level of 5%
# plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
#                xlab="Confidence level", ylab="Number of rules found", 
#                main="Apriori with a support level of 5%") + 
#   scale_y_continuous(breaks=seq(0, 10, 2)) +
#   theme_bw()
# 
# 
# 
# 
# 
# # Subplot
# grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
