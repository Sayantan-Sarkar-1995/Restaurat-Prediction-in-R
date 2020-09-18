getwd()

restaurant_train = read.csv("D:/R/Assignment/Intellipaat/Case Study for Restaurant Prediction/CSV files/train.csv")
restaurant_test = read.csv("D:/R/Assignment/Intellipaat/Case Study for Restaurant Prediction/CSV files/test.csv")

ncol(restaurant_test)
ncol(restaurant_train)

#As there is no revenue column in Test data, hence it is assigned implicitly with 1
restaurant_test$revenue = 1
View(restaurant_test)

restaurant = rbind(restaurant_train, restaurant_test)
nrow(restaurant)

#To check the Column Names of Dataset
colnames(restaurant)

#To check the Dataset in detailed format
summary(restaurant)

#To check whether there is any Null Value in the Dataset
table(is.na(restaurant))

#To check the datatypes of the value present in the Dataset
str(restaurant)

#Extracting Year from Date in 'Open.Date' column
restaurant$Open.Date = as.Date(restaurant$Open.Date , '%m/%d/%Y')
restaurant$Open.Year = as.numeric(format(restaurant$Open.Date , '%Y'))
this_year = as.Date('01/01/2020', '%m/%d/%y')
restaurant$Age = as.numeric(this_year - restaurant$Open.Date)
restaurant$Age = restaurant$Age / 1000

head(restaurant$Open.Year)
str(restaurant$Age)

#Assigning the 'City' column with 'other' value where the 'City.Group' value is 'Other'

restaurant$City[restaurant$City.Group == 'Other'] = 'Other'

table(restaurant$City) #To check the number of City present in the table
unique(restaurant$City)
#Replacing the Least Count of City with the Second Least City name
restaurant$City[restaurant$City == unique(restaurant$City)[4]] = restaurant$City[2]

restaurant$City = as.factor(restaurant$City)
restaurant$City.Group = as.factor(restaurant$City.Group)

#Consolidating the Type that has least occurrence with More occurred data
table(restaurant$Type)
restaurant$Type[restaurant$Type == 'MB'] = 'FC'
restaurant$Type[restaurant$Type == 'DT'] = 'IL'

restaurant$Type = as.factor(restaurant$Type)

#Plotting the Training Dataset in a Histogram to look for the skewness
hist(restaurant_train$revenue) #It is right skewed data

#Performing log transformation over the training set to get the data in normally distributed form
hist(log(restaurant_train$revenue + 1))

#Log Transformation of Revenue
restaurant$revenue = log(restaurant$revenue + 1) 

###Log Transformation of P-variables in the dataset
library(dplyr)

#Storing the values of P in a variable
temp = (restaurant %>% select(starts_with("P")))
head(temp)

#Applying Log Transformation on the P-variables
temp = log(temp + 1)
head(temp)
#Assigning the log Transformed value to the dataframe
restaurant[c(6:42)] <- temp

#Removing unwanted column in our dataset
restaurant = subset(restaurant, select = -c(Id, Open.Date, Open.Year))
#Pointing out the Important Variable
library(Boruta)
restaurant_important = Boruta(revenue ~ ., data = restaurant[1:137 , ])
decision_Boruta = restaurant_important$finalDecision
decision_Boruta = as.character(decision_Boruta)
cbind(colnames(restaurant), decision_Boruta)

#Applying Ensemble Model on the Dataset for prediction
restaurant_train_sample = restaurant[1: 137, c(decision_Boruta != 'Rejected')]
restaurant_test_sample = restaurant[138: nrow(restaurant), c(decision_Boruta != 'Rejected')]
library(party)
library(randomForest)
library(caret)

model = cforest(revenue ~ ., data = restaurant_train_sample, 
                controls = cforest_unbiased())
summary(model)


caret_model = train(revenue ~ . , data = restaurant_train_sample,
                    method = 'rf')

print(caret_model)
plot(caret_model)

pred_restaurant = predict(caret_model, restaurant_test_sample)
restaurant$revenue[138 : nrow(restaurant)] = pred_restaurant

View(restaurant)
