# to avoid the scientific e notation in the results
options(scipen=999)

#Q.1
# Perform a one-sample t-test to compare the population mean glucose levels to the recommended level of 100 mg/dL
t.test(diabetes$Glucose, alternative = "two.sided",conf.level=.95, mu = 100)

#Q.2
# Subset the data by diabetes status
diabetes_no <- diabetes[diabetes$Outcome == 0,]
diabetes_yes <- diabetes[diabetes$Outcome == 1,]
sd(diabetes_no$Glucose)
sd(diabetes_yes$Glucose)
# Perform the 2 sample t-test
t.test(diabetes_no$Glucose, diabetes_yes$Glucose)
sd(diabetes_no)

#Q.3
# Set seed for reproducibility
set.seed(123)
# Split the data into training and testing sets
library(caret)
trainIndex <- createDataPartition(diabetes$Outcome, p = .8, list = FALSE, times = 1)
train <- diabetes[trainIndex,]
test <- diabetes[-trainIndex,]
# Fit the logistic regression model
q3_model <- lm(Outcome ~ Glucose + BMI + Insulin + Age, data = train)
summary(q3_model)
# Make predictions on the test set
pred = predict(q3_model, newdata = test, type = "response")
library(pROC)
roc(test$Outcome, pred)

plot(q3_model)
dev.off()


#Q.4
# Subset the data to include only women with diabetes
yes_db <- diabetes[diabetes$Outcome == 1 & diabetes$Pregnancies!=0 ,]
#new column with categories
yes_db$Preg <- cut(yes_db$Pregnancies, breaks = c(0,1,2,Inf), labels = c(1,2,3))
yes_db
table(yes_db$Preg)
# Conduct one-way ANOVA on BMI among women with diabetes who have had one, two, or three or more pregnancies
q4_model <- lm(BMI ~ Preg, data = yes_db)
anova(q4_model)