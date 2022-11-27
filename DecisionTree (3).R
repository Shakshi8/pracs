# Installing required packages
install.packages("tidyverse")
library(tidyverse)
install.packages("caTools")
library(caTools)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
install.packages("titanic")
library(titanic)


titanic <- titanic_train

# Exploring the Dataset
colnames(titanic)
ncol(titanic)
nrow(titanic)
dim(titanic)
head(titanic)
tail(titanic)
str(titanic)
summary(titanic)

sum(is.na(titanic))

# Pre Processing
# Convert to factors:
str(titanic)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
str(titanic)


# Visualizations

survivals <- table(titanic$Survived) 
survivals

# How many survived
ggplot(titanic) +
  geom_bar(aes(x = Survived, fill = Survived))

pie(table(titanic$Survived), col = c(3,2)) 

ggplot(titanic,aes(x=Age,y=Fare,col=Survived)) + geom_point(aes(shape = Survived))
# Passengers with high fair survived more than passengers at low fair

ggplot(titanic,aes(x=Sex)) + geom_bar(aes(fill=Survived)) +
  ggtitle("Titanic Gender Survival Rate") +
  scale_x_discrete(name= "Passenger Gender") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))
# Women survived more than Men

# Age Distribution
ggplot(titanic) +
  geom_histogram(aes(x = Age),  color = "#355a63", fill = "#96e4f7") +
  ggtitle("Titanic Age Distribution") +
  scale_x_continuous(name= "Passenger Age", breaks = 5*c(0:18)) +
  scale_y_continuous(name = "Passenger Count")


ggplot(titanic) +
  geom_histogram(aes(x = Age, fill = Survived), color = "#000000") +
  ggtitle("Titanic Survival Rate by Age") +
  scale_x_continuous(name= "Passenger Age", breaks = 5*c(0:18)) +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))
# Ages between 30 - 35 did not survive the most. Children < 15 were mostly saved.


# How many survived based on Age across each passenger class
ggplot(titanic,aes(x=Age)) + geom_histogram(aes(fill=Survived)) +
  facet_wrap(~Pclass) + 
  ggtitle("Titanic Survival Rate by Age across Pclass") +
  scale_x_continuous(name= "Passenger Age") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))
# Passengers from Class 3 suffered more deaths


ggplot(titanic ,aes(x = Age,y = Sex)) +
  geom_jitter(aes(colour = Survived)) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass vs Sex vs Age vs Survived") +
  scale_x_continuous(name="Age",limits=c(0, 81))+
  scale_y_discrete(name= "Sex") +
  scale_color_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))
# Majority of Females from class 1 and class 2 survived
# Half of Men from class 1 survived


# Pre Processing

# Drop the following fields as they do not contribute to the decision tree (pruning):
# - PassengerID
# - Name
# - Ticket 
# - Cabin
# - SibSp
titanic <- select(titanic, Survived, Pclass, Sex, Age, Parch, Fare, Embarked)
str(titanic)

sum(is.na(titanic))

# Check which columns have null values
names(which(colSums(is.na(titanic)) > 0)) # Only Age column has NA Values

# Replace NA values in age with the mean
titanic$Age[which(is.na(titanic$Age))] <- mean(titanic$Age,na.rm = TRUE)

sum(is.na(titanic$Age))



# Splitting Dataset into Train and Test
set.seed(72)
sample <- sample.split(titanic,SplitRatio = 0.8)
train <- subset(titanic,sample == TRUE) 
test <- subset(titanic, sample == FALSE)

# Creating the model
model <- rpart(Survived ~ ., data = train, method = "class")
model
rpart.plot(model, cex = 0.8)

# Making the predictions on the test set based on the model
survived <- predict(model, test, type="class")

# Accuracy of the model
confusionMatrix = table(survived, test$Survived)
confusionMatrix(confusionMatrix)

# Alternative Decision Tree
model2 <- ctree(Survived ~ .,train)
model2
plot(model2, main = "Decision Tree")
