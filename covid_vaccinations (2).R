# Installing required packages
install.packages("tidyverse") # For dplyr and ggplot2
install.packages("zoo") # For dealing with NA values
install.packages("splitstackshape") # To split columns (csplit)
install.packages("caTools") # For splitting the dataset into train and test
install.packages("ggforce") # for 'geom_arc_bar' in pie chart

# Load required packages
library(tidyverse)
library(zoo)
library(splitstackshape)
library(caTools)
library(ggforce) 

# Covid Vaccination Progress
# https://www.kaggle.com/gpreda/covid-world-vaccination-progress
#vaccine_data <- read.csv("C:/Users/user/Desktop/Prem/College/Sem 8/R/project/Covid-Vaccination-Progress/country_vaccinations.csv")
vaccine_data <- read.csv("C:/Users/prana/Desktop/R Mini project/country_vaccinations.csv")
View(vaccine_data)

# Exploring the Dataset
ncol(vaccine_data)
nrow(vaccine_data)
dim(vaccine_data)
head(vaccine_data)
tail(vaccine_data)
str(vaccine_data)
summary(vaccine_data)

# Drop unnecessary columns - iso code, source name and source website
vaccine_data = subset(vaccine_data, select = -c(iso_code, source_name, source_website))
colnames(vaccine_data)

# Exploratory analysis 

# Unique countries in the dataset
vaccine_data%>%distinct(country)
# Number of countries included in the dataset
vaccine_data%>%distinct(country)%>%count()  


sputnik_count <- vaccine_data %>% filter(vaccines %in% c("Sputnik V"))
sputnik_count%>%distinct(country) #shows names of countries with Sputnik vaccines
sputnik_count%>%distinct(country)%>%count() #count of those countries

# Data visualization

dataplot <- vaccine_data %>% 
  select(country, vaccines) %>% group_by(country) %>%
  cSplit("vaccines", 
         ",", 
         direction="long") 
dataplot = unique(dataplot)

# Bar Plot
ggplot(dataplot, aes(x = vaccines, fill = vaccines)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Vaccine") + ylab("Number of Countries") +
ggtitle('Number of Countries using a particular Vaccine')

# Calculating percentage share of each vaccine
dataplot <- 
  dataplot %>%
  group_by(vaccines) %>%
  summarise(total = n()) %>%
  mutate(percentage = round(total/sum(total)*100, 2),
         end = 2 * pi * cumsum(total)/sum(total),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

ggplot(dataplot) + ggtitle("Percentage Share of each Vaccine") +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = vaccines)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = paste0(percentage,"%"),
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.5),
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1.1), 
                     name = "", breaks = NULL, labels = NULL)


# Plotting the Global Vaccination Trends wrt monthly vaccination count

# Make a copy with all NA values = 0 (easier for plotting)
vaccine_data_copy <- vaccine_data
vaccine_data_copy[is.na(vaccine_data_copy)] = 0
vaccine_data_copy$date = as.Date(vaccine_data_copy$date)

dataplot <- vaccine_data_copy %>%
  select("date","people_vaccinated","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise(people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated))
  
ggplot(dataplot) +
  geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1) +
  geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Date",y ="Vaccination count", title  = "Global Vaccination Trends", col=element_blank()) +
  theme_bw() +
  scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))


# Plotting the Indian Vaccination Trends wrt monthly vaccination count
india_vaccine_data <-  vaccine_data_copy %>%
  filter(country %in% c("India"))

indiaplot <- india_vaccine_data %>%
  select("date","people_vaccinated","people_fully_vaccinated") %>%
  group_by(date) %>%
  summarise (people_vaccinated=sum(people_vaccinated), people_fully_vaccinated=sum(people_fully_vaccinated))

ggplot(indiaplot) +
  geom_line(aes(x=date, y=people_vaccinated, col="1st Dosage"), size = 1) +
  geom_line(aes(x=date, y=people_fully_vaccinated, col="2nd Dossage"), size=1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Date",y ="Vaccination count", title  = "India Vaccination Trends", col=element_blank()) +
  theme_bw() +
  scale_color_manual(values = c('1st Dosage' = '#3366CC','2nd Dossage' = '#FF9933'))

# Plotting the trend line of vaccinations
india_plot <- india_vaccine_data %>%
  select("date","people_fully_vaccinated")%>%
  group_by(date) %>%
  summarise (people_fully_vaccinated=sum(people_fully_vaccinated) / 1000000)


ggplot(india_plot, aes(x = date, y = people_fully_vaccinated)) +
  geom_smooth(color="red") +
  ggtitle('Trend in Vaccinations in India')+
  labs(x="Months",y ="Vaccination count in millions", col=element_blank())


# Pre Processing:

colnames(vaccine_data)
# Creating dataframe for Indian Vaccination Statistics
india_vaccine_data <- vaccine_data %>%
  filter(country %in% c("India" )) %>%
  select("date", "total_vaccinations", "people_vaccinated", "daily_vaccinations")
View(india_vaccine_data)

# Check which rows and cols have null values
sum(is.na(india_vaccine_data))
which(is.na(india_vaccine_data), arr.ind=TRUE)

# Smooth out NA Values 

# Vector of column names which have NA values
na.cols <- names(which(colSums(is.na(india_vaccine_data)) > 0))
na.cols

# Smooth out the NA values using na.approx()
india_vaccine_data[, na.cols] <- na.approx(india_vaccine_data[, na.cols], na.rm="FALSE")

sum(is.na(india_vaccine_data))
colnames(india_vaccine_data)

# Now make them as 0
names(which(colSums(is.na(india_vaccine_data)) > 0))
india_vaccine_data[is.na(india_vaccine_data)] = 0
sum(is.na(india_vaccine_data))


# Change date to days from vaccination campaign began [1,2,3...]
indian_vaccine_data_per_day <- india_vaccine_data
indian_vaccine_data_per_day$date <- seq.int(nrow(indian_vaccine_data_per_day))
View(indian_vaccine_data_per_day)

# Splitting Dataset into Train and Test

set.seed(72)
sample <- sample.split(indian_vaccine_data_per_day,SplitRatio = 0.7)
train <- subset(indian_vaccine_data_per_day,sample == TRUE) 
test <- subset(indian_vaccine_data_per_day, sample == FALSE)

linear_regression_model <- lm(people_vaccinated ~ date + total_vaccinations + daily_vaccinations, data = train)
summary(linear_regression_model)

# Predicting
pred <- predict(linear_regression_model, test)
numx <- nrow(test)
x_axis <- seq(numx)
pred_df <- data.frame(x_axis, pred,test$people_vaccinated)

# Converting values to millions
pred_df[2:3] <- pred_df[2:3] / 1000000
test_df <- test$people_vaccinated / 1000000

# Plotting the predicted values against the actual values
pl <- ggplot(pred_df, aes(x=x_axis))
pl <- pl + geom_line(aes(y=pred, colour="Predicted"))
pl <- pl + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
pl <- pl + geom_line(aes(y=test_df, colour="Actual"))
pl <- pl + geom_point(aes(x=x_axis, y=test_df, colour="Actual"))
pl <- pl + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
pl <- pl + xlab("Test Samples") + ylab("People Vaccinated (In Millions)") + ggtitle("Model Evaluation")
pl

# Manually predicting, with 200M vaccinations and 2M daily vaccinations
new_pred <- data.frame(date=200, total_vaccinations=200000000, daily_vaccinations=2000000)
people_vaccinated <- predict(linear_regression_model, new_pred)
people_vaccinated
