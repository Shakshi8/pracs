#tidyr package

#reshapes the layout of data or reorganizes it

library (tidyverse)
library(tidyr)

#consider following dataframe

country = c("IN", "USA","FR","ENG","JAP")
value1 = c(23,24,26,27,25)
value2 = c(22,25,29,26,27)
value3 = c(33,34,36,37,35)

df = data.frame(country,value1,value2,value3)

df
colnames(df)[2:4] <- c(2011, 2012, 2013)

df
#gather() or pivot_longer()
gather(df,"year", "GDP",2:4)

 #or

gather(df,key = "year",value = "GDP", 2:4)

df
df %>% pivot_longer(-country,names_to = "year", values_to = "GDP")

#another way
df %>% pivot_longer(cols = c(2:4),names_to = "year", values_to = "GDP")

#spread()  and pivot_wider()
#generates multiple columns from two columns
#each value in the "key" column becomes a column name
#each value in the "value" column becomes a cell in the new column

data("fish_encounters")

fish_encounters

str(fish_encounters)
View(fish_encounters)

fish_encounters %>% spread(station, seen)

fish_encounters %>% pivot_wider(names_from = station, values_from = seen)

#unite() and separate() functions
data(storms)

str(storms)

View(storms)

new_df <- unite(storms,"Date", year,month,day,sep = "-")
head(new_df)
View(new_df)

sep_df <- separate(new_df, Date, c("year", "month", "day"), sep = "-")

View(sep_df)

#Handling missing values

#install.packages("hflights")
library(hflights)
df <- hflights
sum(is.na(df))
#colnames(is.na(df))

sum(is.na(df$ArrDelay))

nrow(df)

ArrDelay_null <- which(is.na(df$ArrDelay == "NA"))

#replacing missing value by element below it

df1 <-df %>% fill(ArrDelay,.direction = "up")

sum(is.na(df1$ArrDelay))

ArrDelay_null <- which(is.na(df1$ArrDelay == "NA"))
#df1 <-df %>% fill(ArrDelay,down)

#drop the null value rows

df1 <- drop_na(df,ArrDelay)
sum(is.na(df1$ArrDelay))

