# Packages in R

#1. dplyr package

#provides some great, easy-to-use functions that are very handy 
#when performing exploratory data analysis and manipulation.

#filter, select, arrange, mutate, summarise, group by, slice, distinct,

#install.packages("dplyr")
library(dplyr)

#install.packages("hflights")
library(hflights)
#library(tidyr)

#hflights <- flight_df

head(hflights)
tail(hflights)
str(hflights)
#flight_df <- as_tibble(hflights) #gives local dataframe

#head(flight_df)

#filter : keeps the rows that match the given criteria

#base R approach
subdf2 <- hflights[hflights$UniqueCarrier == "AA",]

#dplyr approach
subdf21 <- filter(hflights,UniqueCarrier == "AA")


#pipe operator to pass dataframe to filter function
subdf3 <- hflights %>% filter(DayofMonth ==1 , UniqueCarrier == "UA")

#we can also use in operator to combine condition on same variable
subdf4 <- filter(hflights,ActualElapsedTime %in% c(60,70))

#2. Select : to pick the columns from dataframe

#base R approach
hflights[1:10, c("FlightNum","ArrDelay", "DepDelay")]

result <- select(hflights,FlightNum, ArrDelay, DepDelay)

#selecting range of columns using :
result <- select(hflights,Year:DepTime)

#selecting column names start with specific name
result <- select(hflights,starts_with("Day"))

result <- select(hflights,ends_with("Time"))

#apply filter on select to select speific rows

result <- filter(select(hflights,Year:DepTime), DepTime > 1300)

result <- filter(select(hflights,ends_with("Time")),ActualElapsedTime > 70)   

# same can be written using %>% operator

result <- hflights %>% 
              select(ends_with("Time")) %>%
                  filter( ActualElapsedTime > 70)


#3. Arrange: to reorder rows

# Select UniqueCarrier and DepDelay columns and display in the order of DepDelay

#base R Approach
result <- hflights[order(hflights$DepDelay), c("UniqueCarrier", "DepDelay")]


#dplyr approach

result <- hflights %>%
  select(UniqueCarrier,DepDelay) %>%
    arrange(DepDelay)


result <- hflights %>%
  select(UniqueCarrier,DepDelay) %>%
  arrange(desc (DepDelay))

#4. mutate : to add new column in the dataframe

#base R approach
#hflights$speed <- hflights$Distance / hflights$AirTime

#dplyr approach

hflights_new <- hflights %>%
  select(Distance,AirTime) %>%
      mutate(Speed = Distance/AirTime)

# summarise

result <- hflights %>%
  group_by(Dest) %>%
    summarise(avg_delay = mean(ArrDelay, na.rm = TRUE))

result <- hflights %>%
  summarise(avg_arr_delay = mean(ArrDelay),
            max_dep_delay = max(DepDelay, na.rm = TRUE),
            min_dep_delay = min(DepDelay, na.rm = TRUE))

sum(is.na(hflights))
df <- hflights
#finding which columns have null values
null_columns <- colnames(is.na(df))

null_columns

sum(is.na(hflights$Month))

null_rows_depdelay <- df %>% filter(is.na(DepDelay))

#index of null rows
null_rows_depdelay1  <- which(is.na(df$DepDelay))

#replace null values by average delay time
df <- df %>% mutate(DepDelay=replace(DepDelay,is.na(DepDelay),  median(DepDelay, na.rm = TRUE)))

#slice
#selects row by position

slice(hflights,1:10)

#rename:  to rename the column

head(hflights_new %>%
  rename(speed_new = Speed))

#distinct gives distinct values in column
hflights %>% distinct(Dest)

hflights %>% distinct(Dest)%>% count()

