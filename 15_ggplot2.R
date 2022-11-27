#Data visualization in R

# R has 3 plotting systems
#1. Base plotting system
    #scatter plot
    #Histogram
    #Boxplot
#2. Lattice plotting system
#3. GGplot2 


#Base plotting system

#iris dataset
data("iris")
View(iris)

table(iris$Species)

#scatterplot: gives the idea of bivariate analysis
#i.e. how one variable changes with respect to changes in another variable.

plot(iris$Sepal.Length ~ iris$Petal.Length) #variable on left side of ~ is y axis 
#variable on right side of ~ is X axis

#adding labels for X axis and Y axis

plot(iris$Sepal.Length ~ iris$Petal.Length,xlab="Petal Length", ylab = "Sepal Length", main="Sepal Length vs. Petal Length")

#adding color and shape to plotting character

plot(iris$Sepal.Length ~ iris$Petal.Length,xlab="Petal Length", ylab = "Sepal Length", main="Sepal Length vs. Petal Length", col="blue",pch=20)


#Histogram  :  Univariate Analysis

hist(iris$Sepal.Length, xlab = "Sepal Length", ylab = "Frequency", main = "Histogram of Sepal Length", col = "tomato")

#to know different colors provided by R plotting system
colors()

#Boxplot:  How a continuous variable changes with categorical variable

boxplot(iris$Sepal.Length ~iris$Species, xlab = "Species", ylab = "Sepal Length", main="Sepal Length of Species", col="rosybrown2")

----------------------------------------------------------------------------------------------------------------------------------------------
  
  #ggplot2
  
  #gg stands for grammer of graphics
  
  #components of graphics:
  #1. Data : The dataset being summarized
  #2. Aesthetics: Variables mapped to visual cues, such as x-axis and y-axis values and color, shape, size etc.
  #3. Geometry: The type of plot (scatterplot, boxplot, barplot, histogram etc.)
  #4. Facets: Groups by which we divide the data or allows multiple plots on single canvas
  #5. Statistics: allows to add statistics to graph
  #6. Coordinates: allows to limit x axis and y axis
  #7. Theme: font size, coloring background, grid lines etc.


#data("iris")

str(iris)
#install.packages("ggplot2")
library(ggplot2)

ggplot(data=iris)

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))

ggplot(data=iris, aes(x= Petal.Length))+ geom_histogram()

#you can also write as

pl <- ggplot(data = iris,aes(x = Petal.Length))

pl <- pl + geom_histogram(bins = 15)

print(pl)

print(ggplot(data = iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.15))

print(ggplot(data = iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.15,color = 'red'))

print(ggplot(data = iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.15,color = 'red', fill = 'pink'))

#transparency
print(ggplot(data = iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.15,color = 'red', fill = 'pink', alpha = 0.4))


#Adding labels

pl <- ggplot(data = iris, aes(x = Petal.Length))

#print(pl)

#adding title

 pl <- pl + ggtitle("Histogram of Petal Length")
             
pl <- pl + geom_histogram(binwidth = 0.15,color = 'red', fill = 'pink', alpha = 0.4)

pl <- pl + xlab("Petal length") + ylab("Count")

print(pl)

pl <- ggplot(data = iris, aes(x = Petal.Length))

#filling with respect to count

pl <- pl + geom_histogram(binwidth = 0.15,aes(fill = ..count..))

print(pl)


#scatter plot

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))+ geom_point()

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))+ geom_point(col="red")

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))+ geom_point(col="red", alpha = 0.5, size = 5)

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))+ geom_point(col="red", alpha = 0.5, aes(size = Petal.Length))


ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length, color= Species))+ geom_point()

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length))+ geom_point(aes(color = Species), alpha = 0.5, size = 5)

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length, shape= Species))+ geom_point()

ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length, color= Species, shape= Species))+ geom_point()

p1 <- ggplot(data=iris, aes(x= Petal.Length, y=Sepal.Length,  shape= Species))+ geom_point(aes(color= Sepal.Length,size = 2))

p1<- p1 + scale_color_gradient(low = 'purple', high = 'red')

#scale_color_gradient(low = "#132B43", high = "#56B1F7")


print(p1)


# Bar plot

data("mpg")

str(mpg)

View(mpg)

ggplot(mpg,aes(x = class)) + geom_bar(fill = "red")


#stacked barplot -> filling with some categorical variable

ggplot(mpg,aes(x = class)) + geom_bar(aes(fill = drv))

#side by side bar charts using position argument

ggplot(mpg,aes(x = class)) + geom_bar(aes(fill = drv), position = 'dodge')


#Boxplot

data("mtcars")

str(mtcars)

View(mtcars)

ggplot(mtcars,aes(x = cyl,y = mpg)) + geom_boxplot()

ggplot(mtcars,aes(x = factor(cyl),y = mpg)) + geom_boxplot()


ggplot(mtcars, aes(x = factor(gear),y = mpg)) + geom_boxplot() + coord_flip()

ggplot(mtcars,aes(x = factor(cyl),y = mpg)) + geom_boxplot(fill = "green")

ggplot(mtcars,aes(x = factor(cyl),y = mpg, fill = factor(cyl))) + 
  
  geom_boxplot() + theme_dark()


#Adding coordinate limit

pl <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

print(pl)


pl <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()+
  coord_cartesian(xlim = c(3,6), ylim = c(15,30))

print(pl)

#fixing aspect ratio:  by default it is 1 hence we see square

pl <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()+
  coord_fixed(ratio = 1/2, xlim = c(3,6), ylim = c(15,30))

print(pl)


#creating facet grid

pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(.~ cyl)

print(pl)


pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(drv ~ .)

print(pl)


pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(drv ~ cyl)

print(pl)


#adding themes

pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(. ~ cyl) + theme_bw()

print(pl)


#setting theme for all plots

theme_set(theme_gray())

pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(. ~ cyl)

print(pl)


pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(drv ~ .)

print(pl)

# we can use additional themes from ggthemes package

#install.packages("ggthemes")
library(ggthemes)

pl <- ggplot(mpg,aes(displ,hwy)) + geom_point()+
  facet_grid(drv ~ .)

print(pl) +theme_economist()



df=read.csv("D:/Jan May 2020/R labs/Housing.csv")

# Format
# A dataframe containing :
#   
#   price
# sale price of a house
# 
# lotsize
# the lot size of a property in square feet
# 
# bedrooms
# number of bedrooms
# 
# bathrms
# number of full bathrooms
# 
# stories
# number of stories excluding basement
# 
# driveway
# does the house has a driveway ?
#   
#   recroom
# does the house has a recreational room ?
#   
#   fullbase
# does the house has a full finished basement ?
#   
#   gashw
# does the house uses gas for hot water heating ?
#   
#   airco
# does the house has central air conditioning ?
#   
#   garagepl
# number of garage places
# 
# prefarea
# is the house located in the preferred neighbourhood of the city ?





str(df)

View(df)

ggplot(data=df,aes(x=price))+ geom_histogram(bins=50)

ggplot(data=df,aes(x=price))+ geom_histogram(bins=30, fill="palegreen3")

ggplot(data=df,aes(x=price))+ geom_histogram(bins=30, fill="palegreen3",color="green")

ggplot(data=df,aes(x=price, fill=airco))+ geom_histogram(bins=30, color="green")

#ggplot(data=df,aes(x=price, fill=airco))+ geom_histogram(bin=30,position = "fill")

#Bar plot

#when to use histogram and when to use bar plot
#bar plot is to be used to show distribution of categorical variable w.r.t. continuous variable
#histogram is used to show frequency distribution of contnious variable
str(df)

ggplot(data=df, aes(x=airco))+geom_bar()

ggplot(data=df, aes(x=gashw,fill=airco))+geom_bar()

ggplot(data=df, aes(x=gashw,fill=airco))+geom_bar(position="fill")

#frequency polygon


ggplot(data=df,aes(x=price))+ geom_freqpoly()

ggplot(data=df,aes(x=price))+ geom_freqpoly(bins=20) #changing the bin no. changes the variation

ggplot(data=df,aes(x=price,col=airco))+ geom_freqpoly(bins=60)

