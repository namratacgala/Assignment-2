# Library
library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)

# Reading data set
StuPerform <- read.csv("StudentsPerformance.csv",header=T)

# Check for the any missing Data in the data sets
with(StuPerform,sum(is.na(gender)))
#[1] 0
with(StuPerform,sum(is.na(race.ethnicity)))
#[1] 0
with(StuPerform,sum(is.na(parental.level.of.education)))
#[1] 0
with(StuPerform,sum(is.na(lunch)))
#[1] 0
with(StuPerform,sum(is.na(test.preparation.course)))
#[1] 0
with(StuPerform,sum(is.na(math.score)))
#[1] 0
with(StuPerform,sum(is.na(reading.score)))
#[1] 0
with(StuPerform,sum(is.na(writing.score)))
# [1] 0 so no missing value in all the columns checked.

#Histogram to see the performance with frequency of students. 

hist(StuPerform$math.score)
# From the above graph it can be concluded that most of the students secured marks 
# between 50 to 80%. In histogram equally spaced breaks. 
# In this case, the height of a cell is equal to the number of observation falling in that cell.


hist(StuPerform$reading.score)
# From the above graph it can be concluded that most of the students secured marks 
# between 50 to 90%

hist(StuPerform$writing.score, main = "Histogram of Writing Score", xlab="Writing Score", ylab="No of Students")
# From the above graph it can be concluded that most of the students secured marks 
# between 50 to 80%

# Plotting,count as the y bar with all the parameters given to take basic idea
# Using ggplot package we can plot non-numeric value using count
ggplot(StuPerform, aes(x = gender, y = ..count..)) + geom_bar()
# That shows the count of female and male. female count is higher than males
ggplot(StuPerform, aes(x = race.ethnicity, y = ..count..)) + geom_bar()
# there are total 5 groups. Group C is being higher in count
ggplot(StuPerform, aes(x = parental.level.of.education, y = ..count..)) + geom_bar()
# Less students are belong to the parents who have master degree and bachelor degree
ggplot(StuPerform, aes(x = lunch, y = ..count..)) + geom_bar()
# number of count is less in free/reduced lunch but still its near to 40% of total data
ggplot(StuPerform, aes(x = test.preparation.course, y = ..count..)) + geom_bar()
# Around 350 Students out of 1000 have completed test preparation course
ggplot(StuPerform, aes(x = math.score, y = ..count..)) + geom_bar()
ggplot(StuPerform, aes(x = reading.score, y = ..count..)) + geom_bar()
ggplot(StuPerform, aes(x = writing.score, y = ..count..)) + geom_bar()


# Descriptive statistics
mean(StuPerform$math.score)
#66.089
mean(StuPerform$reading.score)
#69.169
mean(StuPerform$writing.score)
#68.054
#Average Marks in Reading and writing are higher compare to maths that shows that
#Students have command on languages compare to numbers.

median(StuPerform$math.score)
#66
median(StuPerform$reading.score)
#70
median(StuPerform$writing.score)
#69
# Median shows the mid value of the observation here median for reading score is 
# higher compare to math score

sd(StuPerform$math.score)
#15.16308
sd(StuPerform$reading.score)
#14.60019
sd(StuPerform$writing.score)
#15.19566
# Standard Deviation shows the range of the deviation from the average 
# Deviation in reading score is less than the others.

# By using summary function we can get the basic idea of minimum value and Maximum value
# with quartile 1 and 3 and Mean.
summary(StuPerform$math.score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   57.00   66.00   66.09   77.00  100.00 

summary(StuPerform$reading.score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#17.00   59.00   70.00   69.17   79.00  100.00 

summary(StuPerform$writing.score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   57.75   69.00   68.05   79.00  100.00 

#box plot to compare quantities or variables. From the boxplot we can vizualize the
# Median, Q1, Q3 and Outliers
head(StuPerform)
boxplot(StuPerform$math.score ~ StuPerform$gender)
# The above boxplot shows the math.score and performance gender wise where we can say
# Males have performed better than females and Even in Females outliers are near to zero in case of females
boxplot(StuPerform$math.score ~ StuPerform$gender, main = "math.score")
boxplot(StuPerform$math.score ~ StuPerform$gender, main = "math.score" , xlab = "gender", ylab = "math.score")
# Same Graph can be done with adding label to make is visually appealable

# Same parameters of box plot using ggplot package as well as using mapping and binwidth
ggplot(StuPerform, aes(as.factor(gender),math.score)) + geom_boxplot() + labs(x="gender", y="Math Score")
#Males have performed better but below graph gives complete picture that highest marks secured by females
ggplot(data = StuPerform, mapping = aes(x = math.score)) + 
  geom_freqpoly(mapping = aes(colour = gender), binwidth = 5)

# Parental.Level.of.education
boxplot(StuPerform$reading.score ~ StuPerform$parental.level.of.education)
boxplot(StuPerform$reading.score ~ StuPerform$parental.level.of.education, main = "Parental Education" , xlab = "Parental Level of Education", ylab = "Reading Score")
ggplot(StuPerform, aes(as.factor(parental.level.of.education),reading.score)) + geom_boxplot(fill="skyblue", col="red")
#Color function and Fill function is introduced to make it visually better
#We can conclude that parental level of education is affects the reading score.
#Parents who have studied till high school, the reading score is quite less compare to 
#Others with higher Outliers towards lower side
boxplot(StuPerform$math.score ~ StuPerform$parental.level.of.education)
par(bg="skyblue")
boxplot(StuPerform$math.score ~ StuPerform$parental.level.of.education, main = "Parental Education" , xlab = "Parental Level of Education", ylab = "Math Score", col = "red")

#comparison between Math score and writing Score
boxplot(StuPerform$math.score ~ StuPerform$writing.score)
#Math and writing linearly correlated with some deviations
boxplot(StuPerform$reading.score ~ StuPerform$writing.score)
#Reading and writing scores are highly correlated with less deviation.
# Correlation between the data
cor.test(StuPerform$reading.score,StuPerform$writing.score)
#cor 0.9545981 
cor.test(StuPerform$writing.score,StuPerform$math.score)
#cor 0.802642 
cor.test(StuPerform$math.score,StuPerform$reading.score)
#cor 0.8175797 
pairs(StuPerform[,(6:8)])
# pairs shows that there is prominent correlation between reading and writing score
# math.score and reading.score are quite less correlated


# Plotting with colored parameter in ggplot as well as box plot
ggplot(StuPerform, aes(x = writing.score, y = math.score, color = race.ethnicity)) + geom_point()
ggplot(StuPerform, aes(x = writing.score, y = math.score, color = race.ethnicity)) + geom_boxplot()
#geom box plot and ggplot gives higher clarity compare to scatter plot for the above graphs. 
ggplot(data = StuPerform) +
  geom_point(mapping = aes(x = race.ethnicity, y = math.score))
ggplot(data = StuPerform) + 
  geom_point(mapping = aes(x = race.ethnicity, y = math.score), alpha = 1 / 10)
#Even Mapping with the alpha function give better data visualization on above graph

ggplot(StuPerform, aes(x = reading.score, y = writing.score, color = test.preparation.course)) + geom_point()
ggplot(StuPerform, aes(x = reading.score, y = writing.score, color = test.preparation.course)) + geom_boxplot()
# Who has completed test preparation they have performed well in reading and writing score

ggplot(StuPerform, aes(x = reading.score, y = writing.score, colour = lunch)) + geom_point()
ggplot(StuPerform, aes(x = reading.score, y = writing.score, colour = lunch)) + geom_boxplot()
ggplot(data = StuPerform) + geom_count(mapping = aes(x = lunch, y = math.score))
#The students who has taken standard lunch has performed better in all three test.
#geom box plot and ggplot gives higher clarity compare to scatter plot for the above graphs. 
#EDA helps to use visualization and transformation to explore your data in a systematic way
