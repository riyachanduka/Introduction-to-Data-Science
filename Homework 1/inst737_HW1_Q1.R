#New York Times Dataset
#Q1
#First I will read the dataset into variable d
d=read.csv("C:/Users/riyac/Documents/all_nyt/nyt1.csv",header=TRUE)

#Part 1a) Creating a new variable age_group from Age variable using cut function
age_group=cut(d$Age,c(0,18,25,35,45,55,65,Inf),right = FALSE,labels=c("<18","18-24","25-34","35-44","45-54","55-64","65+"))
d$age_group=age_group
summary(d)

#Part 1b)(i) Plotting the number of impressions and click through rate for the 6 different age groups
library(ggplot2)
no_of_Imp_plot=ggplot(d, aes(x=Impressions, colour=age_group,fill=age_group)) + geom_histogram(position = "dodge",binwidth = 1)+ggtitle("Number of Impressions according to age group")
no_of_Imp_plot
d$crt=d$Clicks/d$Impressions
summary(d$crt)
d$crt[is.na(d$crt)]=0
ctr_plot=ggplot(d, aes(x=crt, colour=age_group,fill=age_group)) + geom_histogram(position = "dodge",binwidth = 0.1)+ggtitle("Click Through Rate according to age group")
ctr_plot

#Part 1b)(ii) Creating a new variable have_clicked which categorizes users on their click behavior
summary(d$Clicks)
table1=prop.table(table(d$Clicks))*100
d$have_clicked <- ifelse(d$Clicks>=1, 1, 0)

#Part 1b)(iii) Exploring the data and making visual and quantitative comparisons across user segments / demographics
#Creating new table with only required values
newdata <- d

#newdata$age_group <- newdata$CTR_cat <- NULL

#Recategorizing Age into <18 and >=18
newdata$age_group <- cut(d$Age, c(0,18,Inf),right = FALSE,labels = c("<18",">=18"))

#Encoding '0' and '1' to 'Female' and 'Male' respectively
newdata$Gender <- ifelse(newdata$Gender == 0, 'Female',
                         'Male')

#Creating tables with mean values for Impressions, Clicks and CTR
Impressions.table <- aggregate(newdata$Impressions,
                               by = list(newdata$Gender, newdata$age_group),
                               FUN = mean, na.rm = TRUE)
Clicks.table <- aggregate(newdata$Clicks,
                          by = list(newdata$Gender, newdata$age_group),
                          FUN = mean, na.rm = TRUE)
CTR.table <- aggregate(newdata$have_clicked,
                       by = list(newdata$Gender, newdata$age_group),
                       FUN = mean, na.rm = TRUE)
#Plot to see frequency distribution of CTR for Males and Females in two age categories
myplot <- ggplot(newdata,
                 aes(Clicks, fill = Gender,
                     color = Gender))
#From the plot ->
#Clicks by gender and age group, day 1
#1. Most Males and Females have clicks<=1
#2. Change in No of clicks value from <18 years to >=18 years is more for males than females 
myplot + geom_histogram(position = "dodge", binwidth = 1) + facet_grid(.~ age_group) 

#Now checking if the data is normally distributed for those who have clicks 
prop.table(table(as.factor(d$have_clicked)))
table2=table(d$have_clicked,d$age_group) 
ClkA <- prop.table(table(d$age_group, d$have_clicked==1))
ClkA <- data.frame(ClkA)
ClkA <- subset(ClkA, Var2==TRUE)            
ClkA$Var2 <- NULL           
s <- sum(ClkA$Freq)
ClkA$RelFrq <- ClkA$Freq/s 
library(data.table)
labl <- data.table(ClkA)[, per := sprintf("%.1f%%", RelFrq*100), by = Var1]
#The data is not normally distributed.The <18 group makes up almost half of all clicks for day 1
ggplot(ClkA, aes(x=Var1, y=RelFrq, fill=Var1))+geom_bar(stat="identity")+labs(title="1+ Clicks by Age Group", x= "Age Groups", y= "Relative Frequency of Clicks")+ guides(fill=FALSE)+geom_text(data = labl, aes(x = Var1, y = RelFrq, label = per), vjust= -0.5)

#Q1c)Now extending the analysis across Days
nyt1 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt1.csv",header = TRUE)
nyt2 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt2.csv",header = TRUE)
nyt3 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt3.csv",header = TRUE)
nyt4 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt4.csv",header = TRUE)
nyt5 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt5.csv",header = TRUE)
nyt6 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt6.csv",header = TRUE)
nyt7 <- read.csv("C:/Users/riyac/Documents/all_nyt/nyt7.csv",header = TRUE)

nyt1$day <- "day1"
nyt2$day <- "day2"
nyt3$day <- "day3"
nyt4$day <- "day4"
nyt5$day <- "day5"
nyt6$day <- "day6"
nyt7$day <- "day7"

nyt <- rbind(nyt1, nyt2, nyt3, nyt4, nyt5, nyt6, nyt7)
levels(nyt$day) <- list('day1' = 'day1',
                        'day2' = 'day2',
                        'day3' = 'day3',
                        'day4' = 'day4',
                        'day5' = 'day5',
                        'day6' = 'day6',
                        'day7' = 'day7')

nyt$Gender <- ifelse(nyt$Gender == 0, 'Female','Male')

nyt$CTR <- ifelse(nyt$Impressions != 0, nyt$Clicks/nyt$Impressions,NA)
#Part Q1c)(i) Create metrics/measurements/statistics that summarize the data
#Comparing mean number of clicks/person for both genders across 7 days
clicks.plot2 <- aggregate(nyt$Clicks,
                          by = list(nyt$day, nyt$Gender),
                          FUN = mean,
                          na.rm = TRUE)
#From table -> 
#1. Average number of clicks/ person for males and females separately, is almost same from day1 to day7  
#2. Overall Females have a higher average number of clicks/ person  
clicks.plot2

#Comparing mean CTR for both genders across 7 days
ctr.plot2 <-aggregate(nyt$CTR,
                      by = list(nyt$day, nyt$Gender),
                      FUN = mean, na.rm = TRUE)
#From table -> 
#1. Average CTR/ person for males and females separately, is almost same from day1 to day7  
#2. Overall Females have a higher number of CTR/ person  
ctr.plot2

#Part Q1c)(ii) Plotting distribution of CTR for Males and Females for Day1 to Day7
#From plot -> 
#Total Clicks remains almost same for all days except day5 and day6. On day5 it is lower than normal and on day6 it is higher than normal. This behaviour is common for both males and females
agg <- ggplot(nyt, aes(day, Clicks))
agg + geom_bar(stat = "identity") + facet_grid(.~ Gender)

#Part Q1d)Patterns found
#From Clicks table in Q1b(iii)
#Males <18 years have slightly greater CTR value than Females <18 years
#Females >=18 years have slightly greater CTR value than males >=18

#From plot in Q1b(iii)
#1. Most Males and Females have clicks<=1
#2. Change in No of clicks value from <18 years to >=18 years is more for males than females 

#From table in Q1c)
#1. Average number of clicks/person and CTR/ person for males and females separately, is almost same from day1 to day7  
#2. Overall Females have a higher number of CTR/ person  and no of clicks/person

#From plot in Q1c)
#This dataset is from May 2012. From the calendar we can see that day 1 corresponds to a Tuesday, day 2 to Wednesday and similarly day 7 to Monday. 
#a. Total number of clicks for day 5 and day 6 is dissimilar from the other days.
#b. On day 5, it is lower than expected and on day 6 it is much higher than expected. This can be attributed to the fact that day 6 corresponds to Sunday for May 2012, and more number of people use internet and click on ads. 
