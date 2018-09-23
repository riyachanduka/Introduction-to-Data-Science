## Mass Shooting in US Dataset Exploration
#Mass Shootings in the United States of America (1966-2017) The US has witnessed 398 mass shootings in last 50 years that resulted in 1996 deaths and 2488 injured. The latest and the worst mass shooting of October 2, 2017 killed 58 and injured 515 so far. The number of people injured in this attack is more than the number of people injured in all mass shootings of 2015 and 2016 combined. The average number of mass shootings per year is 7 for the last 50 years that would claim 39 lives and 48 injured per year.

#Q2a) Analyzing Data
#installing necessary modules
library(data.table)
library(readr)
library(plotly)
library(ggplot2)
library(maps)
library(tm)
library(wordcloud)

#Loading data from source. Extract the year and month from the date and combine the variables of gender
MS_dataset<- read_csv("C:/Users/riyac/Documents/Mass_Shootings_Dataset_Ver_5.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
MS_dataset <- data.table(MS_dataset)
summary(MS_dataset)
MS_dataset[,Month:=as.factor(month(Date))]
MS_dataset[,Year_n:=as.numeric(year(Date))]
MS_dataset[,Year:=as.factor(year(Date))]

MS_dataset[Gender=='M',Gender:="Male"]
MS_dataset[Gender=='M/F',Gender:="Male/Female"]
MS_dataset[is.na(Gender),Gender:="Unknown"]
MS_dataset[,Gender:=as.factor(Gender)]

#2.1 Number of total victims by years
plot_ly(data = MS_dataset
        ,type = 'bar'
        ,mode = 'markers' 
        ,hoverinfo = 'text'
        ,x = ~Year
        ,y = ~ `Total victims` 
        ,color = 'Red'
        ,alpha = 0.9
        ,text = ~paste(
          'Fatalities : ', Fatalities
          ,'\n Injured : ', Injured
        )) %>% 
  layout(title = "Number of Total victims by years"
         , xaxis = list(title = "")
         , yaxis = list(title = "Number of victims"))

#2.2 Number of incidents by years
plot_ly(data = MS_dataset
        ,type = 'histogram'
        ,mode = 'markers'
        ,x = ~Year
        ,alpha = 0.9) %>% 
  layout(title = "Number of incidents by years"
         , xaxis = list(title = "")
         , yaxis = list(title = "Number of incidents"))

#2.3 Number of incidents by month
plot_ly(data = MS_dataset
        ,type = 'histogram'
        ,mode = 'markers'
        ,x = ~Month
        ,alpha = 0.9) %>% 
  layout(title = "Number of incidents by month"
         , xaxis = list(title = "Month")
         , yaxis = list(title = "Number of incidents"))


#2.4 Pie chart by Mental Health Issues of the shooter
MS_dataset[`Mental Health Issues`=="unknown",`Mental Health Issues`:="Unknown"]

# set colors for first pie chart
colors_pie1 <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

plot_ly(data = MS_dataset[,.(`Total victims`,`Mental Health Issues`)]
        ,type = 'pie'
        ,labels = ~`Mental Health Issues`
        ,values = ~`Total victims`
        ,textposition = 'inside'
        ,insidetextfont = list(color = '#FFFFFF')
        ,marker = list(colors = colors_pie1,
                       line = list(color = '#FFFFFF', width = 1)))%>%
  layout(title = "Mental Health Issues",
         showlegend = T)



MS_dataset$State <- sapply(MS_dataset$Location, function(x){
  temp <- strsplit(x, split = ",")
  sapply(temp, function(y){y[2]
    
  })
})

#2.5 Pie chart with Number of incidents by States
plot_ly(data = MS_dataset[!is.na(State),.('Number of incidents'= uniqueN(`S#`)),by=State]
        ,type = 'pie'
        ,labels = ~State
        ,values = ~`Number of incidents`
        ,textposition = 'inside'
        ,insidetextfont = list(color = '#FFFFFF')
        ,marker = list(colors = colors_pie1,
                       line = list(color = '#FFFFFF', width = 1)))%>%
  layout(title = "Number of incidents by States",
         showlegend = T)

#2.6 Bar plot with Total victims by Years and Race
# Clearing and merging data in the Race field
MS_dataset[Race=="unclear",Race:="Unknown"]
MS_dataset[is.na(Race),Race:="White"]

MS_dataset[Race=="Black American or African American" 
           | Race=="black"
           | Race=="Black American or African American/Unknown"
           ,Race:="Black"]

MS_dataset[Race=="White American or European American"
           | Race=="White American or European American/Some other Race" 
           | Race=="white"
           ,Race:="White"]

MS_dataset[Race=="Asian American"
           | Race=="Asian American/Some other race" 
           ,Race:="Asian"]

MS_dataset[Race=="Unknown",Race:="Other"]
MS_dataset[Race=="Two or more races",Race:="Other"]
MS_dataset[Race=="Some other race",Race:="Other"]
MS_dataset[Race=="Native American or Alaska Native",Race:="Native American"]
plot_ly(data = MS_dataset[,.('Total victims'= sum(`Total victims`)),by=.(Race,Year)]
        ,type = 'bar'
        ,mode = 'markers'
        ,x = ~Year
        ,y = ~`Total victims`
        ,color =~Race
        ,alpha = 0.9) %>% 
  layout(title = "Total victims by Race"
         , showlegend = T
         , barmode = 'stack'
         , position = 1
         , xaxis = list(title = "")
         , yaxis = list(title = "")
         , legend = list(x = 0, y = 1)
         , hovermode = 'compare')


#2.7 Total victims & Fatalities on US map
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill="white")

p <- 
  p + geom_point(data=MS_dataset[Longitude >=-140,]
                 , aes(x=Longitude, y=Latitude
                       ,size = `Total victims`
                       ,color = Fatalities)
                 ,alpha = 0.6) + 
  scale_color_gradient(low = "red", high = "black") + 
  ggtitle("Total victims & Fatalities on US map")


ggplotly(
  p
)

#Q2b) How was data made usable?
#1. Clearing and merging data in the Race field 
#2. Location was split into State and City
#3. No missing values for Location, Latitude and Longitude

