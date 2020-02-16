# Read data from csv files
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Check the first 5-6 rows for each dataset
head(ny)

head(wash)

head(chi)

## Extracting month/day/hour information from Start.Time and add new columns including month/day/hour information
## Crating functions to extract month/day/hour information and add new columns
fn.month = function(start.time){
    Start.Time.Month <- format(as.POSIXct(start.time, format = "%Y-%m-%d %H:%M:%S"),  "%m")
    return(Start.Time.Month)
}
fn.day = function(start.time){
    Start.Time.Day <- weekdays(as.Date(start.time))
    return(Start.Time.Day)
}
fn.hour = function(start.time){
    Start.Time.Hour <- format(as.POSIXct(start.time, format = "%Y-%m-%d %H:%M:%S"),  "%H")
    return(Start.Time.Hour)
}
# For New York
ny$Start.Time.Month <- fn.month(ny$Start.Time)
ny$Start.Time.Day <- fn.day(ny$Start.Time)
ny$Start.Time.Hour <- fn.hour(ny$Start.Time)
# For Washington
wash$Start.Time.Month <- fn.month(wash$Start.Time)
wash$Start.Time.Day <- fn.day(wash$Start.Time)
wash$Start.Time.Hour <- fn.hour(wash$Start.Time)
# For Chicago
chi$Start.Time.Month <- fn.month(chi$Start.Time)
chi$Start.Time.Day <- fn.day(chi$Start.Time)
chi$Start.Time.Hour <- fn.hour(chi$Start.Time)

## Creating plots for NYC
#Calling ggplot library
library(ggplot2)
# Creating plot showing how many travels happened for each month of year
ggplot(data=ny, aes(ny$Start.Time.Month))+
        geom_bar(stat="count", color='black',fill='palevioletred')+
        labs(x="Month of Year", y="Counts", title="Counts for each month of year - NYC")
# Creating plot showing how many travels happened for each day of week
ggplot(data=ny, aes(ny$Start.Time.Day))+
        geom_bar(stat="count", color='black',fill='skyblue')+
        labs(x="Day of Week", y="Counts", title="Counts for each day of week - NYC")
# Creating plot showing how many travels happened for each hour of day
ggplot(data=ny, aes(ny$Start.Time.Hour))+
        geom_bar(stat="count", color='black',fill='tomato')+
        labs(x="Hour of Day", y="Counts", title="Counts for each hour of day - NYC")

## Creating plots for Washington
# Creating plot showing how many travels happened for each month of year
ggplot(data=wash, aes(wash$Start.Time.Month))+
        geom_bar(stat="count", color='black',fill='palevioletred1')+
        labs(x="Month of Year", y="Counts", title="Counts for each month of year - Washington")
# Creating plot showing how many travels happened for each day of week
ggplot(data=wash, aes(wash$Start.Time.Day))+
        geom_bar(stat="count", color='black',fill='skyblue1')+
        labs(x="Day of Week", y="Counts", title="Counts for each day of week - Washington")
# Creating plot showing how many travels happened for each hour of day
ggplot(data=wash, aes(wash$Start.Time.Hour))+
        geom_bar(stat="count", color='black',fill='tomato1')+
        labs(x="Hour of Day", y="Counts", title="Counts for each hour of day - Washington")

## Creating plots for Chicago
# Creating plot showing how many travels happened for each month of year
ggplot(data=chi, aes(chi$Start.Time.Month))+
        geom_bar(stat="count", color='black',fill='palevioletred4')+
        labs(x="Month of Year", y="Counts", title="Counts for each month of year - Chicago")
# Creating plot showing how many travels happened for each day of week
ggplot(data=chi, aes(chi$Start.Time.Day))+
        geom_bar(stat="count", color='black',fill='skyblue4')+
        labs(x="Day of Week", y="Counts", title="Counts for each day of week - Chicago")
# Creating plot showing how many travels happened for each hour of day
ggplot(data=chi, aes(chi$Start.Time.Hour))+
        geom_bar(stat="count", color='black',fill='tomato4')+
        labs(x="Hour of Day", y="Counts", title="Counts for each hour of day - Chicago")

# Creating subset for New York and Washington which contain NA values in trip duration
ny.td <- subset(ny, !is.na(Trip.Duration))
wash.td <- subset(wash, !is.na(Trip.Duration))

## Visualization for total travel time data
# Creating data table (array) to store total travel time (hr) info for each city
three.total = data.frame(city=c("New York", "Washington", "Chicago"))
three.total$total.travel <- c(sum(ny.td$Trip.Duration)/3600, sum(wash.td$Trip.Duration)/3600, sum(chi$Trip.Duration)/3600)
three.total
## Creating a plot showing total travel for all three cities
ggplot(data=three.total, aes(x=city, y=total.travel))+
    geom_col(color='black',fill='skyblue')+
        labs(x="Cities", y="Total Travel Time (hr)", title="Total Travel Time (hr) for All 3 Cities")

## Creating data table containing average travel time info for each city
# Extracting travel duration info for each city from each data table
travel1 <- as.data.frame(ny.td[,4],drop=false)
travel2 <- as.data.frame(wash.td[,4],drop=false)
travel3 <- as.data.frame(chi[,4],drop=false)
# Creating a column consisting of (repeated) city name for each city travel duration data point
city1 <- rep("New York", nrow(travel1))
city2 <- rep("Washington", nrow(travel2))
city3 <- rep("Chicago", nrow(travel3))
# Combining city name column and travel duration colume for each city
ny.travel <- cbind(city1, travel1)
colnames(ny.travel) <- c("city", "travel.duration")
wash.travel <- cbind(city2, travel2)
colnames(wash.travel) <- c("city", "travel.duration")
chi.travel <- cbind(city3, travel3)
colnames(chi.travel) <- c("city", "travel.duration")
# Combining all 3 cities' city name - travel tables into one data table
three.travel <- rbind(ny.travel, wash.travel, chi.travel)

## Visualization for average travel time data using boxplot
ggplot(data=three.travel, aes(x=city, y=travel.duration/60))+
    geom_boxplot(color='black', fill='#F79420', outlier.shape=NA)+
      coord_cartesian(ylim=c(0,50))+
    labs(x="Cities", y="Average Travel Time (min)", title="Average Travel Time (min) for All 3 Cities")


## Subsetting to exclude NAs in Birth.Year and Age columns
ny.by <- subset(ny, !is.na(Birth.Year))
chi.by <- subset(chi, !is.na(Birth.Year))
## Add new column including age information
# For New York
ny.by$Age <- as.numeric(format(as.Date(Sys.Date()), "%Y"))-ny.by$Birth.Year
# For Chicago
chi.by$Age <- as.numeric(format(as.Date(Sys.Date()), "%Y"))-chi.by$Birth.Year
## Checking summary data for ages by gender
by(ny.by$Age, ny.by$Gender, summary)
by(chi.by$Age, chi.by$Gender, summary)

## Creating plot showing distribution of user ages for each city
# Converting the blank value ("") to NA
ny.by[ny.by==""]<-NA
chi.by[chi.by==""]<-NA
# Subsetting to exclude NAs in Gender columns
ny.by <- subset(ny.by, !is.na(ny.by$Gender))
chi.by <- subset(chi.by, !is.na(chi.by$Gender))
# Plot for New York
ggplot(data=ny.by, aes(ny.by$Age))+
        geom_bar(stat="count", color='black',fill='palevioletred')+
        labs(x="Age", y="Counts", title="Counts for User Age - NYC")+
        scale_x_continuous(limits=c(0,100))+
        facet_wrap(~ny.by$Gender)
# Plot for Chicago
ggplot(data=chi.by, aes(chi.by$Age))+
        geom_bar(stat="count", color='black',fill='palevioletred')+
        labs(x="Age", y="Counts", title="Counts for User Age - Chicago")+
        scale_x_continuous(limits=c(0,100))+
        facet_wrap(~chi.by$Gender)

## Re-checking summary data for ages and ages by gender for New York and Chicago
# Summary data for ages (regardless of gender)
summary(ny.by$Age)
summary(chi.by$Age)
#Summary data for ages by gender
by(ny.by$Age, ny.by$Gender, summary)
by(chi.by$Age, chi.by$Gender, summary)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
