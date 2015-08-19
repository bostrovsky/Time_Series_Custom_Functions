loadData <- function() {
  
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(zoo)
  
  
  #data found at: http://catalog.data.gov/dataset/consumer-complaint-database
  
  #Bring in data
  data <- file.choose()
  data1 <- read_csv(data)
  
  #Summarise daily data 
  dataSum <- data1 %>% group_by(Date) %>% summarise(Count=n())
  
  ##If you need to change a column date name
  names(dataSum)[1] <- "Date"
  
  
  #Create column for month
  dataSum$Month <- as.yearmon(dataSum$Date, format = "%m/%d/%Y")
  dataSum$Month <- as.character(dataSum$Month)
  
  save(dataSum, file="dataSum")
}


monthly <- function(month, year) {
  
  load("dataSum")
  
  #filter data by date and month
  dataSum1 <- dataSum %>% select(Date, Count)
  dataSum1 %<>% filter(grepl(year,Date))
  dataSum1 <- dataSum1[grepl(paste("^",month,sep=""), dataSum1$Date),]
  dataSum1$Date <- as.Date(dataSum1$Date, "%m/%d/%Y")
  dataSum1 %<>% group_by(Date) %>% summarise_each(funs(sum(Count)))
  #create visualization for days in month
  df <- dataSum1[order(dataSum1$Date), ]
  dt <- qplot(Date, Count, data=df, geom="line") + theme(aspect.ratio = 1/2)
  dt + scale_x_date(labels = date_format("%m/%d"),breaks = date_breaks("day"))
}