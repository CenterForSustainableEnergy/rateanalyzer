#' Takes greenbutton data,extracts hourly interval and returns daily mean consumption
#' @export
#' @title takes an xml file of greenbutton format, comsumption data and returns 5 objects - annual, summer and winter daily mean consumption, and mean total annual consumption,and the total consumption array
#' @name greenbuttonParse
#' @param xmlfile, the file to parse 
#' @return A data frame of daily mean comsumption by the hour 
#' @author Christina Machak
#' @import xml2
#' @import anytime
#' @import httr

greenbuttonParse <- function(xmlfile){
  xml_data <- read_xml(xmlfile)
  
  ##initialize a matrix to store the interval data in
  mat <- c(1,1)
  
  ##extract the portion of the greenbutton that stores interval data
  ##search through the xml tree and pull out all of the necessary data
  ##this currently only works for hourly data, needs to be edited to work for 15 minute interval data
  
  for (i in 9:(xml_length(xml_data)-1)){
    
    entry = xml_children(xml_data)[[i]]
    day = xml_children(xml_child(entry,"d1:content"))[[1]]
    
    ##for each day, loop through 24 hours, or fewer if there are missing hours in the data
    for (j in 2:xml_length(day)){
      start = xml_integer(xml_children(xml_children(xml_children(day)[[j]])[[1]])[[2]])
      value = xml_integer(xml_children(xml_children(day)[[j]])[[2]])
      mat <- rbind(mat,c(start,value),deparse.level = 0)
    }
  }
  ##put the data extracted from the xml file into an R data frame and then convert the date to POSIX format
  interval_data <- data.frame(mat[-1,])
  colnames(interval_data) <- c("start","value")
  ##the SDGE data is off by 8hours because the timestamp has PST listed, but it is actually in UTC
  interval_data[,1] <- anytime(interval_data[,1])
 ##interval_data[,1] <- anytime(interval_data[,1]) + (8*3600)
  
  ##extract the day,month and year, hour and day of week from the start object and store in their own columns of the data frame
  interval_data[,"day"] <- format(interval_data[,1],"%d")
  interval_data[,"month"] <- format(interval_data[,1],"%m")
  interval_data[,"year"] <- format(interval_data[,1],"%Y")
  interval_data[,"hour"] <- format(interval_data[,1],"%H")
  interval_data[,"weekday"] <- weekdays(interval_data[,1])
  
  ##disaggregate summer and winter consumption
  summer.consumption <- interval_data[interval_data$month %in% c('05','06','07','08','09','10'),]
  winter.consumption <- interval_data[interval_data$month %in% c('11','12','01','02','03','04'),]
  
  ##calculate daily consumption annually, in summer and in winter
  mean.daily.consumption <- aggregate(value ~ hour, interval_data, FUN = mean)
  mean.daily.consumption$value <- mean.daily.consumption$value/1000
  summer.daily.consumption <- aggregate(value ~ hour, summer.consumption, FUN = mean)
  summer.daily.consumption$value <- summer.daily.consumption$value/1000
  winter.daily.consumption <- aggregate(value ~ hour, winter.consumption, FUN = mean)
  winter.daily.consumption$value <- winter.daily.consumption$value/1000
  
  ##calculate the total annual consumption
  mean.annual.consumption <- (sum(interval_data[,"value"])/nrow(interval_data))*(365*24)
  
  return_array <- list(mean.daily.consumption,mean.annual.consumption,summer.daily.consumption,winter.daily.consumption,interval_data)
  return(return_array)
}

