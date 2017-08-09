#' Takes energy consumption data, solar production data, climate zone, and  
#' @export
#' @title takes an xml file of greenbutton format, gets the address and then estimates solar production
#' @name billcalculate
#' @param the address of the house and the average annual consumption
#' @return an array of average daily solar production
#' @author Christina Machak
#' @import httr

billcalculate <- function(interval_data,cz,fuel,discount){
  
  ##SDG&E baselines for residential service
  basic_baseline = data.frame(summer = c(9.6,11.2,14.8,16.4),winter = c(10.1,10.8,13.8,11.2), row.names = c('Coastal','Inland','Mountain','Desert'))
  elec_baseline = data.frame(summer = c(9.8,11.0,17.3,19.5),winter = c(16.6,18.3,28.5,22.0), row.names = c('Coastal','Inland','Mountain','Desert'))
  
  ##number of days in each month, for data cleaning
  days.month <- data.frame(month = c('01','02','03','04','05','06','07','08','09','10','11','12'), days.in.month = c(31,28,31,30,31,30,31,31,30,31,30,31))
  
  ##choose the appropriate baseline matrix
  if (fuel == 'Natural Gas'){
    baseline = t(basic_baseline[cz,])
  } else{
    baseline = t(elec_baseline[cz,])
  }
  
  colnames(baseline) <- c('daily.baseline')
  
  ##SDG&E DR Rates
  dr.rates <- data.frame(season=c("summer","winter"),rate1=c(0.19134,0.17548),rate2=c(0.39485,0.36189))
  
  ##drop 2/29 if it exists in the interval data as this date is not present in the climate year used for solar prod. estimates
  interval_data <- interval_data[!((interval_data$month == '02') & (interval_data$day == 29)),]
  
  hours.month.data <- aggregate(day~month+year, interval_data, length)
  monthly.sums <- aggregate(value~month+year, interval_data, sum)

  ##get rid of incomplete months of data, if more than 10 hours of data are missing from any month it will be removed
  monthly.sums <- merge(monthly.sums,hours.month.data, by=c("month","year"))
  monthly.sums <- merge(monthly.sums,days.month, by=c("month"))
  
  monthly.sums <- monthly.sums[!((monthly.sums$days.in.month*24) > (monthly.sums$day+20)),]
  
  monthly.sums$season <- "Empty"
  monthly.sums[as.numeric(monthly.sums$month) %in% c(11,12,1,2,3,4), ]$season <- "winter"
  monthly.sums[as.numeric(monthly.sums$month) %in% c(5,6,7,8,9,10), ]$season <- "summer"
  
  ##add the monthly baseline to the data
  monthly.sums <- merge(monthly.sums, baseline, by.x = "season", by.y = "row.names")
  monthly.sums[,'monthly.baseline'] = monthly.sums[,'daily.baseline']*monthly.sums[,'days.in.month']
  
  ##convert consumption from Wh to kWh
  monthly.sums[,'value'] <- monthly.sums[,'value']/1000
  
  ##determine the portion of consumption that is up to 130% of baseline
  monthly.sums['tier2'] <- (monthly.sums$value) - (1.3*monthly.sums$monthly.baseline)
  monthly.sums[monthly.sums['tier2'] < 0, which(colnames(monthly.sums) == "tier2")] <- 0
  
  ##determine the potion of consumption that is above 130% of baseline
  monthly.sums['tier1'] <- (monthly.sums['value'] - monthly.sums['tier2'])
  
  ##calculate the monthly cost
  monthly.sums = merge(monthly.sums,dr.rates,by="season")
  monthly.sums['bill'] <- monthly.sums['tier1']*monthly.sums['rate1'] + monthly.sums['tier2']*monthly.sums['rate2']
  
  monthly.sums = monthly.sums[order(as.numeric(monthly.sums$month)),]
  
  current.bill = monthly.sums$bill
  
  if (discount=="CARE"){
    current.bill <- 0.7*current.bill
  }
  
  if (discount=="FERA"){
    current.bill <- 0.88*current.bill
  }
  
  annual.bill = sum(current.bill)
  return_array <- list(current.bill,annual.bill)
  
  return(return_array)
}
