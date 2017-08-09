#' Takes energy consumption data, solar production data, climate zone, and  
#' @export
#' @title takes a customers consumption data, estimated solar production and 
#' @name solarbill_shift
#' @param interval_data, solar_prod, cz, fuel
#' @return an array of average daily solar production
#' @author Christina Machak
#' @import httr

solarbill_shift <- function(interval_data,solar.prod,discount){
  
  ##number of days in each month, for data cleaning
  days.month <- data.frame(month = c('01','02','03','04','05','06','07','08','09','10','11','12'), days.in.month = c(31,28,31,30,31,30,31,31,30,31,30,31))
  
  ##drop 2/29 if it exists in the interval data as this date is not present in the climate year used for solar prod. estimates
  interval_data <- interval_data[!((interval_data$month == '02') & (interval_data$day == 29)),]
  
  hours.month.data <- aggregate(day~month+year, interval_data, length)
  colnames(hours.month.data) <- c('month','year','hour.count')
  
  interval_data <- merge(interval_data,hours.month.data, by=c("month","year"))
  interval_data <- merge(interval_data,days.month, by=c("month"))
  
  interval_data <- interval_data[!((interval_data$days.in.month*24) > (interval_data$hour.count+20)),]
  
  ##SDG&E DR-SES Rates
  rate <- data.frame(summer=c(0.49395,0.24592,0.22272),winter=c(0,0.23145,0.21738), row.names = c("onpeak","semipeak","offpeak"))
  
  ##minimum bill per day, CARE customers get 50% minimum bill
  min.bill = 0.329
  
  ##define the peaks
  peak <- data.frame(clock = c(0:23), peak.summer = c(rep(0,24)), semi.summer = c(rep(0,24)), off.summer = c(rep(0,24)), weekend.summer = c(rep(1,24)), peak.winter = c(rep(0,24)), semi.winter = c(rep(0,24)), off.winter = c(rep(0,24)), weekend.winter = c(rep(1,24)))
  ##proposedsummer peak is from 13:00-20:00
  peak[14:20,'peak.summer'] <- 1
  ##summer semi-peak is from 6:00-11:00 and 20:00-22:00
  peak[c(7:13,21:22),'semi.summer'] <- 1
  ##summer off-peak is from 22:00-6:00
  peak[c(23:24,1:6),'off.summer'] <- 1
  ##winter peak does not exist
  ##winter semi peak is from 6:00 to 18:00
  peak[7:18,'semi.winter'] <- 1
  ##winter off-peak from from 18:00 to 6:00
  peak[c(19:24,1:6),'off.winter'] <- 1
  
  ##create the rate table
  peak['peak.summer'] <- peak['peak.summer']*rate[1,1]
  peak['semi.summer'] <- peak['semi.summer']*rate[2,1]
  peak['off.summer'] <- peak['off.summer']*rate[3,1]
  peak['weekend.summer'] <- peak['weekend.summer']*rate[3,1]
  peak['peak.winter'] <- peak['peak.winter']*rate[1,2]
  peak['semi.winter'] <- peak['semi.winter']*rate[2,2]
  peak['off.winter'] <- peak['off.winter']*rate[3,2]
  peak['weekend.winter'] <- peak['weekend.winter']*rate[3,2]
  peak['weekday.summer'] <- peak['peak.summer'] + peak['semi.summer'] + peak['off.summer']
  peak['weekday.winter'] <- peak['peak.winter'] + peak['semi.winter'] + peak['off.winter']
  
  ##non-bypassable charges, curently the same for all time periods (PPP+ND+CTC+DWR-BC)
  nonbypassable = 0.01063+0.00049+0.00182+0.00549
  
  colnames(solar.prod) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
  solar.prod['day'] <- format(seq(as.Date("2017-01-01"),as.Date("2017-12-31"),by="days"),"%d")
  solar.prod['month'] <- format(seq(as.Date("2017-01-01"),as.Date("2017-12-31"),by="days"),"%m")
  
  solar.prod = reshape(solar.prod, idvar = c("day","month"), v.names="prod", varying=c(1:24), direction="long")
  
  solar.prod['time'] = solar.prod['time'] - 1
  
  interval_data$time <- as.numeric(interval_data$hour)
  
  interval_data = merge(solar.prod, interval_data, by = c("day","month","time"), all.x = TRUE, all.y = FALSE)
  
  ##delete rows with mising data
  interval_data = interval_data[!is.na(interval_data$value),]
  
  ##convert from kWh to Wh
  solar.prod$prod <- solar.prod$prod*(1/1000)
  interval_data$value <- interval_data$value*(1/1000)
  
  ##determine the net consumption and production for each metered period
  interval_data['net.consumption'] <- interval_data['value'] - interval_data['prod']
  interval_data[interval_data$net.consumption < 0,]$net.consumption <- 0
  
  interval_data['net.prod'] <- interval_data['prod'] - interval_data['value']
  interval_data[interval_data$net.prod < 0,]$net.prod <- 0
  
  ##join the rate information
  interval_data <- merge(interval_data,peak, by.x ="time", by.y="clock")
  
  interval_data['rate'] <- 0
  
  interval_data$season <- "empty"
  interval_data[as.numeric(interval_data$month) %in% c(11,12,1,2,3,4), ]$season <- "winter"
  interval_data[as.numeric(interval_data$month) %in% c(5,6,7,8,9,10), ]$season <- "summer"
  
  ##set the weekday rates
  interval_data[(interval_data$season == 'winter'),]$rate <- interval_data[(interval_data$season == 'winter'),]$weekday.winter
  interval_data[(interval_data$season == 'summer'),]$rate <- interval_data[(interval_data$season == 'summer'),]$weekday.summer
  
  ##set all the off peak hours for the weekends
  interval_data[(interval_data$season == 'winter') & (interval_data$weekday %in% c('Saturday','Sunday')),]$rate <- interval_data[(interval_data$season == 'winter') & (interval_data$weekday %in% c('Saturday','Sunday')),]$weekend.winter
  interval_data[(interval_data$season == 'summer') & (interval_data$weekday %in% c('Saturday','Sunday')),]$rate <- interval_data[(interval_data$season == 'summer') & (interval_data$weekday %in% c('Saturday','Sunday')),]$weekend.summer
  
  ##calculate the consumption cost
  interval_data$cons.bill <- interval_data$net.consumption * interval_data$rate
  
  ##calculate the nonbypassable portion of the bill
  interval_data$nonbypass <- interval_data$net.consumption * nonbypassable
  
  ##calculate the production credits
  interval_data$prod.cred <- interval_data$net.prod * interval_data$rate
  
  monthly.cons <- aggregate(cons.bill~month+year, interval_data,sum)
  monthly.prod <- aggregate(prod.cred~month+year, interval_data,sum)
  monthly.nonbypass <- aggregate(nonbypass~month+year, interval_data,sum)
  
  monthly.cost <- merge(monthly.cons, monthly.nonbypass, by=c("month","year"))
  monthly.cost <- merge(monthly.cost, monthly.prod, by=c("month","year"))
  
  monthly.cost$bill = (monthly.cost$cons.bill - monthly.cost$prod.cred) + monthly.cost$nonbypass
  monthly.cost <- merge(monthly.cost,days.month, by="month")
  monthly.cost$min <- monthly.cost$days.in.month*min.bill
  
  monthly.cost$final.bill <- monthly.cost$bill
  monthly.cost[(monthly.cost$bill < monthly.cost$min),]$final.bill <- monthly.cost[monthly.cost$bill < monthly.cost$min,]$min
  
  solar.bill <- monthly.cost$final.bill
  
  ##need to account for net annual production, if total consumption cost is less than total production cost, set all months bill to sero and 
  ##then subtract total consumption from total production and use that to calculate a rebate, based on the wholesale rate
  
  
  
  ##incorporate discounts if they apply
  if (discount=="CARE"){
    solar.bill <- 0.7*solar.bill
  }
  
  if (discount=="FERA"){
    solar.bill <- 0.88*solar.bill
  }
  
  annual.bill <- sum(solar.bill)
  return_array <- list(solar.bill,annual.bill)
  
  return(return_array)
}