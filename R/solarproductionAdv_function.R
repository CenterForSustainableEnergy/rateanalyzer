#' An advanced version of the solar production calculator, allows the user to input tilt, azimuth and system size
#' @export
#' @title takes an xml file of greenbutton format, gets the address and then estimates solar production
#' @name solarproductionAdv
#' @param the address of the house and the average annual consumption
#' @return an array of average daily solar production
#' @author Christina Machak
#' @import httr

solarproductionAdv <- function(lat,lon,system.size,tilt,azimuth){
  
  pv_watts <- paste("https://developer.nrel.gov/api/pvwatts/v5.json?api_key=IMxJhgp5gFZ0asOXGNy3SowfsVLyy4e7ef2XlwNj&lat=",lat,"&lon=",lon,
                    "&system_capacity=",system.size,"&azimuth=",azimuth,"&tilt=",tilt,"&array_type=1&module_type=1&losses=10&timeframe=hourly",sep='')
  
  watts.out <- GET(pv_watts)
  results <- content(watts.out)
  
  ac_pv <- data.frame(matrix(unlist(results$outputs$ac), ncol=24, byrow=T))
  
  ##convert from Wh to kWh
  ac_pv <- ac_pv/1000
  
  mean.daily.production <- (colSums(ac_pv)/(nrow(ac_pv)))
  
  return_array <- list(mean.daily.production,ac_pv,system.size)
  
  return(return_array)
}
