#' Takes greenbutton data,extracts hourly interval and returns daily mean consumption
#' @export
#' @title takes an xml file of greenbutton format, gets the address and then estimates solar production
#' @name solarproduction
#' @param the address of the house and the average annual consumption
#' @return an array of average daily solar production
#' @author Christina Machak
#' @import httr

solarproduction <- function(lat,lon,annual.consumption){
  size.ratio = 0.8
  pv.production = (annual.consumption/1000)*size.ratio
  
  ##determine appropriate system size, 80% of average annual consumption
  pv_watts.1 <- paste("https://developer.nrel.gov/api/pvwatts/v5.json?api_key=IMxJhgp5gFZ0asOXGNy3SowfsVLyy4e7ef2XlwNj&lat=",lat,"&lon=",lon,
                      "&system_capacity=1&azimuth=180&tilt=33&array_type=1&module_type=1&losses=10",sep='')
  test <- GET(pv_watts.1)
  results <- content(test)
  
  pv.output.1kW <- results$outputs$ac_annual
  system.size <- pv.production/pv.output.1kW
  
  pv_watts <- paste("https://developer.nrel.gov/api/pvwatts/v5.json?api_key=IMxJhgp5gFZ0asOXGNy3SowfsVLyy4e7ef2XlwNj&lat=",lat,"&lon=",lon,
                    "&system_capacity=",system.size,"&azimuth=180&tilt=40&array_type=1&module_type=1&losses=10&timeframe=hourly",sep='')
  
  watts.out <- GET(pv_watts)
  results <- content(watts.out)
  
  ac_pv <- data.frame(matrix(unlist(results$outputs$ac), ncol=24, byrow=T))
  
  ##convert from Wh to kWh
  ac_pv <- ac_pv/1000
  
  mean.daily.production <- (colSums(ac_pv)/(nrow(ac_pv)))
  
  return_array <- list(mean.daily.production,ac_pv,system.size)
  
  return(return_array)
}
