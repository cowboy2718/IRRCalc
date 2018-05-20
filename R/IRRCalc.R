# Incident Rate Ratio Calculator
# May 19, 2018
# Created by Tony Gojanovic

#' Incident Rate Ratio Calculator
#'
#' @description
#' This simple function calculates the ratio of two incident rates along with a 95% confidence bound for the ratio.
#' @param count1 Incident count for the first sample.
#' @param count2 Incident count for the second sample.
#' @param volume1 Volume or standardizing value for the first sample.
#' @param volume 2 Volume or standardizing value for the second sample.
#' @return Returns a data frame with the Incident Rate Ratio (IRR) and the lower and upper 95% confidence bound.
#' @references Algorithm based on "Comparing two incident rate ratios,” https://stats.stackexchange.com/questions/297994/comparing-two-incidence-rate-ratios
#' @export


IRR<-function(count1, volume1, count2, volume2){
  
  # Provide filters to the user to assure positive or non-zero values are input

  if(count1< 0){stop("Input a postive integer of 0 or greater")}else
    if(count2 <0){stop("Input a postive integer of 0 or greater")}else
      if(volume1 <= 0){stop("Input a postive integer of 0 or greater")}else
        if(volume2 <= 0){stop("Input a postive integer of 0 or greater")}else

  # This program calculate the incident ratio along with a confidence interval.

  IRR = (count1/volume1)/(count2/volume2)

  lower_CI = exp(log(IRR) - 1.96*sqrt(1/volume1 + 1/volume2))
  upper_CI = exp(log(IRR) + 1.96*sqrt(1/volume1 + 1/volume2))
  
  # Create a data frame of values for the user

  bounds_matrix<-data.frame(Statistic=c("Incident Rate Ratio","Lower 95% CI","Upper 95% CI"),Estimate=c(IRR,lower_CI,upper_CI))
  
  # Return the data frame

  return(bounds_matrix)

 }
