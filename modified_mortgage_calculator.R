#############################################################################
## Function to Calculate Monthly Mortgage Payments and Amortization Tables ##
#############################################################################
# Original Author: Thomas Girke
# Modifited By: Matt Cooper
# Last update: 2018-08-03
# Utility: Calculates monthly and annual loan or mortgage payments, generates amortization tables and plots the results
# Original Script: http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R
# Updates: Output as a list with the monthly and yearly tables returned from the function, removed prints to screen and production of graph

# Definitions: 
#	  P = principal, the initial amount of the loan
#	  I = annual interest rate
#	  L = length of the loan in years, or at least the length over which the loan is amortized.
#	  J = monthly interest in decimal form = I / (12 x 100)
#	  M = monthly payment; formula: M = P * ( J / (1 - (1 + J) ^ -N))
#	  N = number of months over which loan is amortized = L x 12
# see also: http://www.jeacle.ie/mortgage/instructions.html

mortgage <- function(P=500000, I=6, L=30, amort=T) { 
  J <- I/(12 * 100)
  N <- 12 * L
  M <- P*J/(1-(1+J)^(-N))
  monthPay <<- M
  
  # Calculate Amortization for each Month
  if(amort==T) {
    Pt <- P # current principal or amount of the loan
    currP <- NULL
    
    while(Pt >= 0) {
      H <- Pt * J # this is the current monthly interest
      C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
      Q <- Pt - C # this is the new balance of your principal of your loan
      Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
      currP <- c(currP, Pt)
    }
    
    monthP <- c(P, currP[1:(length(currP)-1)])-currP
    
    month <- data.frame(
      Amortization = c(P, currP[1:(length(currP) - 1)]), 
      Monthly_Payment = monthP + c((monthPay - monthP)[1:(length(monthP) - 1)],0),
      Monthly_Principal = monthP, 
      Monthly_Interest = c((monthPay - monthP)[1:(length(monthP) - 1)],0), 
      Year = sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
    )
    
    year <- data.frame(
      Amortization = tapply(month$Amortization, month$Year, max), 
      Annual_Payment = tapply(month$Monthly_Payment, month$Year, sum), 
      Annual_Principal = tapply(month$Monthly_Principal, month$Year, sum), 
      Annual_Interest = tapply(month$Monthly_Interest, month$Year, sum), 
      Year = as.vector(na.omit(unique(month$Year)))
    )
    
    return(list("month" = month, "year" = year))             
    
  }
}
