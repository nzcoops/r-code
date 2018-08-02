#############################################################################
## Function to Calculate Monthly Mortgage Payments and Amortization Tables ##
#############################################################################
# Author: Thomas Girke
# Last update: Feb 27, 2007
# Utility: Calculates monthly and annual loan or mortgage payments, generates amortization tables and plots the results
# How to run the script:
#	source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")

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
	#cat("\nThe payments for this loan are:\n 
	#		Monthly payment: $", M, " (stored in monthPay)\n
	#		Total cost: $", M*N, "\n\n", sep="")
	
	# Calculate Amortization for each Month
	if(amort==T) {
		Pt <- P # current principal or amount of the loan
		currP <- NULL
		while(Pt>=0) {
			H <- Pt * J # this is the current monthly interest
			C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
			Q <- Pt - C # this is the new balance of your principal of your loan
			Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
			currP <- c(currP, Pt)
		}
		monthP <- c(P, currP[1:(length(currP)-1)])-currP
		month <- data.frame(
					      Amortization=c(P, currP[1:(length(currP)-1)]), 
					      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
					      Monthly_Principal=monthP, 
					      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
					      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
				)
		year <- data.frame(
					     Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
					     Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
					     Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
					     Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
					     Year=as.vector(na.omit(unique(aDFmonth$Year)))
					     )
  return(list("month" = month, "year" = year))             
	#	aDFyear <- aDFyear
	#	cat("The amortization data for each of the", N, "months are stored in \"aDFmonth\".\n\n")
	#	cat("The amortization data for each of the", L, "years are stored in \"aDFyear\".\n\n")
	}
}
#cat("The monthly mortgage payments and amortization rates can be calculted with the mortgage() function like this: \n 
#	mortgage(P=500000, I=6, L=30, amort=T, plotData=T)
#		P = principal (loan amount)
#		I = annual interest rate
#		L = length of the loan in years \n")
