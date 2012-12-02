# Regression diagnostics for mixed models
# For use with nlme::lme 
# Original code from Andrew Robinsons - icebreakeR
# http://www.ms.unimelb.edu.au/~andrewpr/
# 100% credit to Andrew
# last modified 03/02/2011

mm.diag.2 <- function(diag.mod)
	{
	options(warn=-1)
	
	diag.y <- diag.mod$fitted[,1] + diag.mod$residuals[,1]

	opar <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), las = 1,
	cex.axis = 0.9)
	
	#
	plot(fitted(diag.mod, level=0), diag.y,
	xlab = "Fitted Values", ylab = "Observed Values",
	main = "Model Structure (I)")
	abline(0, 1, col = "gray")
	
	#
	scatter.smooth(fitted(diag.mod), residuals(diag.mod, type="pearson"),
	main = "Model Structure (II)",
	xlab = "Fitted Values", ylab = "Innermost Residuals")
	abline(h = 0, col = "gray")
	
	#
	acf.resid <- ACF(diag.mod, resType = "normal")
	plot(acf.resid$lag[acf.resid$lag < 10.5],
	acf.resid$ACF[acf.resid$lag < 10.5],
	type="b", main="Autocorrelation",
	xlab="Lag", ylab="Correlation")
	stdv <- qnorm(1 - 0.01/2)/sqrt(attr(acf.resid, "n.used"))
	lines(acf.resid$lag[acf.resid$lag < 10.5],
	stdv[acf.resid$lag < 10.5],
	col="darkgray")
	lines(acf.resid$lag[acf.resid$lag < 10.5],
	-stdv[acf.resid$lag < 10.5],
	col="darkgray")
	abline(0,0,col="gray")

	options(warn=0)	
	par(opar)
	}
	