# Regression diagnostics for mixed models
# For use with nlme::lme 
# Original code from Andrew Robinsons - icebreakeR
# http://www.ms.unimelb.edu.au/~andrewpr/
# 100% credit to Andrew
# last modified 03/02/2011

mm.diag.1 <- function(diag.mod)
	{
	options(warn=-1)
	
	diag.y <- diag.mod$fitted[,1] + diag.mod$residuals[,1]
	diag.id <- diag.mod$groups

	# set up graphing area
	opar <- par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), las = 1, cex.axis = 0.9)

	#### Plot 1
	plot(fitted(diag.mod, level=0), diag.y,
	xlab = "Fitted Values",
	ylab = "Observed Values",
	main = "Model Structure (I)")
	abline(0, 1, col = "blue")
	
	#### Plot 2
	scatter.smooth(fitted(diag.mod), residuals(diag.mod, type="pearson"),
	xlab = "Fitted Values",
	ylab = "Innermost Residuals",
	main = "Model Structure (II)")
	abline(h = 0, col = "red")
	
	#### Plot 3
	ref.group <- ranef(diag.mod)[[1]]
	ref.var.group <- tapply(residuals(diag.mod, type="pearson", level=1), diag.id, var)
	
	qqnorm(ref.group, main="Q-Q Normal - Group Random Effects")
	qqline(ref.group, col="red")
	
	#### Plot 4
	qqnorm(residuals(diag.mod, type="pearson"), main="Q-Q Normal - Residuals")
	qqline(residuals(diag.mod, type="pearson"), col="red")
	
	#### Plot 5
	#print(residuals(diag.mod, type="pearson", level=1))
	#print(diag.y)
	
	boxplot(residuals(diag.mod, type="pearson", level=1) ~ diag.id[,1],
	ylab = "Innermost Residuals", xlab = "Group",
	notch=T, varwidth = T, at=rank(ref.group))
	axis(3, labels=format(ref.group, dig=2), cex.axis=0.8,
	at=rank(ref.group))
	abline(h=0, col="darkgreen")
	
	#### Plot 6
	plot(ref.group, ref.var.group, xlab="Group Random Effect",
	ylab="Variance of within-Group Residuals")
	abline(lm(ref.var.group ~ ref.group), col="purple")
	
	options(warn=0)
	
	par(opar)
	}
	