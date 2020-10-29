whatis <- function() {

	origobj <- winDialogString('What is the name of the R object you would like to screen?','')
	object <- get(origobj)

	if(is.null(dim(object))==TRUE) {
		odims <- paste("L=",length(object),sep="")
	}

	else { 
		odims <- paste("R=",dim(object)[1]," C=",dim(object)[2],sep="") 
	}

	outtab <- data.frame(	'Type'=typeof(object),
				'Mode'=mode(object),
				'Class'=class(object),
				'Dimension'=odims,
				'Size'=paste(object.size(object),"bytes"),
				'Attributes'=length(c(names(attributes(object)))))

	print(paste("Object Specifications for { ", origobj, " }",sep=""))
	outtab
}

winMenuAdd("Class Demos")
winMenuAdd("Class Demos/Data")
winMenuAddItem("Class Demos/Data","Screen","whatis()")
winMenuAddItem("Class Demos/Data","Plot","plot(get(winDialogString('What is the name of the R object you would like to plot?','')))")
winMenuAdd("Class Demos/Computations")
winMenuAddItem("Class Demos", "--","")

NowAddno <- function() {

	x <- as.numeric(winDialogString("Define the scalar value", "X-value here..."))
	y <- as.numeric(winDialogString("Define the scalar value", "Y-value here..."))
	x+y
}

NowAdd <- function() {

	object <- winDialogString("Name the output object", "")
	x <- as.numeric(winDialogString("Define the scalar value", "X-value here..."))
	y <- as.numeric(winDialogString("Define the scalar value", "Y-value here..."))
	assign(object, x+y, pos=.GlobalEnv)
	print(paste("Your object {",object,"} has been created")) 
}

winMenuAddItem("Class Demos/Computations","Addition w/o object", "NowAddno()")
winMenuAddItem("Class Demos/Computations","Addition w/ object", "NowAdd()")

winMenuAdd("Class Demos/Graphing Demos")

bkdpd <- function() {
	library(MASS)
	x <- rnorm(1000)
	y <- rnorm(1000)

	sgbr <- winDialog("yesno", "Should the 3-D graph be rotated?")

	if(sgbr=="NO") {
		persp(kde2d(x,y),phi=30, theta=20, ticktype="detailed", shade=.6, col="light green", main="Bivariate Density Distribution", xlab="X - Variable", ylab="Y - Variable", zlab="Density")
	}
	
	if(sgbr=="YES") {
		alwa <- winDialogString("Along which axis should the graph be rotated? (X, Y, BOTH)", "")

			if(alwa=="X" | alwa=="x" | alwa=="X-axis" | alwa=="x-axis" | alwa=="X axis" | alwa=="x axis") {
				for (i in 1:1000) {
					persp(kde2d(x,y),phi=30, theta=i, ticktype="detailed", shade=.6, col="light green", main="Bivariate Density Distribution", xlab="X - Variable", ylab="Y - Variable", zlab="Density")
				}
			}
			if(alwa=="Y" | alwa=="y" | alwa=="Y-axis" | alwa=="y-axis" | alwa=="Y axis" | alwa=="y axis") {
				for (i in 1:1000) {
					persp(kde2d(x,y),phi=i, theta=20, ticktype="detailed", shade=.6, col="light green", main="Bivariate Density Distribution", xlab="X - Variable", ylab="Y - Variable", zlab="Density")
				}
			}
			if(alwa=="BOTH" | alwa=="both" | alwa=="Both" | alwa=="Both axes" | alwa=="both  axes" | alwa=="Both axes") {
				for (i in 1:1000) {
					persp(kde2d(x,y),phi=i, theta=i, ticktype="detailed", shade=.6, col="light green", main="Bivariate Density Distribution", xlab="X - Variable", ylab="Y - Variable", zlab="Density")
				}
			}
			else {		
					persp(kde2d(x,y),phi=30, theta=20, ticktype="detailed", shade=.6, col="light green", main="Bivariate Density Distribution", xlab="X - Variable", ylab="Y - Variable", zlab="Density")
			}
	}
	
}

winMenuAddItem("Class Demos/Graphing Demos", "Bivariate Density", "bkdpd()")

regoutdemo <- function(n) {

	xdat <- sort(rnorm(100))
	ydat <- sapply(sort(rnorm(100)),function(x) {x+rnorm(1, mean=0, sd=.8)}) 

	par(mar=c(4.5,4.5,1,1))
	plot(xdat,ydat, pch=20)
	origlin <- lm(ydat~xdat)
	abline(origlin, lty=2, lwd=3, col="dark green")
	origr <- round(cor(ydat,xdat),4)
	legend("bottomright",paste("r = ",origr, sep=""), adj=.2, inset=.03, box.lwd=2)

	for(i in 1:n) {
		newadd <- locator(1)
		xdat <- c(xdat, newadd$x)
		ydat <- c(ydat, newadd$y)
		par(mar=c(4.5,4.5,1,1))
		plot(xdat,ydat, type="n")
		points(xdat[1:100],ydat[1:100], pch=20)
		points(xdat[101:(100+i)],ydat[101:(100+i)], pch=15, cex=1.3, col="red3")
		abline(origlin, lty=2, lwd=3, col="dark green")
		abline(lm(ydat~xdat), lty=2, lwd=3, col="red3")
		legend("bottomright",legend=bquote(paste(r[old]," = ",.(origr))), adj=.1, inset=c(.27,.03), box.lwd=2)
		legend("bottomright",legend=bquote(paste(r[new]," = ",.(round(cor(ydat,xdat),4)))), adj=.1, inset=.03, box.lwd=2)
		legend("bottomright",legend=bquote(paste(r[delta]," = ",.(origr-round(cor(ydat,xdat),4)))), adj=.1, inset=c(.03,.11), box.lwd=2, text.col="red3", box.col="red3")
	}

}

winMenuAddItem("Class Demos/Graphing Demos", "Regression Outlier", "regoutdemo(as.numeric(winDialogString('How many outliers would you like to investigate? (n)', '')))")



