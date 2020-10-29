findfreq <- function(x,y) {
	message("The function has been initiated...")
	require(MASS)
	dxy <- kde2d(x,y)
	checkmat <- matrix(dxy$z,length(dxy$x), length(dxy$y))
	rownames(checkmat) <- dxy$x
	colnames(checkmat) <- dxy$y
	for(i in 1:length(dxy$x)) {
		for (j in 1:length(dxy$y)) {
			if(checkmat[i,j]==max(checkmat)) {
				bivals <- c(dxy$x[i],dxy$y[j],checkmat[i,j])
			}
		}
	}

	matmat <- persp(dxy, phi=20, theta=30, xlab="X", ylab="Y", zlab="Density", tick="detailed")
	points(trans3d(bivals[1], bivals[2],bivals[3], pmat = matmat), col="red3",pch=16, cex=2)
	message("... and we are done.")
	bivals
}
