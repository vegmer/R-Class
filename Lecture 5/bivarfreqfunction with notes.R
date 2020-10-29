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


#if you give me two variables, Im oging to find you what the most frequent value

#It gives you three values: for that value: x, y and density (since we=re using default 
normal distributions, x and y should be around 0 (remember, mean is 0 and sd is 1 in a default
N(0,1)

