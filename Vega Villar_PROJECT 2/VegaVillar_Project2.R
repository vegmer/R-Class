load("/Users/mercedesvegavillar/Dropbox/CUNY/YEAR 3/Spring 2014/R Class/PROJECT 2/latencypersession.RData")
load("/Users/mercedesvegavillar/Dropbox/CUNY/YEAR 3/Spring 2014/R Class/PROJECT 2/parseddata.RData")
load("/Users/mercedesvegavillar/Dropbox/CUNY/YEAR 3/Spring 2014/R Class/PROJECT 2/meanbins.RData")


## I'm going to plot 3 pairs of graphs:
### - Bar plots of the mean proportion of trials in which rats responded across the session (standard R plot)
### - Line plot of the mean latency of response during two moments in the trial (custom plot)
### - Raster plots of the head entries of one rat (custom plot)


dev.off()
par(mfrow=c(3,3), pty="s", mar=c(4,4,4,4), oma=c(2,2,4,3))


######
# BAR PLOT OF RESPONSE RATIO ACROSS A SESSION OF 100 TRIALS (I grouped trials in bins of 10 trials)
######

#Day 1
barplot(meanbins[,1], 
        col="orange", 
        border=NA,
        xlab="10-trial bins",
        ylab="Response ratio", 
        xlim=c(0,10), 
        ylim=c(0,1), 
        las=1,
        main="Day 1")



#Day 5
barplot(meanbins[,5], 
        col="purple", 
        border=NA,
        xlab="10-trial bins",
        ylab="Response ratio", 
        xlim=c(0,10), 
        ylim=c(0,1), 
        las=1,
        main="Day 5")

plot.new()
mtext("Response ratio goes up", side=1, line=-5, cex=0.7, col="darkgray")
mtext("but it's high since day 1.", side=1, line=-4, cex=0.7, col="darkgray")
mtext("Let's look at latency.", side=1, line=-3, cex=0.7, col="darkgray")


######
# LINE PLOT OF LATENCY AT ITI (Intertrial interval) AND CUE ACROSS TRIALS (Each session has 100 trials approx., I grouped trials in bins of 10 trials)
######

#Day 1
plot.new()
plot.window(xlim = c(0,10), ylim = c(0,5))

axis(1, at = seq(0,10,1), cex.axis = 0.8, las=1)
axis(2, at = seq(0,5,1) , cex.axis = 0.8, las=2)

mtext("10-trial bins", side = 1, line = 2.2, cex = 0.7)
mtext("Mean latency (sec)", side = 2, line = 2.2, cex = 0.7)
mtext("Day 1", side = 3, line = 0, cex = 0.8, font=2)

points(latencypersession[[1]][1,], pch=19, col="orange")
lines(latencypersession[[1]][1,], lwd=3, lty=3, col="orange")
points(latencypersession[[1]][2,], pch=19, col="orange")
lines(latencypersession[[1]][2,], lwd=3, col="orange")

error.bar <- function(x, y, upper, lower=upper, z, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90,col=z,lwd=1, code=3, length=length, ...)
}

error.bar(seq(1:10), latencypersession[[1]][1,], latencypersession[[6]][,1], latencypersession[[6]][,1], z="orange")
error.bar(seq(1:10), latencypersession[[1]][2,], latencypersession[[7]][,1], latencypersession[[7]][,1], z="orange")

#Day 5
plot.new()
plot.window(xlim = c(0,10), ylim = c(0,5))

axis(1, at = seq(0,10,1), cex.axis = 0.8, las=1)
axis(2, at = seq(0,5,1) , cex.axis = 0.8, las=2)

mtext("10-trial bins", side = 1, line = 2.2, cex = 0.7)
mtext("Mean latency (sec)", side = 2, line = 2.2, cex = 0.7)
mtext("Day 5", side = 3, line = 0, cex = 0.8, font=2)

points(latencypersession[[5]][1,], pch=19, col="purple")
lines(latencypersession[[5]][1,], lwd=3, lty=3, col="purple")
points(latencypersession[[5]][2,], pch=19, col="purple")
lines(latencypersession[[5]][2,], lwd=3, col="purple")

error.bar(seq(1:10), latencypersession[[5]][1,], latencypersession[[6]][,5],  latencypersession[[6]][,5], z="purple")
error.bar(seq(1:10), latencypersession[[5]][2,], latencypersession[[7]][,5], latencypersession[[7]][,5], z="purple")


#Legend:

plot.new()
plot.window(xlim = c(0,20), ylim = c(0,5))

lines(c(1,5), c(3,3), lwd=3, lty=3, col="black")
lines(c(1,5), c(2,2), lwd=3, col="black")
text(6, 3, labels="ITI latency", cex=0.8, adj=0)
text(6, 2, labels="CUE latency", cex=0.8, adj=0)
polygon(c(0,20,20,0),c(1,1,4,4))



######
# RASTER PLOTS OF ONE OF THE RATS (i.e. Days 1 and 5 of Rat 2: parseddata[[6]] and parseddata[[10]])
######

#Day 1

i <- 6   
intervalwindow<-15
current <- parseddata[[i]]
reverseidx <- c(nrow(current$trials):1)  
current$trials$idx <- c(1:nrow(current$trials))


plot.new()
plot.window(xlim = c(-intervalwindow,intervalwindow), ylim=c(1,(nrow(current$trials))))
segments(current$entries$Nent,reverseidx[current$entries$trialno],current$entries$Next,reverseidx[current$entries$trialno],lwd = 2)
abline(v = 0, lwd = 2, col = "red")

points(x= current$trials$latency[which(current$trials$reward ==T)], reverseidx[current$trials$idx[which(current$trials$reward ==T)]], col = "deepskyblue", pch = 19, cex=0.5)
points(x= current$trials$latency[which(current$trials$reward ==F)], reverseidx[current$trials$idx[which(current$trials$reward ==F)]], col = "red", pch = 19, cex=0.5)

axis(1, at = seq(-15,15,5), cex.axis = 0.8, las=1)
axis(2, at = seq(from= nrow(current$trials),to= 1, by=-10) , cex.axis = 0.8, las = 2)

mtext("Time from cue onset (s)", side = 1, line = 2.2, cex = 0.7)
mtext("Trial number", side = 2, line = 2.2, cex = 0.7)
mtext("Day 1", side = 3, line = 1, cex = 0.8, font=2)


#Day 5

i = 10   
intervalwindow=15
current = parseddata[[i]]
reverseidx = c(nrow(current$trials):1)  
current$trials$idx = c(1:nrow(current$trials))


plot.new()
plot.window(xlim = c(-intervalwindow,intervalwindow), ylim=c(1,(nrow(current$trials))))
segments(current$entries$Nent,reverseidx[current$entries$trialno],current$entries$Next,reverseidx[current$entries$trialno],lwd = 2)
abline(v = 0, lwd = 2, col = "red")

points(x= current$trials$latency[which(current$trials$reward ==T)], reverseidx[current$trials$idx[which(current$trials$reward ==T)]], col = "deepskyblue", pch = 19, cex=0.5)
points(x= current$trials$latency[which(current$trials$reward ==F)], reverseidx[current$trials$idx[which(current$trials$reward ==F)]], col = "red", pch = 19, cex=0.5)

axis(1, at = seq(-15,15,5), cex.axis = 0.8, las=1)
axis(2, at = seq(from= nrow(current$trials),to= 1, by=-10) , cex.axis = 0.8, las = 2)

mtext("Time from cue onset (s)", side = 1, line = 2.2, cex = 0.7)
mtext("Trial number", side = 2, line = 2.2, cex = 0.7)
mtext("Day 5", side = 3, line = 1, cex = 0.8, font=2)


#Legend
plot.new()
plot.window(xlim = c(0,16), ylim = c(0,5))

lines(c(0.5,2), c(4,4), lwd=3, col="black")
text(2.5, 4, labels="Head inside", cex=0.8, adj=0)

points(1,3, col = "deepskyblue", pch = 19)
text(2.5, 3, labels="Cue-evoked entry", cex=0.75, adj=0)

points(1,2, col = "red", pch = 19)
text(2.5, 2, labels="Missed trial", cex=0.8, adj=0) 

polygon(c(0,16,16,0),c(1,1,5,5))


mtext("Evolution of performance from day 1 to 5 of training", side = 3, line = 1, cex=1.5, outer = TRUE, font=2)
mtext("ITI and cue latency across trials", side = 3, line = -15, cex=0.9, outer = TRUE, col="darkgray", font=3, adj=0)
mtext("Raster plots of one of the rats (example)", side = 3, line = -27, cex=0.9, outer = TRUE, col="darkgray", font=3, adj=0)

