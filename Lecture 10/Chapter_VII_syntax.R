# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                   Section VII: Graphing                 #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 7.1 # 
# ----------------- #

var1<-rnorm(1000, 100, 10)
var2<-sort(rbeta(1000, 2,4))

#What if I want more than 1 plot per graph?
par(mfcol=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0))#How many plots are we going to include in the graph? c(1,2)-> 1 row, 2 columns of plot

#If I now plot a boxplot of var2, it's going to leave space for a second one.
boxplot(var2, notch=TRUE, col="dark green")


#And I just have to tell it what I want to plot and he'll put it in place
plot (density(var2), main="", lty=2, col="dark blue", lwd=1)

#Notice that you have to call them in the correct order! Every time you call one, it's going to fill left column, right column...


demo(graphics)
install.packages("rgl")
library(rgl)
demo(lollipop3d)
demo(bivar)

#Only windows?
windows()
windows(3,10)

#I would like a new window
dev.new()
dev.new(3,10)#This doesn-t work well, not like windows(3,10)
dev.new(width=3, height=10)#Now it works
dev.off() #It is going to kill whatever device is active (?). It's gone,so be careful.
dev.new()#It opens up a new device

#Let's create a matrix of random 1000 values (500 rows, 2 columns)
mat1 <- matrix(rnorm(1000),500)
#The default for plotting this data is a scatterplot with each column as coordinate axes
plot(mat1)
dev.off(mat1)
#But let's make it pretty (customize). Go to the documentation to see all you can do
?plot
plot(mat1, #Object to be plotted
     type="p", #how to plot it (p is for point) (instead of line...)
     pch=15, #point character. There is a code (15 is bold square)
     lty=3, #line
     lwd=2, # line width
     xlab="Variable1", #Label of x axis
     ylab="cheesecake", #Label of y axis
     main="Scatterplot of rubbish data", #Main title
     cex=0.5, #Character expansion of points
     cex.lab=2) #Character expansion of labels

example(pch) #It gives you examples of these different arguments


library(foreign) #package that lets us bring in .csv files
#Click in the console!!!!!!!!! Otherwise it doesnt work

#Import my data
dat7 <- read.spss("band.sav", to.data.frame=TRUE)
hist(dat7$sales, col="light blue")#You don't need to know the colors, there are codes too
hist(dat7$sales, col=colors()[644])#You index it from the colors matrix

plot(factor(dat7$performances), 
     density=15, #I don't want to color sth but to shade it (in this case 15 lines per sq inch)
     col= c("black", "pink", "blue"), #imagine I want it to have a different color for each bar
     main="Bargraph of Performance Frequencies")

boxplot(dat7$sale)#Default. For the variable sales, it's going to give me a nonparametric plot with the quintiles
boxplot(dat7$sale~dat7$performances #This means you want to look at the sales distributed by performance 
        )
#And then you can write names, put colors, etc.

pie(c(35,65), c("Sad","Happy"), clockwise=TRUE, col=c("blue","green"))

# How can I use my graphs? I click in it, say file>save as> png or jpeg at 100% quality

names(par()) #par is a ghost function that is in the background and controls things. Run it, you'll see all the things you can control

#Let's go back to our scatterplot matrix mat1
plot(mat1)

#Let's use the par function.

par(mar=c(5,5,1,1)) #mar (margins) size: It goes clockwise starting from the bottom
#The par function changes the default values of the device, so I have to use it before I plot my plot

plot(mat1, #Object to be plotted
     type="p", #how to plot it (p is for point) (instead of line...)
     pch=15, #point character. There is a code (15 is bold square)
     lty=3, #line
     lwd=2, # line width
     xlab="Variable1", #Label of x axis
     ylab="cheesecake", #Label of y axis
     cex=0.5, #Character expansion of points
     cex.lab=2) #Character expansion of labels

#See what happens if you didnt use that par function
dev.off(mat1)
plot(mat1, #Object to be plotted
     type="p", #how to plot it (p is for point) (instead of line...)
     pch=15, #point character. There is a code (15 is bold square)
     lty=3, #line
     lwd=2, # line width
     xlab="Variable1", #Label of x axis
     ylab="cheesecake", #Label of y axis
     cex=0.5, #Character expansion of points
     cex.lab=2) #Character expansion of labels

#Notice it's different (the margins)


colors()
?jpeg

jpeg("boxandpie.jpeg",quality=100, width=780)
	par(mar=c(4.5,4.5,1,1), mfcol=c(1,2))
	boxplot(dat7$sale~dat7$performances, names=c("4 Concerts","5 Concerts","6 Concerts"),col=c(colors()[123],colors()[16],colors()[5]))
	pie(c(35,65), c("Sad","Happy"), clockwise=TRUE, col=c("blue","green"))
dev.off()

# ------------------ #
# Demonstration 7.2  #
# ------------------ #

library(MASS)
datain <- rbeta(1000,4, 2)
datain2 <- rnorm(1000,mean=4, sd=1)

kobj <- kde2d(datain,datain2, h=c(.1,.1))
persp(kobj, theta=60, phi=20, ticktype="detailed")

kobj <- kde2d(datain,datain2, h=c(1,1))
persp(kobj, theta=60, phi=20, ticktype="detailed")

kobj <- kde2d(datain,datain2)
persp(kobj, theta=60, phi=20, ticktype="detailed")

for(i in 1:1000){
	persp(kobj, theta=i, ticktype="detailed")
}

for(i in 1:1000){
	persp(kobj, phi=i, ticktype="detailed")
}

for(i in 1:1000){
	persp(kobj, theta=i, phi=-i, ticktype="detailed",
		xlab="Datain 1", ylab="Datain 2", zlab="Density",
		col=colors()[128], shade=.7)
}

install.packages("rgl")
library("rgl")
?rgl

xdat <- rnorm(100)
ydat <- rnorm(100)
zdat <- rnorm(100)

rgl.open()
bg3d(col="light yellow")
?points3d
points3d(xdat, ydat, zdat, cex=5)

bbox3d(color=c("#333377","black"), emission="#333377", 
         specular="#3333FF", shininess=2, alpha=2.8,
		xat=seq(from=-3, to=3, by=1),
		yat=seq(from=-3, to=3, by=1),
		zat=seq(from=-3, to=3, by=1), 
		xlab=c("C", "B", "A", "0", "A", "B", "C"),
		ylab=c("C", "B", "A", "0", "A", "B", "C"),
		zlab=c("C", "B", "A", "0", "A", "B", "C")
		# xlab="Coordinate 1",
		# ylab=expression(paste(sqrt(frac(1,x-beta)),sep="")),
		# zlab="Test"
)

title3d("Normal Theory Value","RGL Demonstration","X","Y","Z", col="black")
title3d("Normal Theory Value",col="black", cex=1)

rgl.close()

# ----------------- #
# Demonstration 7.3 # 
# ----------------- #

install.packages("TeachingDemos") 
library(TeachingDemos)

set.seed(17)
faces(matrix(sample(1:1000,128,),16,15),main="random faces") 
?faces
faces(matrix(sample(1:1000, 16*15),16,15),main="random faces") 

test <- 	cbind(rep(1,25),rep(1,25),rep(1,25),rep(1,25),
		rep(1,25),sort(rep(1:5,5)),rep(1,25),sort(rep(1:5,5)),
		rep(1:5,5),rep(1:5,5)
		)

par(oma=c(1,1,1,1), mar=c(4,6,4,4))
faces(test,fill=FALSE, main="", labels=c())
mtext(side=3, "SAT Composite Score", font=2, line=3)
mtext(side=2, "First AP Grade", font=2, line=5)

axis(3,at=locator(1)[1], label="600-790", cex=1.1, font=2)
axis(3,at=locator(1)[1], label="800-990", cex=1.1, font=2)
axis(3,at=locator(1)[1], label="1000-1190", cex=1.1, font=2)
axis(3,at=locator(1)[1], label="1200-1390", cex=1.1, font=2)
axis(3,at=locator(1)[1], label="1400-1600", cex=1.1, font=2)
axis(2,at=locator(1)[2], label="1", cex=1.1, font=2, las=2, line=2.2)
axis(2,at=locator(1)[2], label="2", cex=1.1, font=2, las=2, line=2.2)
axis(2,at=locator(1)[2], label="3", cex=1.1, font=2, las=2, line=2.2)
axis(2,at=locator(1)[2], label="4", cex=1.1, font=2, las=2, line=2.2)
axis(2,at=locator(1)[2], label="5", cex=1.1, font=2, las=2, line=2.2)

# ------------------ #
# Demonstration 7.4  #
# ------------------ #

#Data: how students changed their self/reported ethnicity
ped <- read.csv("ethn_data.csv", row.names=1)
ped*100

# Outline and frame

par(oma=c(0,0,0,0), mar=c(1,1,1,1),yaxs="i")
par(bg = colors()[129])
plot(1,1,type="n", #"n":Write this as if you were creating a graph, but you are not
     xlim=c(0,12),ylim=c(0,10), #I'm creating a customized coordinate system
     axes=FALSE, ylab="", xlab="", main="")

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white") #I want a nice white rectangle in which I can plot sth

# Reference Axes

segments(1.2,2,10.8,2, lwd=4) #Static segments: from 1.2 to 10.8 in the x axis
segments(1.2,1.8,1.2,2.2, lwd=4)
segments(10.8,1.8,10.8,2.2, lwd=4)
segments(1.2,8,10.8,8, lwd=4)
segments(1.2,7.8,1.2,8.2, lwd=4)
segments(10.8,7.8,10.8,8.2, lwd=4)

# Labeling

text(1,2, "Junior", cex=1, font=2, pos=2)
text(11,2, "2009", cex=1, font=2, pos=4)
text(1,8, "Sophomore", cex=1, font=2, pos=2)
text(11,8, "2008", cex=1, font=2, pos=4)

text(c(4,5,6,9,10), c(8.4,8.4,8.4,8.4,8.4,8.4,8.4,8.4,8.4), row.names(ped)[c(3,4,5,8,9)], cex=.9, pos=3)
text(c(4,5,6,9,10), c(2,2,2,2,2,2,2,2,2), row.names(ped)[c(3,4,5,8,9)], cex=.9, pos=1)
text(2,8.7,"No", cex=.9, pos=3);text(2,8.4,"Response", cex=.9, pos=3)
text(3,8.7,"American", cex=.9, pos=3);text(3,8.4,"Indian", cex=.9, pos=3)
text(7,8.7,"Puerto", cex=.9, pos=3);text(7,8.4,"Rican", cex=.9, pos=3)
text(8,8.7,"Other", cex=.9, pos=3);text(8,8.4,"Hispanic", cex=.9, pos=3)
text(2,2,"No", cex=.9, pos=1);text(2,1.7,"Response", cex=.9, pos=1)
text(3,2,"American", cex=.9, pos=1);text(3,1.7,"Indian", cex=.9, pos=1)
text(7,2,"Puerto", cex=.9, pos=1);text(7,1.7,"Rican", cex=.9, pos=1)
text(8,2,"Other", cex=.9, pos=1);text(8,1.7,"Hispanic", cex=.9, pos=1)

#This gsamp is a scaling factor. I'm interested in the students who changed their mind, which is a small percentage of all the students. So I'm going to look at the percentage of the percentage of students who changed their minds.
gsamp <- c(22984,4170,69758,94742,50860,12681,59075,404385,27088)

text(2,8.05, paste(round((1-round(ped[1,1],2))*100,0),"%"," of ",gsamp[1],sep=""), cex=.7, pos=3)
text(3,8.05, paste(round((1-round(ped[2,2],2))*100,0),"%"," of ",gsamp[2],sep=""), cex=.7, pos=3)
text(4,8.05, paste(round((1-round(ped[3,3],2))*100,0),"%"," of ",gsamp[3],sep=""), cex=.7, pos=3)
text(5,8.05, paste(round((1-round(ped[4,4],2))*100,0),"%"," of ",gsamp[4],sep=""), cex=.7, pos=3)
text(6,8.05, paste(round((1-round(ped[5,5],2))*100,0),"%"," of ",gsamp[5],sep=""), cex=.7, pos=3)
text(7,8.05, paste(round((1-round(ped[6,6],2))*100,0),"%"," of ",gsamp[6],sep=""), cex=.7, pos=3)
text(8,8.05, paste(round((1-round(ped[7,7],2))*100,0),"%"," of ",gsamp[7],sep=""), cex=.7, pos=3)
text(9,8.05, paste(round((1-round(ped[8,8],2))*100,0),"%"," of ",gsamp[8],sep=""), cex=.7, pos=3)
text(10,8.05, paste(round((1-round(ped[9,9],2))*100,0),"%"," of ",gsamp[9],sep=""), cex=.7, pos=3)

# text(2,8.05, paste("n = ", round((1-ped[1,1])*gsamp[1],0),sep=""), cex=.9, pos=3)
# text(3,8.05, paste("n = ", round((1-ped[2,2])*gsamp[2],0),sep=""), cex=.9, pos=3)
# text(4,8.05, paste("n = ", round((1-ped[3,3])*gsamp[3],0),sep=""), cex=.9, pos=3)
# text(5,8.05, paste("n = ", round((1-ped[4,4])*gsamp[4],0),sep=""), cex=.9, pos=3)
# text(6,8.05, paste("n = ", round((1-ped[5,5])*gsamp[5],0),sep=""), cex=.9, pos=3)
# text(7,8.05, paste("n = ", round((1-ped[6,6])*gsamp[6],0),sep=""), cex=.9, pos=3)
# text(8,8.05, paste("n = ", round((1-ped[7,7])*gsamp[7],0),sep=""), cex=.9, pos=3)
# text(9,8.05, paste("n = ", round((1-ped[8,8])*gsamp[8],0),sep=""), cex=.9, pos=3)
# text(10,8.05, paste("n = ",round((1-ped[9,9])*gsamp[9],0),sep=""), cex=.9, pos=3)

# Rescaling percentages for lines. This just rescales the percentages so that theyre reflecting the percentage of the people who changed their minds (not all the people in the category)

nores <- round((ped[1,2:9]/sum(ped[1,2:9]))*100,0)
indian <- round((ped[2,c(1,3:9)]/sum(ped[2,c(1,3:9)]))*100,0)
asian <- round((ped[3,c(1:2,4:9)]/sum(ped[3,c(1:2,4:9)]))*100,0)
black <- round((ped[4,c(1:3,5:9)]/sum(ped[4,c(1:3,5:9)]))*100,0)
mexican <- round((ped[5,c(1:4,6:9)]/sum(ped[5,c(1:4,6:9)]))*100,0)
puerto <- round((ped[6,c(1:5,7:9)]/sum(ped[6,c(1:5,7:9)]))*100,0)
hispanic <- round((ped[7,c(1:6,8:9)]/sum(ped[7,c(1:6,8:9)]))*100,0)
white <- round((ped[8,c(1:7,9)]/sum(ped[8,c(1:7,9)]))*100,0)
other <- round((ped[9,1:8]/sum(ped[9,1:8]))*100,0)

# Comparing rounded percentage estimates

c(nores,indian,asian,black,mexican,puerto,hispanic,white,other)
length(c(nores,indian,asian,black,mexican,puerto,hispanic,white,other))
min(as.numeric(c(nores,indian,asian,black,mexican,puerto,hispanic,white,other)))
max(as.numeric(c(nores,indian,asian,black,mexican,puerto,hispanic,white,other)))

# Plotting percentage lines

#I'm going to create a function to plot this
#Im going to use the color and thickness as an indicator of the value (like a heat map)
plotlines <- function(object, numor) {
	for(j in 1:length(object)) {
		if(numor==1) {segx2 <- c(3,4,5,6,7,8,9,10)}
		if(numor==2) {segx2 <- c(2,4,5,6,7,8,9,10)}
		if(numor==3) {segx2 <- c(2,3,5,6,7,8,9,10)}
		if(numor==4) {segx2 <- c(2,3,4,6,7,8,9,10)}
		if(numor==5) {segx2 <- c(2,3,4,5,7,8,9,10)}
		if(numor==6) {segx2 <- c(2,3,4,5,6,8,9,10)}
		if(numor==7) {segx2 <- c(2,3,4,5,6,7,9,10)}
		if(numor==8) {segx2 <- c(2,3,4,5,6,7,8,10)}
		if(numor==9) {segx2 <- c(2,3,4,5,6,7,8,9)}

		if(object[j] == 0) {lcol = "white"; lthi = 0}
		if(object[j] > 0 & object[j] <= 10) {lcol = colors()[109]; lthi=1}
		if(object[j] > 10 & object[j] <= 20) {lcol = colors()[124]; lthi=1.5}
		if(object[j] > 20 & object[j] <= 30) {lcol = colors()[514]; lthi=2}
		if(object[j] > 30 & object[j] <= 40) {lcol = colors()[518]; lthi=2.5}
		if(object[j] > 40 & object[j] <= 50) {lcol = colors()[652]; lthi=3}
		if(object[j] > 50 & object[j] <= 60) {lcol = colors()[142]; lthi=3.5}
		if(object[j] > 60 & object[j] <= 70) {lcol = colors()[499]; lthi=4}
		if(object[j] > 70) {lcol = colors()[630]; lthi=4.5}

		segments(numor+1, 8, segx2[j], 2, lwd=lthi, col=lcol)

	}
}

#Now I actually have to plot this function applied to each category
plotlines(nores,1)
plotlines(indian,2)
plotlines(asian,3)
plotlines(black,4)
plotlines(mexican,5)
plotlines(puerto,6)
plotlines(hispanic,7)
plotlines(white,8)
plotlines(other,9)

# Drawing mini segments (on the axes, to mark each category)

segments(c(2,3,4,5,6,7,8,9,10), c(1.9,1.9,1.9,1.9,1.9,1.9,1.9,1.9,1.9),
	c(2,3,4,5,6,7,8,9,10),c(2.1,2.1,2.1,2.1,2.1,2.1,2.1,2.1,2.1), lwd=1)
segments(c(2,3,4,5,6,7,8,9,10), c(7.9,7.9,7.9,7.9,7.9,7.9,7.9,7.9,7.9),
	c(2,3,4,5,6,7,8,9,10),c(8.1,8.1,8.1,8.1,8.1,8.1,8.1,8.1,8.1), lwd=1)

# Legend and title. It's going to be a series of boxes with the color and what the color stands for

polygon(c(1.5,1.5,2.5,2.5), c(.7,1.2,1.2,.7), col="white", density=-.5, border=TRUE, lty=1)
polygon(c(2.5,2.5,3.5,3.5), c(.7,1.2,1.2,.7), col=colors()[109], density=-.5, border=TRUE, lty=1)
polygon(c(3.5,3.5,4.5,4.5), c(.7,1.2,1.2,.7), col=colors()[124], density=-.5, border=TRUE, lty=1)
polygon(c(4.5,4.5,5.5,5.5), c(.7,1.2,1.2,.7), col=colors()[514], density=-.5, border=TRUE, lty=1)
polygon(c(5.5,5.5,6.5,6.5), c(.7,1.2,1.2,.7), col=colors()[518], density=-.5, border=TRUE, lty=1)
polygon(c(6.5,6.5,7.5,7.5), c(.7,1.2,1.2,.7), col=colors()[652], density=-.5, border=TRUE, lty=1)
polygon(c(7.5,7.5,8.5,8.5), c(.7,1.2,1.2,.7), col=colors()[142], density=-.5, border=TRUE, lty=1)
polygon(c(8.5,8.5,9.5,9.5), c(.7,1.2,1.2,.7), col=colors()[499], density=-.5, border=TRUE, lty=1)
polygon(c(9.5,9.5,10.5,10.5), c(.7,1.2,1.2,.7), col=colors()[630], density=-.5, border=TRUE, lty=1)

text(2, .95, "0%", cex=.8, font=2)
text(3, .95, "1-10%", cex=.8, font=2)
text(4, .95, "11-20%", cex=.8, font=2)
text(5, .95, "21-30%", cex=.8, font=2)
text(6, .95, "31-40%", cex=.8, font=2)
text(7, .95, "41-50%", cex=.8, font=2)
text(8, .95, "51-60%", cex=.8, font=2)
text(9, .95, "61-70%", cex=.8, font=2)
text(10, .95, "> 70%", cex=.8, font=2)

mtext("Changes in self-reported ethnic identification of test takers",
	side=3, line=-2.4, cex=1.6, font=2)

#mtext means marginal texts














































