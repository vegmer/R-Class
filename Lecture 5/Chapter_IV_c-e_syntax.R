# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                    Section IV: Using R                  #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 4.4 # 
# ----------------- #

objects()
ls.str()
find("dat1")
exists("dat1")
apropos("dat")
apropos("dat1")
getAnywhere("dat1")
browseEnv()
exists("func1")
rm(func1); exists("func1")
options()
names(options())
options()[9]

# ----------------- #
# Demonstration 4.5 # 
# ----------------- #

dat1
dat1 <- dat1[order(dat1[,1]),]
sort(dat1$"Uniform 2")
sort(dat1$"Uniform 2", decreasing=TRUE)
dat1[which(dat1$Gender=="M"),]
match("M",dat1$Gender)
"M" %in% dat1$Gender
gentab <- table(dat1$Gender); gentab
newmat <- matrix(sample(1:10,100,replace=TRUE),10,)

#This is to replace values
newmat2 <- sub("10", "Cheese", newmat, fixed=TRUE)
sub("ee", "!TOT!", newmat2)
matrix(as.numeric(sub("10", "-99", newmat, fixed=TRUE)),10,)

colSums(dat1[,2:4])
rowSums(dat1[,2:4])
colMeans(dat1)
colMeans(dat1[,-1])
rowMeans(dat1[,-1])
ifelse(dat1$"Uniform 2" < .5, 1, 0) 
ifelse(dat1$"Uniform 2" > .9, NA, dat1$"Uniform 2") 



#LOOP
#For each index (i) between 1 to 5 an do whatever is in the curly brackets

for(i in 1:5) {
  sqrt(dat1[i,3])
}

#But where is it? We created an i object. Let's see what's in it. Nothing, number 5. 
#We have to tell R where to put it!!!


#Empty substantiation of an object (allsqrt is Null until I change it)
allsqrt <- c()

#Now you assign that object to the loop
for(i in 1:5) {
	allsqrt <- c(allsqrt,sqrt(dat1[i,3]))
}


allsqrt
for(i in 1:5) {
	if(dat1[i,2] > .5) {
		dat1[i,1] <- NA
	}
}
dat1


#Instead of doing a loop, you can tell me what you want to do and to what
#you want to apply it to. It's more efficient than the loop.

#Consider the two margins (margin of the rows=1, margin of the columns=1)//> c(1,2)
#in the object dat 1 columns 2 to 4 (dat1[,2:4])
#To that, I want to apply that all the parameters >.60 become NA //> function(x){ifelse(x>.60,NA,x)}

apply(dat1[,2:4], c(1,2), function(x) {ifelse(x > .60, NA, x)})

colSums(dat1[,2:4])

#REPEAT COMMAND: You keep going until some criteria is satisfied.
repeat{
	thiscell <- sample(1:5,1)
	dat1[thiscell,3] <- dat1[thiscell,3]+rnorm(1)
	if (sum(dat1[,3]) > 10) {break}
}


#Let-s change it a little.
cnt<-0

repeat{
  cnt<-cnt+1
  thiscell <- sample(1:5,1)
  dat1[thiscell,4] <- dat1[thiscell,4]+rnorm(1)
  message(paste("iteration",cnt,sep" "))
  if (sum(dat1[,4]) > 10) {break}


colSums(dat1[,-1]);dat1

# ----------------- #
# Demonstration 4.6 # 
# ----------------- #

library()
getCRANmirrors()
names(getCRANmirrors())
install.packages("rela")
library(rela)
?rela
citation("rela")
ls("package:rela")
?paf
example(paf)
apropos("paf")
names(paf.belt)
summary(paf.belt)
plot(paf.belt2)
detach("package:rela", unload = TRUE)
?rela

ls()
source("E:/bivarfreqfunction.R")
objects()
findfreq
findfreq(rbeta(1000,2,4),rbeta(1000,3,1))

# ----------------- #
# Demonstration 4.7 # 
# ----------------- #

?read.table
dat3 <- read.table("E:/scores.txt")
dim(dat3)
names(dat3)
mode(dat3); class(dat3)
dat3[1:10,]
typeof(dat3[,1])
typeof(dat3[,2])
typeof(dat3[,3])
summary(dat3)
dat3[,1] <- factor(dat3[,1], levels=c(0,1), labels=c("yes","no"))
summary(dat3)
dat3[1:10,]
as.numeric(dat3[,1])

dat4 <- read.table("E:/time_data.csv")
dim(dat4)
names(dat4)
mode(dat4); class(dat4)
dat4[1:10,]
dat4 <- read.csv("E:/time_data.csv")
dim(dat4)
names(dat4)
mode(dat4); class(dat4)
dat4[1:10,]
typeof(dat4[,1])
typeof(dat4[,2])
typeof(dat4[,3])
class(dat4[,1])
summary(dat4)

?read.fwf
dat5 <- read.fwf("E:/IRT_report.txt",
	skip=49, widths=rep(c(4,-3),11))
dat5
dat5 <- t(dat5)
dat5[,1]%*%t(dat5[,2])

dat6 <- read.fwf("E:/Data/IRT_report.txt",
		skip=34, widths=list(c(-15,7,-3,7,-3,7,
		-3,7,-3,7,-3,7,-3,7,-3,7,-3,7,-3,7),
		c(-1)),n=3)
dim(dat6)
names(dat6)
mode(dat6); class(dat6)
dat6
typeof(dat6[,1])
rownames(dat6) <- c("Discrimination","Difficulty","Guessing")
summary(t(dat6))

install.packages("foreign")
library(foreign)
?read.spss
dat7 <- read.spss("E:/band.sav")
dim(dat7)
length(dat7)
names(dat7)
mode(dat7); class(dat7)
dat7
dat7 <- read.spss("E:/band.sav", to.data.frame=TRUE)
dim(dat7)
names(dat7)
mode(dat7); class(dat7)
dat7[1:10,]

install.packaged("RODBC")
library(RODBC)
?sqlFetch

# ----------------- #
# Demonstration 4.8 # Saving things. Objects and/or statistical analysis.
# ----------------- #


mat3 <- matrix(runif(10000),100,100)
#I want to create the labels for the columns
matlabs <- c()
for(j in 1:100){
	matlabs <- c(matlabs, paste("Item",j,sep=" "))
}
#I don-t really need the loop, I can do a paste(...)

colnames(mat3) <- matlabs
mat3[1:3,1:3]

#Author
attr(mat3,"author") <- "Michael Chajewski"

#Object size is memory it-s taking up.
object.size(mat3)

#If I write func3, we reduce the size of the object. Its because the function takes up less space
func3 <- function(x,y) {
	(runif(1)*x)/runif(1)*y
}
object.size(func3)
#If you have a function that creates an object (matrix in sudoku), it-s cheaper to store the function than the data it creates.

#It-s a way of saving that data.This way, it makes a table
write.table(mat3, "E:/mat3.txt")
write.csv(mat3, "E:/mat3.csv")


#This will export text or syntax and the function.You can open it easily with notepad.
dput(mat3, "E:/mat3_put.R")

#This will redo everything (objects and functions).You can select what objects/functions you want to save.
#You can open it easily with notepad too. It saves the function in the bottom too.
#He remembers the name of the objects, dput doesn't.
dump(list=c("mat3","func3"), "E:/matfunc_dump.R")

#This won't give you a text file. It creates a computer system file, notepad can't open it.
#BEnefit: it generates a hugely compressed file, so it's great if you have big files.
save(list=c("mat3","func3"), file="E:/mat3_save.R")

ls()
#Lets remove mat3 and func3 from global environment
rm(mat3, func3)
ls()

#Look at notes
exists("mat3");exists("func3")
rm(mat3, func3)
exists("mat3");exists("func3")
load(file="E:/mat3_save.R")
exists("mat3");exists("func3")
mode(mat3);dim(mat3)
rm(mat3, func3)
exists("mat3");exists("func3")
source("E:/matfunc_dump.R")
exists("mat3");exists("func3")
rm(mat3, func3)
mat4 <- dget("E:/mat3_put.R")
exists("mat3");exists("func3")
mode(mat4);dim(mat4)

attach(mat4)
mat4 <- data.frame(mat4)
attach(mat4)
summary(Item.100)
detach(mat4)
summary(Item.100)
attach(dat7)
names(dat7)
summary(sales)

