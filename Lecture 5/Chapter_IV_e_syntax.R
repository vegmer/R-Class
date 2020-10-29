# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                    Section IV: Using R                  #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 4.7 # 
# ----------------- #

#HOW TO DEAL WITH DATA!
#Look at data "scores". Its just a tabulated text file, so it reads it easily as a table.
#If variables are separated by sth else (e.g. coma). In that case you need the function
#read.csv!

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

#This is when the data are separated by a "fixed wide...".
?read.fwf
dat5 <- read.fwf("E:/IRT_report.txt",
	skip=49, widths=rep(c(4,-3),11))#"skip 49 lines, read 4, not 3, do this 11 times
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
# Demonstration 4.8 #
# ----------------- #

mat3 <- matrix(runif(10000),100,100)
matlabs <- c()
for(j in 1:100){
	matlabs <- c(matlabs, paste("Item",j,sep=" "))
}
colnames(mat3) <- matlabs
mat3[1:3,1:3]
attr(mat3,"author") <- "Michael Chajewski"
object.size(mat3)
func3 <- function(x,y) {
	(runif(1)*x)/runif(1)*y
}
object.size(func3)

write.table(mat3, "E:/mat3.txt")
write.csv(mat3, "E:/mat3.csv")

dput(mat3, "E:/mat3_put.R")
dump(list=c("mat3","func3"), "E:/matfunc_dump.R")
save(list=c("mat3","func3"), file="E:/mat3_save.R")

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

