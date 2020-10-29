# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                    Section IV: Using R                  #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 4.1 # 
# ----------------- #

assign("obj1", 2+3)
obj1
obj2 = 5
obj3 <- 5 # Recommended
obj2; obj3
?eval
eval(2+sqrt(4))
get("obj3")
is.numeric(obj2)
is.character(obj2)
as.character(obj2)
paste(obj1, obj2, obj3)

# ----------------- #
# Demonstration 4.2 # 
# ----------------- #

1:6
seq(from=2, to=5, by=.2)
sample(1:10, 100, replace=TRUE)
gender
gender <- c("M", "F", "M", "F", "F")
runif(10)
rnorm(10)
matrix(1:25, 5, 5)
t(matrix(1:25, nrow=5))
diag(10)
mat1 <- matrix(runif(15),,3)
mat1
rbind(mat1, c(20, 20, 20))
cbind(gender,mat1)
data.frame(cbind(gender,mat1))
dat1 <- data.frame(gender,mat1)
colnames(dat1) <- c("Gender", "Uniform 1", "Uniform 2", "Uniform 3")
rownames(dat1) <- c("Cas1", "Cas2", "Cas3", "Cas4", "Cas5")
dat1
proc.time()
now <- Sys.time()
difftime(Sys.time(),now)
func1 <- function(x) {x+2}
func1(5)
func1(1:6)

# ----------------- #
# Demonstration 4.3 # 
# ----------------- #

mode(gender)
typeof(gender)
class(gender)
length(gender)
mode(dat1)
typeof(dat1)
class(dat1)
length(dat1)
dim(dat1)
object.size(dat1)
format.info(gender)
format.info(runif(10))
summary(gender)
levels(factor(gender))

#Multiple univariate regression with two predictors (x1 and x2)

model1<-lm(dat1$X1~dat1$x2+dat1$gender)
?lm

summary(dat1)
names(dat1)
labels(dat1)
attributes(dat1)
attr(dat1, "created") <- date()
names(attributes(dat1))
dat1
dat1/2
t(dat1[,2:4])
as.matrix(dat1[,2:4])%*%as.matrix(t(dat1[,2:4]))
invar <- as.matrix(t(dat1[,2:4]))%*%as.matrix(dat1[,2:4])
diag(invar)*diag(3)

# ----------------- #
# Demonstration 4.4 # 
# ----------------- #

objects()
ls()
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
table(dat1$Gender)
colSums(dat1[,2:4])
rowSums(dat1[,2:4])
ifelse(dat1$"Uniform 2" < .5, 1, 0) 
ifelse(dat1$"Uniform 2" > .9, NA, dat1$"Uniform 2") 
for(i in 1:5) {sqrt(dat1[i,3])}
allsqrt <- c()
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
apply(dat1[,2:4], c(1,2), function(x) {ifelse(x > .60, NA, x)})
colSums(dat1[,2:4])
repeat{
	thiscell <- sample(1:5,1)
	dat1[thiscell,3] <- dat1[thiscell,3]+rnorm(1)
	if (sum(dat1[,3]) > 10) {break}
}
colSums(dat1[,2:4])

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
source("D:/Programs/bivarfreqfunction.R")
objects()
findfreq
findfreq(rbeta(1000,2,4),rbeta(1000,3,1))

# ----------------- #
# Demonstration 4.7 # 
# ----------------- #

?read.table
dat3 <- read.table("D:/Data/scores.txt")
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

dat4 <- read.table("D:/Data/time_data.csv")
dim(dat4)
names(dat4)
mode(dat4); class(dat4)
dat4[1:10,]
dat4 <- read.csv("D:/Data/time_data.csv")
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
dat5 <- read.fwf("D:/Data/IRT_report.txt",
	skip=49, widths=rep(c(4,-3),11))
dat5
dat5 <- t(dat5)
dat5[,1]%*%t(dat5[,2])

dat6 <- read.fwf("D:/Data/IRT_report.txt",
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
dat7 <- read.spss("D:/Data/band.sav")
dim(dat7)
length(dat7)
names(dat7)
mode(dat7); class(dat7)
dat7
dat7 <- read.spss("D:/Data/band.sav", to.data.frame=TRUE)
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

write.table(mat3, "C:/mat3.txt")
write.csv(mat3, "C:/mat3.csv")

dput(mat3, "C:/mat3_put.R")
dump(list=c("mat3","func3"), "C:/matfunc_dump.R")
save(list=c("mat3","func3"), file="C:/mat3_save.R")

exists("mat3");exists("func3")
rm(mat3, func3)
exists("mat3");exists("func3")
load(file="C:/mat3_save.R")
exists("mat3");exists("func3")
mode(mat3);dim(mat3)
rm(mat3, func3)
exists("mat3");exists("func3")
source("C:/matfunc_dump.R")
exists("mat3");exists("func3")
rm(mat3, func3)
mat4 <- dget("C:/mat3_put.R")
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

