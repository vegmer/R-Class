# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                Additional programming topics            #
# ------------------------------------------------------- #

# Reading in data

dat <- read.table("/Users/mercedesvegavillar/Dropbox/CUNY/YEAR 3/Spring 2014/R Class/Lecture 11/glm_demo_data.txt", header=TRUE)
summary(dat)
attach(dat)

# ----------------- #
# BOOTSTRAP EXAMPLE # Bootstrap tells you, if you had more data, would my analysis be similar?
# ----------------- #

# The HDAC SE

N <- length(HDAC)
HDAC.SE <- sd(HDAC)/sqrt(N)

# 95% D.C.I.
?qt
t.crit <- qt(.975, df=N-1)
HDAC.DCI <- cbind(	Lower = mean(HDAC) - t.crit * HDAC.SE,
				Mean = mean(HDAC),
				Upper = mean(HDAC) + t.crit * HDAC.SE,
				SE=HDAC.SE)
rownames(HDAC.DCI) <- "HDAC"
HDAC.DCI #THis spits out my confidence interval

# Bootstrap function. 
    #Two decisions you have to take when bootstrapping: 
      #how many samples you want to take
      #how big do I want the samples to be? The most appropriate sample size: it should be proportional to the one you got (24 in this case).

bootstrap <- function(var, bn, sizen){
	b.means <- mean(var)	
	for(i in 1:bn){
		var.2 <- sample(var, sizen, replace=TRUE)
		b.means <- c(b.means,mean(var.2))
    
		pie(c(i,bn-i),c(paste("Done",i),
			paste("Running",bn-i)),col=c("green","grey"), clockwise=TRUE)
	}
	dev.off()
	b.means
}

# Bootstrap C.I.

boot.out <- bootstrap(HDAC, 999, 1000)

HDAC.DCI <- rbind(	HDAC.DCI,
		  cbind(	Lower = sort(boot.out)[round(length(boot.out)*.025,0)],
				Mean = mean(boot.out),#This is mu, bc it's the mean of the population
				Upper = sort(boot.out)[round(length(boot.out)*.975,0)],
				SE = sd(boot.out))
)

rownames(HDAC.DCI) <- c("HDAC","Boot"); HDAC.DCI

#We now can compare them: HDAC original sample is pretty representative of the population.

# ------------------ #
# REGRESSION EXAMPLE #
# ------------------ #

# Simple linear regression

lm.npm.hdac <- lm(NPM ~ HDAC) #GLM funcion regressing NPM on to HDAC

names(lm.npm.hdac)
lm.npm.hdac
summary(lm.npm.hdac) #p value is very very small. HDAC is a strong predictor of NPM

# Full multiple regression model

    #I-m predicting NPM from a bunch of variables (equation would be NPM=Intercept (beta 0)+beta1HDAC+beta2AVP...)
    #NPM is a real variable. I want to create a new DV that if I regress that on those predictors, I want it to be such a way such that the coefficients are the same.

lm.npm.all <- lm(NPM ~ HDAC + AVP + Count + Group) #Now I'm going to make my model more complex. I want to predict NPM from all those other things.
summary(lm.npm.all) #THe group category should be treated as such but it-s not. Let's pretend is a numeric category.Usually, categorical variables in models change the intercept (a different one for each group)

# Let's create a new DV that
# will have the same regression coefficients 
# but is artificailly created

new.DV <-	lm.npm.all$coef[1] +
		lm.npm.all$coef[2]*HDAC +
		lm.npm.all$coef[3]*AVP +
		lm.npm.all$coef[4]*Count +
		lm.npm.all$coef[5]*Group

summary(NPM)
summary(new.DV)

# Comparing the two DVs

par(mar=c(4.5,4.5,1,1))
boxplot(NPM, new.DV, names=c("NPM","(Artifical)"), col=c("darkgreen","purple"))

summary.comp <- cbind(unlist(by(NPM, Group, summary)),unlist(by(new.DV, Group, summary)))
colnames(summary.comp) <- c("NPM","New.DV")

par(mar=c(4.5,4.5,1,1))
plot(NPM, HDAC, pch=20, col="darkgreen", cex=1.5)
points(new.DV, HDAC, pch=15, col="purple", cex=1.5)

# Checking the regression

lm.npm.newDV <- lm(new.DV ~ HDAC + AVP + Count + Group)


# Standardization factors

Stand.fact.old <- apply(cbind(HDAC,AVP,Count),2,sd)/sd(NPM)
Stand.fact.new <- apply(cbind(HDAC,AVP,Count),2,sd)/sd(new.DV)

# Comparing estimates

# Unstandardized
un.comp <- cbind("Old"=lm.npm.all$coef,"New"=lm.npm.newDV$coef)
un.comp #Comparison of both estimates (NPM and new.DV)


# Standardized
stand.comp <- cbind("Old"=c(1,Stand.fact.old,1)*lm.npm.all$coef,"New"=c(1,Stand.fact.new,1)*lm.npm.newDV$coef)

# Verification of standardization
new.DVZ <- (new.DV-mean(new.DV))/sd(new.DV)
HDAC.Z <- (HDAC-mean(HDAC))/sd(HDAC)
AVP.Z <- (AVP-mean(AVP))/sd(AVP)
Count.Z <- (Count-mean(Count))/sd(Count)

lm.npm.newDVZ <- lm(new.DVZ ~ HDAC.Z + AVP.Z + Count.Z + Group)
lm.npm.newDVZ$coef

# Root mean square error of the (unstandadized) estimates
RMSE <- sqrt(mean((un.comp[,2]-un.comp[,1])^2))

# Changing the coefficient of AVP

new.DV2 <-	lm.npm.all$coef[1] +
		lm.npm.all$coef[2]*HDAC +
		1.5*AVP +
		lm.npm.all$coef[4]*Count +
		lm.npm.all$coef[5]*Group

lm.npm.newDV2 <- lm(new.DV2 ~ HDAC + AVP + Count + Group)
cbind("Old"=lm.npm.all$coef,"New2"=lm.npm.newDV2$coef)
RMSE2 <- sqrt(mean((lm.npm.newDV2$coef-lm.npm.all$coef)^2))

# Generating more realistic DV outcomes

new.DV3 <-	lm.npm.all$coef[1] +
		lm.npm.all$coef[2]*HDAC +
		lm.npm.all$coef[3]*AVP +
		lm.npm.all$coef[4]*Count +
		lm.npm.all$coef[5]*Group + rnorm(dim(dat)[1])

par(mar=c(4.5,4.5,1,1), mfcol=c(1,2))
boxplot(NPM, new.DV3, names=c("NPM","(Artifical 3)"), col=c("darkgreen","purple"))
par(mar=c(4.5,4.5,1,1))
plot(NPM, HDAC, pch=20, col="darkgreen", cex=1.5)
points(new.DV3, HDAC, pch=15, col="purple", cex=1.5)

# Comparing regression estimates

lm.npm.newDV3 <- lm(new.DV3 ~ HDAC + AVP + Count + Group)

un.comp <- cbind("Old"=lm.npm.all$coef,"New3"=lm.npm.newDV3$coef)
RMSE <- sqrt(mean((un.comp[,2]-un.comp[,1])^2))

# -------------------------- #
# EXAMPLE OF REPEAT FUNCTION #
# -------------------------- #

cnt <- 0
repeat{

	cnt <- cnt +1

	new.DV4 <-	lm.npm.all$coef[1] +	
		lm.npm.all$coef[2]*HDAC +
		lm.npm.all$coef[3]*AVP +
		lm.npm.all$coef[4]*Count +
		lm.npm.all$coef[5]*Group + rnorm(dim(dat)[1])

	lm.npm.newDV4 <- lm(new.DV4 ~ HDAC + AVP + Count + Group)

	if(round(lm.npm.all$coef[3],4)==round(lm.npm.newDV4$coef[3],4)){
		message(paste("Solution found on iteration: ", cnt))
		break
	}		
}

  #If the 3rd coefficient to the 4th decimal equals this other guy, stop


# Verifying solution

un.comp <- cbind("Old"=lm.npm.all$coef,"New4"=lm.npm.newDV4$coef) #Look at the 3rd coeff: it is similar to the 4th decimal


#What if I want to meet another criterion (the second coeff and let's make it to the second decimal)
if ((round(lm.npm.all$coef[3],2)==round(lm.npm.newDV4$coef[3],2)) &
  (round(lm.npm.all$coef[2],2)==round(lm.npm.newDV4$coef[2],2)))
  {
  message(paste("Solution found on iteration: ", cnt))
  break
  }		
}

# -------------------------------------- #
# EXAMPLE OF EXPORTING COMPLEX FUNCTIONS # How to export things so that they're packed out together
# -------------------------------------- #

# Export via dump

dump(list=c("HDAC","bootstrap"), "version_1.txt") 
  #It dumps out a text file. But it gives access to the text to the students, so not enough protected

# Complex export via save #It will compress it fully and nobody can have access to it

test.boot <- function(iter){ #This function just lets me specify the number of iterations

	# Data. This is how you create a static data set.
	perm.dat <- c(	4.69, 6.41, 5.47, 3.43, 4.39, 2.15,
				1.54, 2.67, 1.24, 1.77, 4.46, 1.83,
				5.15, 5.25, 1.72, 3.04, 4.92, 4.85, 
				3.13, 2.29, 4.90, 5.75, 3.61, 4.62)

	# Bootstrap function
	bootstrap <- function(var, bn, sizen){
		b.means <- mean(var)	
		for(i in 1:bn){
			var.2 <- sample(var, sizen, replace=TRUE)
			b.means <- c(b.means,mean(var.2))
			pie(c(i,bn-i),c(paste("Done",i),
				paste("Running",bn-i)),col=c("green","grey"), clockwise=TRUE)
		}
		dev.off()
		b.means
	}

	temp <- bootstrap(perm.dat, iter, length(perm.dat))

	temp <- cbind(	Lower = sort(temp)[round(length(temp)*.025,0)],
					Mean = mean(temp),
					Upper =  sort(temp)[round(length(temp)*.975,0)],
					SE=sd(temp))
	rownames(temp) <- paste("Boot.",iter+1,sep="")
	temp
}

save("test.boot", file="version_2.R") #You save one big program. This is what we have to do.You can save it as a .txt if you want. If you want to open it, it's all symbols, it's encripted, nobody can change the data.

# CLEARING EVERYTHING

ls()
rm(list=ls())
ls()

# Importing dump. NOT THE WAY TO GO, SEE HOW LABOR IT REQUIRES FROM THE USER

source("version_1.txt")
ls()
HDAC
b.out <- bootstrap(HDAC,999,length(HDAC))

rm(list=ls())
ls()

# Importing save. THIS IS THE WAY TO GO.

load("version_2.R")
ls()

test.boot(4999)
#By running this function, it gives me the results without having to do anything.
#Remember that this is a function, what it creates disappears! 
#IF you want to have permanent access to your outputs, 
#create an object.Or have a save statement, or a pdf report, of a write.table inside the function!
