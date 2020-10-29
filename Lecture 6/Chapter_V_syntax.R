# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#          Section V: Statistical Applications           #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 5.1 # 
# ----------------- #

load("test_data.Rdata")
ls()

names(sample.data)
attach(sample.data)

install.packages("Hmisc")
library(Hmisc)
?HmiscOverview
describe(sample.data)

summary(Age)
table(Gender)
by(Age, Gender, summary)
table(Type, Admin)

by(Age, paste(Type, Admin), mean)

raw.score <- rowSums(sample.data[,6:115])
COV.ar <- cov(cbind(Age,raw.score,Gender))
COV.ar
var(Age)

Zscore <- (Age-mean(Age))/sd(Age)
plot(density(Zscore))

length(Zscore[Zscore >= 1.96])
(length(Zscore[Zscore >= 1.96]) / length(Zscore))*100
(length(Zscore[Zscore <= -1.96]) / length(Zscore))*100

?t.test
ttest.1 <- t.test(Age, mu=35)
ttest.1
summary(ttest.1)
ttest.1[]

?combn
combn(1:3, 2)
?expand.grid
expand.grid(1:3, 0:2)

det(COV.ar)
solve(COV.ar)
eigen(COV.ar)
svd(COV.ar)

# ----------------- # Independent samples t test
# Demonstration 5.2 # 
# ----------------- #

ttest.2 <- t.test(Age[which(Gender=="M")], Age[which(Gender=="F")])
ttest.2
ttest.2[]
t.test(Age~Gender)
#I can look at the mean of both group if we're not sure which is the mean of which group
by(Age, Gender, mean)

#Let's save some of this. Let's use dump bc it keeps track of what the object is called ("ttest.2")
dump("ttest.2", file="ttest_for_gender.R")

#But... this gives us a file that is criptic to someone unfamiliar with R.
#capture.output captures what you see in the console.It creates a text doc with some format
?capture.output
capture.output(ttest.2, file="ttest_for_gender.txt")

#Stick these things at the end of the file I just created
capture.output(timestamp(), file="ttest_for_gender.txt", append=TRUE)

#I want to append the analysis I did before
capture.output(ttest.1, file="ttest_for_gender.txt", append=TRUE)

#And I want to append the covariance matrices
capture.output(eigen(COV.ar)$vectors, file="ttest_for_gender.txt", append=TRUE)


#All these capture.output commands work with dump and save.


?cor
cor(cbind(Age, raw.score, Gender))
rcorr(cbind(Age, raw.score, Gender),type="pearson")

temp.1 <- cbind(Age, raw.score, Type)
by(temp.1, Gender, function(x) {rcorr(as.matrix(x),type="spearman")})

by(Age, Type, mean)
ANOVA.1 <- aov(Age~Type)
ANOVA.1
summary(ANOVA.1)
anova(lm(Age~Type))

REG.1 <- lm(raw.score~Gender+as.factor(Type)+Admin+Age)
REG.1
summary(REG.1)
REG.1[]

capture.output(REG.1, file="regression_output.txt")
capture.output(summary(REG.1), file="regression_output_summary.txt")
capture.output(REG.1[], file="regression_output_full.txt")

# ----------------- #
# Demonstration 5.3 # 
# ----------------- #

CHI2.1 <- chisq.test(table(Admin, Type))
CHI2.1
names(CHI2.1)
CHI2.1$observed
CHI2.1$expected
CHI2.1$stdres

CHI2.2 <- chisq.test(table(Admin, Type, Gender))
CHI2.2
CHI2.2$stdres

GLM.1 <- glm(ifelse(Gender=="M", 1, 0)~raw.score+Age, family=binomial)
GLM.1
summary(GLM.1)






