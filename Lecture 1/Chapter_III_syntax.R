# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                Section III: Understanding R             #
# ------------------------------------------------------- #

# ----------------- #
# Demonstration 3.1 # 
# ----------------- #


# Write hashtag at the beginning of the line to make an anotation. Vital to remember things!
# whenever you see parenthesis it means you are dealing with a function.
# whenever you see brackets, you're dealing with an object

# Sometimes they will be empty, sometimes not.

# So the word "license" is a function.
# In order to run a function, you highlight it and press ctrl+R or left button in the mouse.
# if you want to run everything in that line, no need to highlight.



help.start()

license()
#the license info comes up

contributors()
# info about everybody involved

help()
help(license)
?"license"

help.search(licen)
#It gives us an error. Why? Bc we're looking for a term (not what it stands for), and it needs to be in quotation marks.

help.search("licen")
#If Im looking for something that has "licen" in it. And help.search () is a function.
#The thing is that for help, R goes online by default. But if not connected to the internet, it goes to the stored info.

??"licen"
Sys.info()
Sys.time()
date()
timestamp()
memory.limit()
memory.size()
getwd()
setwd("C:/")
history()
environment()

# ----------------- #
# Demonstration 3.2 # 
# ----------------- #

(((2+2)*.2)-.3)/.5
sqrt(169)
125^(1/3)
# This is a comment
message("This is a message")
log(10) # Natural
round(log(10), 1)
3 == 3
3 != 3
3 >= 2
3 < 2
is.na(3)
is.nan(sqrt(-2))
2 %% B
2 %% .5
warnings()

# ----------------- #
# Demonstration 3.3 # 
# ----------------- #

library()
demo()
demo(plotmath)
example()
help.search("Regression")
?lm
example(lm)
data()
data(Orange); Orange
Orange[]
Orange[,2]
Orange$age





