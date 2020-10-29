# ------------------------------------------------------- #
#       Statistical computing and programming using R     #
#                   with Michael Chajewski                #
# ------------------------------------------------------- #
# mchajewski@collegeboard.org // http://www.chajewski.com #
# ------------------------------------------------------- #
#                  Section VI: Programming                #
# ------------------------------------------------------- #

#APPLY FUNCTIONS
mat0<-matrix(1:21,3,7)

colSums(mat0)

#Lets do this with an apply function: first you say what you want to apply it to, then row(1), col(2) or both c(1,2) and then the function

apply(mat0, 2, sum)

#Whats the benefit over colSums? It gives me more flexibility. Let-s get the colSums after subtracting 1 from each cell.

apply(mat0, 2, function(x){sum(x-1)})

#What parameter am I going to pass on to the function? I said 2, so inside the parenthesis of the function, 
#if I write one parameter (whatever, x, cheesecake...) it-s going to take the columns

#To see what happens if I apply this to the rows instead of columns, change it to a 1 and run it!
apply(mat0, 1, function(x){sum(x-1)})

#Or to every cell!
apply(mat0, c(1,2), function(x){sum(x-1)})

#I could also apply it to a specific set of rows or columns (e.g. Rows 1 and 3)
apply(mat0[-2,], 1, function(x){sum(x-1)})

#e.g.All but columns 2 and 5
apply(mat0[,-c(2,5)], 2, function(x){sum(x-1)})

#Remember! I must assign this to an object to have it stored.



# ----------------- #
# Demonstration 6.1 # 
# ----------------- #

#Ways to communicate when we write a program

#When you print, it treats it as data (it-ll come with a [1]in front of it)
print("You have seen this before")

#More official, it treats it as a message (no [1] in front)
message("This is a bit more official")

#It says "warning message" in front of it. It doesn't stop the program, it just prevents you.
warning("This one gets attention")

#If it gets to that command, STOP what you are doing
?stop
stop("Now we know something failed in the function")
on.exit("Is not a good way to communicate")


#LEt-s test the stop function:
W <- function(x,b){
  y <- x+b
  if(y<0){
    stop("No! y is negative, stupid!")
  }else{
    (sqrt(y))^3
  }
}

W(2,3) #This is ok
W(2,-3) #This gives me the error and stops!



#Randomly pick values from a normal distribution, when you find a value larger than 2, STOP and give me that value
y <- 0
y
ox.demo <- function(){
	repeat{
		y <- rnorm(1)
		if(y > 2){
			message(paste("Y =", y))
			break #This is diffrent from stop, stop is only appropriate when there is a problem.
		}
	}
	#on.exit(y <- 0)
}
ox.demo()
y

#This is to interact with other softwares
?require
require(MASS)
?shell
shell.exec("G:/Teaching/R S14/Lecture/Lect9/p22.pdf")
?system
system(paste('"C:/Program Files (x86)/Mozilla Firefox/firefox.exe"',
             '-url www.chajewski.com'), wait = FALSE)

#Debugging. If you run this guy (silly), you-ll see it doesn-t work.
silly <- function(x,y) {
	tempfil <- c()
	for(i in 1:x) {
		tempfile <- c()
		for(j in 1:y) {
			tempfile <- c(tempfile, (rnorm(1)/runif(1))*j)
			tempfil <- c(tempfil, sum(tempfile))
	}
	tempfil		
}

#This won-t work bc we-re missing a curly bracket. If I put the last bracket right after the one I have and I run this...
}

silly(3,2) # Nothing happened. It-s because the curly bracket is misplaced.

#There is a function that is going to help us to go through it one step at a time.Just applicable to funcitons, not datasets

debug(silly)

# or 
# debugonce(silly)
isdebugged(silly)
silly(2,3)
undebug(silly)
silly(2,3)
outobj <- silly(2,3)

silly <- function(x,y) {
	tempfil <- c()
	for(i in 1:x) {
		tempfile <- c()
		for(j in 1:y) {
			tempfile <- c(tempfile, (rnorm(1)/runif(1))*j)
		} # This was missing
		tempfil <- c(tempfil, sum(tempfile))
	}
	tempfil		
}

silly(2,3)
silly(5,3)

# ----------------- #
# Demonstration 6.2 # 
# ----------------- #

rm(list=ls())
ls()

#DYNAMIC INPUT! You write a function that waits for the user to give an input. To interact with the user
#WE'LL USE THIS IN THE NEXT PROJECT
aging <- function() {
	x <- as.numeric(readline("How old are you?:  "))
	print(paste("Now you're",x+10,"years old.",sep=" "))
}
aging()

#Some objects are permanently stored in R. Like... 
pi
letters
LETTERS

#This is a function that gives you options and asks the user to select one!
#It returns the option they pick.
?menu
menu(c("Choice A", "Choice B"))

#I want yo uto switch btwn different things based on what is submitted:1, 2 or 0
switch(menu(c("List letters", "List LETTERS"))+1,
	cat("Nothing done\n"), letters, LETTERS)
#The backslash n means skip to the next line

#POWER OF TOTAL CUSTOMIZATION
#I can create a dropdown menu of the things I want! Not applicable in mac or rstudio
winMenuAdd("Chajewski")
welstat <- "Hello world!!"
winMenuAddItem("Chajewski", "Welcome", "welstat")
winMenuAddItem("Chajewski", "Fame"
	,"paste('Your stage name is : ',readline('Your favorite condiment?  ')
	,readline('A good pet name?  '))")
winMenuDel("Chajewski")

dat8 <- data.frame("Gender"=c("M","M","M","F","F","F"),
	"Age"=c(20,25,28,30,21,20))
View(dat8)


#Let-s bring in that menu function
source("G:/Teaching/R S14/Lecture/Lect9/demo_menu.R")
ls()
#tRY these functions. The first one is cool
winMenuDel("Class Demos")

library(tcltk)
?TkWidgets

### Importante: when we create complex programs and we want to use those functions later or send them to someone
### And we don-t want to freak anybody out w lots of lines of code...
### Save and call it as source



