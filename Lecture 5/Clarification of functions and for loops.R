make.sudo<-function(){matrix(1:9,3,3)}
#make.sudo is going to be the final function. 
#one function can do hundreds of operations, not only one.
#When you call this function, you want it to spit out this matrix
make.sodu
make.sudo
#When you run the function without the parenthesis, it tells you what it does.
#If you put the (), it does what it's supposed to do'

#The cousin to functions are the loops.

for(k in c(99,0,1)){
  k+1
}
#In the for loop you have to tell him what are you doing that for, it's not the case in the functions.
#Functions and for loops open their own environment. 
#What is inside of those curly brackets is another environment, and 
#R doesn-t see it unless you tell R a way to communicate that to the common environment.
#One way is the "print"

for(k in c(99,0,1)){
 print(k+1)
}

#Another way is to create an object. But not this way:

test.out<-for(k in c(99,0,1)){
  print(k+1)
}
#It created an object test.out that is NULL! empty! BC test.out is in the Global Env.
#And the rest (k) is in a crazy environment.

#We have to PUT the results of the for loop inside an object:
object<-c()
for(k in c(99,0,1)){
object<-c(object,k+1)
print(object)
}

#Im going to create a counter to know how many times ive gone through the loop.
cnt<-0
rep(-99,3)

for(k in c(99,0,1)){
  print(cnt) #How cnt looks before i changed it
  cnt<-cnt+1
  print(cnt) #How cnt looks after i changed it
  test.out[cnt]<-k+1
  print(test.out)
}

#You can include for loops inside functions. The benefit of this, is that you 
#don't need to specify to what values it applies. It-s flexible.

make.sudo<-function(x){
  for (i in 1:x)
    print(i+1)
}

make.sudo(5)

#I can even make it a function with two parameters

make.sudo<-function(x,u){
  for(i in 1:x){
    print (i+u)}
  }

make.sudo(5,10)
}