#EXERCISE 2
matrix(1:21,3,7)

#EXERCISE 3
mat1<-round(matrix(runif(49),7,7),digits=2)

#EXERCISE 4.a

##This is what I thought I had to do: vector with diag elements
mat1.diag<-c()
for(i in 1:7){
  mat1.diag<-c(mat1.diag,mat1[i,i])
}

##This is what I had to do: substitute each diag element for its [i,i] number
for(i in 1:7){
  mat1[i,i]<-i
}

#EXERCISE 4.b
##What I thought I had to do
diag(mat1)

##What I had to do
diag(mat1)<-c(1:7)

#EXERCISE 5.a.
for(i in 1:7){
  mat1[i,i]<-0
}
  mat2<-mat1

#EXERCISE 5.b.
for(i in 1:6){
  mat2[i,i]<-round(sqrt(mat2[i,(i+1)]),digits=2)
}


#EXERCISE 6

for(i in 1:7){
  mat2[i,i]<-round(sqrt(max(mat1[i,])),digits=2)
}

#EXERCISE 7
for(i in 1:7){
  mat1[i,i]<-0
}
mat3<-mat1

checkdiag<-apply(mat3,1,function(x){sqrt(max(x))})
round(checkdiag,digits=2)==diag(mat2)

#EXERCISE 8
mat4<-round(matrix(runif(9),3,3),digits=2)
mat5<-round(matrix(runif(100),10,10),digits=2)

diag(mat4)<-0
diag(mat5)<-0

#EXERCISE 9
for(i in 1:10){
  if(i<4){
    mat4[i,i]<-round(sqrt(max(mat4[i,])), digits=2)
    mat5[i,i]<-round(sqrt(max(mat5[i,])), digits=2)
  }
  if (i>3){
    mat5[i,i]<-round(sqrt(max(mat5[i,])), digits=2)
  }
}

#EXERCISE 10
allmats<-list(mat1,mat4,mat5)

#EXERCISE 11
matprods<-c()
for(i in 1:3){
  matprods<-c(matprods, 
      (max(allmats[[i]])*det(allmats[[i]])))
}

#EXERCISE 12
ele_exa <- function(w, tor=2, num.abs=FALSE){ 
  w_pow <- w^tor 
  w_diff <- w_pow-w 
  if(num.abs==1){ 
    w_diff <- abs(w_diff) 
  } 
  w_diff 
}

#EXERCISE 14
ele_exa <- function(w, tor=2, num.abs=FALSE){ 
  w_pow <- w^tor
  w_diff <- w_pow-w
  w_diff2 <- w_diff
  if(num.abs==1){ 
    w_diff2 <- abs(w_diff) 
  } 
  list("w"=w, "tor"=tor, "num.abs"=num.abs, "w_pow"=w_pow, "w_diff"=w_diff, "w_diff2"=w_diff2)
}

test1<-ele_exa(-.50)
test2<-ele_exa(.23,num.abs=TRUE)

#EXERCISE 15
matsumm<-function(x){ 
  checksymm<-x==t(x)
  if(all(checksymm)==TRUE){print("symmetric")}
  
  list("min"=min(x), "max"=max(x), "det"=det(x))
}


#EXERCISE 16
mat<-c()

matsumm2<-function(x){
  for(i in 1:unique(dim(x))){
    mat<-rbind(mat,c(i, min(x[,i]), max(x[,i])))
  }
  colnames(mat)<-c("Column", "Min", "Max")
  print(mat)
}
