#CREATE A STATIC SOLUTION USING A FUNCTION INSIDE A LOOP

##Defining the function that is going to create the 3x3 "boxes"
func1 <- function(x){matrix((x:(x+8)),3,3, byrow=TRUE)}

##Vector that I'm going to use as an index
vector<-c(1,4,7)

##Empty substantiation of matrices I'm going to use in the loop
band<-c()
mat<-c()
sudoku<-c()

## Loop: for each value in the vector, use func1 to create a 3x3 matrixç
## Concatenate them to create a "band"
for(i in vector){
  band<-cbind(band,func1(i))
  
  ## Horizontally concatenate this band with that band+1 and +2
  mat<-rbind(band, band+1, band+2)
  
  ## Replace every value larger than 9 with that value minus 9
  sudoku<-apply(mat[,],c(1,2),function(x){ifelse(x>9,x-9,x)})
 
}

## Test: is it a sudoku?
if ((unique(colSums(sudoku))==45) & (unique(rowSums(sudoku))==45)){print("This is a sudoku")}

