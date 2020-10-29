#CREATE 9 SOLUTIONS USING A FUNCTION THAT TAKES 1 PARAMETER (1:9)

##Defining the function that is going to create the 3x3 "boxes"
func1 <- function(x){matrix((x:(x+8)),3,3, byrow=TRUE)}

##Empty substantiation of matrices I'm going to use in the loop
band<-c()
mat<-c()
sudoku<-c()

## Function "makesudoku". For the value "y", apply the following loop (using the vector (y,y+3,y+6) as an index).
## Loop: for each value in the vector, use func1 to create a 3x3 matrix
## Concatenate these three matrices to create a "band"
makesudoku<-function(y){
  for(i in c(y,y+3,y+6)){
    band<-cbind(band,func1(i))
  
    ## Horizontally concatenate this band with that band+1 and +2
    mat<-rbind(band, band+1, band+2)
  
    ## Replace every value larger than 18 with that value minus 18; 
    ## replace every value  between 9 and 17 with that value minus 9
    sudoku<-apply(mat[,],c(1,2),function(x){ifelse(x>18,x-18,ifelse(x>9,x-9,x))})
  }
  ## Print the sudoku solution for that parameter
  print(sudoku)
  
  ## Create a vector concatenating marginal sums of rows and columns
  marginals<-c(rowSums(sudoku), colSums(sudoku))
  
  ## Test: is the object "sudoku" a sudoku?
  test<-if (identical(marginals,rep(45,18))){print("This is a sudoku :)")}else{print("This is NOT a sudoku :(")}
}

## How to ask R to give me one of these 9 solutions randomly? Run this function.
makesudoku(sample(1:9,1))


