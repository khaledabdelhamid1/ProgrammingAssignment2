

#you will find comments describing the key functions throughout the code

#makecachematrix: This function creates a special "matrix" object that can cache its inverse.

library(MASS)  #used to calculate inverse for squared and non squared matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                #initialising inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x   #function to get matrix x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x          #function to get inverse of matix
    }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...)       #gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){      #check if inverse is NULL
    message("getting cache data")
    return(inv)        #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)   #calculates inverse value
  x$setinv(inv)
  inv    #return matrix that is inverse of x
}


