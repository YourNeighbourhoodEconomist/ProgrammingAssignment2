## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##constructing two functions names makeCachematrix and then a cacheSolve function
##The first part of makeCachefunction is to get the matrix inverse as null and 
## followed by function to get the matrix x and function to get the matrix inverse

makeCachematrix <- function(x= matrix()){
  inv <- NULL
  
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function(){x}
  
  setInverse <- function(inverse){inv <<- inverse}
  
  getInverse <- function () {inv}
  list (set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##The below function will be used to get the cached data  the check if the inverse 
##is null , followed by solving and returning the inverse of the matrix

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  
  if (!is.null(inv)){
    message("getting cached data")
    
    return(inv)
  }
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  inv
}


