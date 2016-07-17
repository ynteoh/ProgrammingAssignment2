## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This funtion will define all the functions necessary to work on 
##inverting the matrix in the cacheSolve Function 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Allow user to set data without calling makeCacheMatrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #store the data used for calculation
  get <- function() x
  #Perform inv calculation
  setinverse <- function(inverse) inv <<- inverse
  #Caching the result 
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
## This function will determine whether the matrix has already been 
##inverted or not. If not then the appropriate funtion is called which 
##are defined in the makeCacheMatrix function and then use solve 
##function to invert the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
