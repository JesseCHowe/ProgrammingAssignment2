## Put comments here that give an overall description of what your
## functions do

#set - included so that once an object is created its value can be changed without initializing makeCacheMatrix again

#get retrieves x from the parent enviroment

#setinverse after solve() is run in the cacheSolve function it will set inv to the inverse of the data

#getinverse this will check if the inverse has been calculated, allowing cacheSolve to save time if possible.

## Write a short comment describing this function
# makeCacheMatrix buils a set of functions and returns those functions within a list to the parent environment.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)   
}


## Write a short comment describing this function
#cacheSolve populates the mean of an object built within makecacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#example
B = matrix( c(1:4), nrow=2, ncol=2) 
aMatrix <- makeCacheMatrix(B)
aMatrix$get()
cacheSolve(aMatrix)