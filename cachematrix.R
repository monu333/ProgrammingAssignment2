## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makecachematrix is used  to create a special vector,which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of matrix
##4.get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function
##The following function cachesolve is used to return the inverse of matrix.
##And if the inverse is already calculated, then the cachesolve function retrieves the inverse from cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  #Solve function is used to compute the inverse of a square matrix
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


#Test Run
##> x = cbind(c(4,5),c(6,7))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    4    6
##[2,]    5    7
##> cacheSolve(m)
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
##> cacheSolve(m)
##getting cached data.
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
##> 
  
  
  