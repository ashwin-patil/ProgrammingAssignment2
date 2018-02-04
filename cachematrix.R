## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                    invse <- NULL
                    set <- function(y) {
                      x<<-y
                      invse <<- NULL
                    }
                    get <- function() x
                    setinverse<- function(inverse) invse <<- inverse 
                    getinverse <- function() invse
                    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
                    }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                    invse <-x$getinverse()
                    if(!is.null(invse)){
                      message("Getting cached data...")
                      return(invse)
                    }
                    data <- x$get()
                    invse <- solve(data,...)
                    x$setinverse(invse)
                    invse
  }