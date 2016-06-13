## Program that allows to cache the inverse of a matrix
## First by caching the inversed matrix, second by checking if the 
## inverse matrix is there

## Create a special matrix, that keeps its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x<<-y
              inv<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<-inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Compute the inverse, if it is not already cached, otherwise
## Tried to compare if 'mat' and 'x' are still identical (not changed)
## But could not get an easy way to do so

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
              message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
}
