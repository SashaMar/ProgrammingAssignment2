## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a list containing a function to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
            if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
            i
}
