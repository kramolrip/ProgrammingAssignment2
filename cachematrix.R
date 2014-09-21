## cachematrix.R
## Contains two functions makeCacheMatrix and cacheSolve
## create a special matrix object whose inverse can be cached,
## then checks if it is unchanged from a previous calculation
## and retrieves the inverse from the cache
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)

## makeCacheMatrix should:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix"
## made by makeCacheMatrix.  Then, it checks to see if
## the inverse has already been calculated (and is unchanged),
## if it has/is, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse <-x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}
