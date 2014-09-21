## The two functions makeCacheMatrix and cacheSolve
## create a special matrix object whose inverse can be cached,
## then checks if it is unchanged from a previous calculation
## and retrieves the inverse from the cache

## makeCacheMatrix should:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve should compute the inverse of the special "matrix"
## that is returned by makeCacheMatrix.  Then, it checks to see if
## the inverse has already been calculated (and is unchanged), if so
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
