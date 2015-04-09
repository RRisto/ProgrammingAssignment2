#Matrix inversion is usually a costly computation and there may be some benefit to 
#caching the inverse of a matrix rather than compute it repeatedly  
#Following pair of functions cache the inverse of a matrix.

#This function creates a  secial "matrix" object that can cache its inverse:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matr <<- inverse
    getinverse <- function() matr
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

#Function to calculate inverse of a matrix created with makeCacheMatrix.
#If  inverse has already been calculated then the inverse from the cache is returned.
cacheSolve <- function(x, ...) {
    matr <- x$getinverse()
    if(!is.null(matr)) {
        message("getting cached data ...")
        return(matr)
    }
    data <- x$get()
    matr <- solve(data)
    x$setinverse(matr)
    return(matr)
}

