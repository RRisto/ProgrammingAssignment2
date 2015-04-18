#Matrix inversion is usually a costly computation and there may be some benefit to 
#caching the inverse of a matrix rather than compute it repeatedly  
#Following pair of functions cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse:
# 1. set value of the matrix
# 2. get value of the matrix
# 3. set value of inverse of the matrix
# 4. get value of inverse of the matrix

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

#This function calculates inverse of the matrix created with makeCacheMatrix().
#If inverse of the matrix has already been calculated 
#then the inverse from the cache is returned.
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

