## makeCacheMatrix and cacheSolve together calculate and cache the inverse of a matrix


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- matrix(NA) ##setup of empty matrix
    set <- function(y){
        x <<- y
        inverse <<- matrix(NA)
    }
    get <- function() x
    setinverse <- function(calcinverse) inverse <<- calcinverse
    getinverse <- function() inverse
    list(set = set,get=get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of a special matrix returned by makeCacheMatrix.
## If the inverse has been already calculated, then the function
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.na(inverse[1,1])){
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}

y <- makeCacheMatrix()
cacheSolve(y)
y <- matrix(rnorm(9),3,3)
x <- makeCacheMatrix(y)
cacheSolve(x)
z <- solve(y)
z
