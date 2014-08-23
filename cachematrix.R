# These functions are used to cache the inverse of a matrix

# Usage:
#> x = matrix(rnorm(4),nrow=2,ncol=2)
#> m = makeCacheMatrix(x)

# when it runs for the first time it computes the inverse
#> cacheSolve(m)
#[,1]       [,2]
#[1,] 1.049268 -0.7330589
#[2,] 1.226854 -2.1513831

# when it runs for the second time it gets the answer from cahce
#> cacheSolve(m)
#getting cached data.
#[,1]       [,2]
#[1,] 1.049268 -0.7330589
#[2,] 1.226854 -2.1513831


# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
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


# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# then the cachesolve should retrieve the 
# inverse from the cache.
# cacheSolve assumes that the matrix is always invertible and the matrix 
# has not changed, if it is changed you must call makeCacheMatrix again
cacheSolve <- function(x) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}