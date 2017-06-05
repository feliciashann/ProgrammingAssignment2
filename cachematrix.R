## ¡§Launch Your Career in Data Science¡¨ by Johns Hopkins U.
## programming-assignment-2-lexical-scoping

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    Xinverse = solve(x)
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseOfX) m <<- inverseOfX
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Xinverse <- x$getInverse()
    if(!is.null(Xinverse)) {
        return(Xinverse)
    }
    
    data <- x$get()
    Xinverse <- solve(data)
    x$setInverse(Xinverse)
    return(Xinverse)
}

orimat <- matrix(rnorm(9), 3, 3)
x <- makeCacheMatrix()
set.seed(1)
x$set(orimat)

cacheSolve(x) #getting cached data
