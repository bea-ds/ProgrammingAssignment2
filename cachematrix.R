## makeCacheMatrix is a function that creates a special "matrix object
## that can cache its inverse

## x is a matrix object 

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL          # to cache inverse matrix
        set <- function(y){     # to set a new matrix
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x     # return matrix
        setinverse <- function(inverse) invmatrix <<- inverse # set inverse matrix
        getinverse <- function() invmatrix   # get inverse matrix
        list (set = set, get = get,       # matrix operations
              setinverse = setinverse,
              getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()   # get inverse matrix
        matrix <- x$get()             # get matrix
        if (!is.null(invmatrix)){     # inverse matrix has already been calculated
                message ("getting cached data")
                return(invmatrix)     # return cached inverse matrix
        }
        
        invmatrix <- solve(matrix)         # invmatrix has not been calculated, calculate it
        x$setinverse (invmatrix)      # set inverse matrix
        invmatrix                     # return inverse matrix
}
