## R programming week 3 :Caching the Inverse of a Matrix


# function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i = x$getinverse()
    #use the inverse from the cache if available
    if(!is.null(i)) {
        message("getting the cached inverse matrix")
        return(i)
    }
    
    #otherwise it computes the inverse, then cache it 
    matrix <- x$get()
    i = solve(matrix)
    x$setinverse(i)
    i
        
}

#test
mat <- matrix(rnorm(100),10,10)
mat1 <- makeCacheMatrix(mat)
mat2 <- cacheSolve(mat1)
mat2
