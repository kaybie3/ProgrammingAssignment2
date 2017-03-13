# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The below
# functions cache the inverse of a matrix for repeated use.

# This function creates a special "matrix" object that can cache its inverse and
# allows you to:
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
	stored_inverse <- NULL
        
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
        
	get <- function() {
		x
	}
        
	setinverse <- function(inverse) {
		stored_inverse <<- inverse
	}
        
	getinverse <- function() {
		stored_inverse
	}
        
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}



# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix.

# If the inverse has already been calculated (and the matrix has not changed), 
# then we retrieve the inverse from the cache.

# This function assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()

        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


# usage example
m <- makeCacheMatrix()
m$set(matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE))
cacheSolve(m) # un-cached value
cacheSolve(m) # cached value

