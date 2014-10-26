## Put comments here that give an overall description of what your
## functions do

## Creates a "matrix" container object that has the following elements:
# - An invertible matrix
# - A cache for storing the inverse of the matrix.
# - A function "set" sets the value of the matrix.
# - A function "get" gets the value of the matrix.
# - A function "setInverse" sets the value of the inverse of the matrix.
# - A function "getInverse" gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL

	set <- function(mat) {
		# Assigns the matrix
		x <<- mat
		cache <<- NULL # Reset the cache
	}

	get <- function() x ## return the matrix

	# set the inverse cache
	setInverse <- function(inverse) cache <<- inverse

	# get from cache. NULL if NA
	getInverse <- function() cache

	# Return the list with the functions
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" accepts the matrix container generated with "makeCacheMatrix" and returns
# the inverse of the matrix. If the inverse has allready been calculated the function
# returns the cached inverse.

cacheSolve <- function(x, ...) {
	# Gets the cached inverse.
	inv <- x$getInverse()
	if(is.null(inv)) {
		print("no cache")
		# If there is no cached value calculates the inverse and assigns it to the object.
		m <- x$get()
		inv <- solve(m, ...)
		x$setInverse(inv)
	}else{
		print("using cached data")
	}
	# Returns the inverse
	inv
}


## Testing:
# ## New matrix
# mat <- matrix(1:4, nrow = 2, ncol=2)
# ## New cachedMatrix
# cacheMat <- makeCacheMatrix()
# ## Sets the matrix
# cacheMat$set(mat)
# ## print matrix values
# cacheMat$get()
# ## 1st pass
# cacheSolve(cacheMat)
# ## 2nd pass
# cacheSolve(cacheMat)
