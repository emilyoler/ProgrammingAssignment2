
## This function sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse, then gets the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	## sets value of matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## gets value of matrix
	get = function()x
	
	## sets inverse of matrix
	setinv = function(inverse) i <<- inverse
	
	## gets inverse of matrix
	getinv <- function() i 
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## This function calculates the inverse of the makeCacheMatrix() matrix
## above, where x is the output of the makeCacheMatrix(). If the inverse has 
## already been calculated and matrix has not changed, then cacheSolve 
## retreives the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
       
	## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	
	## if the inverse has been calculated, get from cache
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	
	## otherwise, calculate the inverse
	data <- x$get()
	i <- solve(data, ...)
	
	## sets the value of inverse
	x$setinv(i)
	return(i)
}
