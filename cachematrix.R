## JHU-R Programming:Week 3 - Programming Assignment 2
## Created two functions to cache square matrices and return the
##     inverse of each matrix.
##     Note:  The matrix supplied is always invertable.


## makeCacheMatrix()
## This function places a passed square matrix into cached memory 
##     for later retrieval and processing.  It consists of an
##     array of 4 functions:
##     set(), get(), setmatrix(), and getmatrix().

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(matrix) m <<- matrix
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)

}


## cacheSolve()
## This function takes in the output of the makeCacheMatrix() 
##	function.  It checks if the input has already been
##	processed.  If it has, the function returns the 
##	cached matrix.  If not, it calculates the matrix
##	inverse using the solve() function, then stores
##	the result in cache.

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	message("setting cached matrix")
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
