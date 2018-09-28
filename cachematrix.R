## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# new function with matrix arg(regmatrix)
makeCacheMatrix <- function(regmatrix = matrix()) {
	# create 'placeholder' for inverse matrix
	inversematrix <- NULL
	# create function to assign value to regmatrix. Reverse matrix still null for now.
	set <- function(setvalue) {
		regmatrix <<- setvalue
		inversematrix <<- NULL
	}
	# create function to return regmatrix
	get <- function() regmatrix
	# create function to assign inverse matrix vals (opps)
	setinvals <- function(opps) inversematrix <<- opps
	# create function to return the above inverse vals
	returninvals <- function() inversematrix
	# multiple functions within a function can be referred to in a parentfunction$childfunction format if you specify those funcions in a list, as below
	list(set = set, get = get, setinvals = setinvals, returninvals = returninvals)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(regmatrix, ...) {
	# use assign returninvals to inversematrix
	inversematrix <- regmatrix$returninvals()
	# check if inversematirx is already populated (i.e. it's not true that it's null). if it is populated, return message and data
	if (!is.null(inversematrix)) {
		message("getting cached data")
		return(inversematrix)
	}
	# if there aren't already values assigned
	# get regmatrix values and assign to a variable
data <- regmatrix$get()
	# using solve, assign the inverse values of your new variable to inversematrix
	inversematrix <- solve(data, ...)
	# use the setinvals functiont to assign your inverse matrix in line with the above
	regmatrix$setinvals(inversematrix)
	# return inversematrix
	inversematrix
}
