## This file contains two functions, makeCacheMatrix() and cacheSolve().
## The first function in the file creates an R object that stores a matrix
## and its inverse. The second function calculates the inverse if it is null

## makeCachematrix defines set of functions to set and get matrix and 
## its inverse and returns functions as a list to parent environment


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(m)
	{
		x <<- m
		inv <<- NULL
	}	
	get <- function() x
	setinv <- function(i) inv <<- i
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverse from the cached value that is stored in 
## the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv))
	{
		message("getting data cached")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
