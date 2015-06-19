## These two functions below work together so that the inverse of a
## given matrix only needs to be computed once. After it is computed
## once, it is retrieved from "cache".

## This function creates a list of four functions that act on a given matrix, x
## The functions that make up the list are:
## set - sets the value of the matrix
## get - retrieves the given matrix
## setinverse - calculates the inverse of the given matrix into "m"
## getinverse - retrieves the inverse of the given matrix, m

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {

		x <<- y
		m <<- NULL

	}

	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## The cacheSolve function accepts the "function" list from makeCacheMatrix
## This function uses the makeCacheMatrix functions to determine if the inverse
## of the matrix has already been computed (is not null).
## If yes, it returns the cached inverse matrix from the makeCacheMatrix function
## Otherwise, it calcluates the inverse and "sets" this value using setinverse()
## and returns the inverse matrix

cacheSolve <- function(x, ...) {

        m <- x$getinverse()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
