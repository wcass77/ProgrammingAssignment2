## Put comments here that give an overall description of what your
## functions do

#These functions create a set of functions with associated persistent variables to represent
#a matrix and it's inverse. The inverse is stored so that it does not need to be recalculated unless
#the matrix is changed

## Write a short comment describing this function

#This function defines the functions that set and retrive the matrix and returns them as a list. The matrix and inverse
#are stored in lexically scoped variables that persist, but are specific to each instance of 
#the functions.

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	
	set <- function(y) {
		mat <<- y
		i   <<- NULL
	}
	get <- function() mat
	setInv <- function(inv) i <<- inv
	getInv <- function() i
	list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
#This function takes a list of functions defined by makeCacheMatrix()
#and returns the inverse of the associated matrix. It also caches the matrix.
#If this is called again and the cached matrix is available, that is returned
#without recalculating the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInv() 
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInv(inv)
	inv
}
