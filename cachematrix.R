## Computing the inverse square matrix using function solve()
## functions do
## Functions: (1) makeCacheMatrix   -- cache functions into RAM
##            (2) cacheSolve        -- inverse Matrix using function solve(matrix)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
				x <<- y
				inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list( set = set,
		  get = get,
		  setInverse = setInverse,
		  getInverse = getInverse)
}

## Return Inverse Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if (!is.null(inv)) {
			message("Gettig cached data")
			return(inv)
		}
		mat <- x$get()
		inv <- solve(mat, ...)
		x$setInverse(inv)
		inv
}
