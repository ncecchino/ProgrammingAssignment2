## Below are two functions that are used to create an object that stores a matrix and caches its inverse.  If the inverse is already calculated, the cache solve should retrieve the inverse from the cache.  

## makeCacheMatrix - creates a special matrix object and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL
			set <- function(y) {
				x <<- y
				s <<- NULL
			}
			get <- function() x
			setsolve <- function(solve) s <<- solve
			getsolve <- function() s
			list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## cacheSolve - computes the inverse of the matrix returned by makeCacheMatrix.  if the inverse has already been calculated, cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		s <- x$getsolve()
			if(!is.null(s))  {
			message("getting cached data")
			return(s)
		}
		data <- x$get()
		s <- solve(data, ...)
		x$setsolve(s)
		s
}
