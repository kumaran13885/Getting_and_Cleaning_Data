## makeCacheMatrix function prepares a special matrix which can able to cache
## the matrix inverse result. Since the computation of inverse is costly storing
## the result ensures faster access and would improve the performance. In order
## to compute and cache the matrix inverse 'cacheSolve' is used.

#  1. Prepare the special cached matrix
#  2. Initialize the matrix value with the input passed
#  3. Store the matrix inverse result
#  4. Retrieve the cached inverse for the input matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
		x <<- y
		m <<- NULL
	}
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## 1. Search cache for the inverse of the input matrix
## 2. Return inverse of the given matrix if available
## 3. If inverse not available retrive the matrix data from makeCacheMatrix
## 4. Using 'solve' function compute inverse of the matrix
## 5. Store the result in cache and Return the result

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	m <- x$getsolve()
      if(!is.null(m)) {
      	message("getting cached data")
            return(m)
	}
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}