## the two functions below are used to create a special
## object that stores a numeric matrix and caches its inverse.

## 
## this first function creates a special "vector"
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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


##
## the second function calculate the inverse of the special
## 'vector' created above. It first checks to see if the 
## inverse has already been created. If it has been created
## then it returns the inverse from the cache and skips the
## calculation. If the inverse has not been calculated then
## it is calculated and cached.

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
