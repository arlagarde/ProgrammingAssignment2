## Create special Matrix object which can store its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize m marker to check if inverse already calculated
  m <- NULL 
  ## Function to set the Matrix, reset m to Null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Function to retrieve the Matrix
  get <- function() x
  ## Function to set inverse of the Matrix
  setsolve <- function(solve) m <<- solve
  ## Function to retrieve inverse of the Matrix
  getsolve <- function() m
  ## Create list of function
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function to either retrieve matrix inverse from cache or
## compute it if not in cache

cacheSolve <- function(x, ...) {
  ## Retrieve inverse from cache
  m <- x$getsolve()
  ## If retrieval successful, return inverse, with message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If cache empty
  ## store matrix in data
  data <- x$get()
  ## calculate inverse from data
  m <- solve(data, ...)
  ## store inverse in cache
  x$setsolve(m)
  ## return inverse
  m
}
