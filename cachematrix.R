## The purpose of this exercise is to create a function that will store/cache a matrix inverse so I don't have to go through the process multiple times
## because calculating the inverse of a matrix consumes time and computing energy

## this function is going to make a matrix that lets us cache the inverse easier

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## I am  using <<- because I don't necessarily have those values in our environment yet, and so that R knows it's not in m current environment


## this function will compute the inverse of the matrix I specified above, or collect it from the cache I created and notify you about it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("retrieving cached inverse")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## tada!!! I tested a matrix here:
M<- matrix(c(2,4,6,8),2,2)
MC<- makeCacheMatrix(M)
cacheSolve(MC)
## if I call cacheSolve(MC) again, it will give the 'retrieving cached inverse' message because I just calculated it on line 40
