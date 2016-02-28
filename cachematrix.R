## inverse of a matrix can only be done on a square matrix.
## That is why I created a function called makeSquareMatrix.
## If the amount of number is 4 (w) there should be 2 rows (r).
## If the amount of numbers is 9 (w) there should be 3 rows(r) etc.

makeSquareMatrix <- function (w,r) {
  b <- matrix(w, r) 
}

## the makeCacheMatrix will cashe the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The casheSolve function computes the inverse of the matrix created by the makeSquareMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## now use the created functions
n <- makeSquareMatrix(c(5,2,7,4,2,2,3,2,1), 3)
a <- makeCacheMatrix(n)
cacheSolve(a)
## repeat cacheSolve without changing the inverse to get the message "getting cached data"
cacheSolve(a)
