
## function makeCacheMatrix  creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

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


##  cacheSolve Return a matrix that is the inverse of 'x'
# example: 
# > c = rbind(c(1, -1/4), c(-1/4, 1))
#> class(c)
#[1] "matrix"
#> ll<-makeCacheMatrix(c)
#> cacheSolve(ll)
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> cacheSolve(ll)
#getting cached data
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> cacheSolve(ll)%*%c
#getting cached data
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1
#> 
cacheSolve <- function(x, ...) {
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
