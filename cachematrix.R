## The code consists of 2 functions that can retrieve the inverse 
## of a matrix. The first function creates an object that can store 
## a matrix and its inverse. The second function uses this object 
## as an argument and computes the inverse of the matrix. If the 
## inverse is already calculated, this function retrieves the stored value

## This function takes a matrix and returns an object that contains
## 4 functions (set,get,setinv,getinv). It also contains 2 objects (x,m)
## that store information of the matrix x and its inverse m.
## The set function is able to store a matrix x, while the get function
## can retrieve it. Similarly, the setinv and getinv can store the
## matrix inverse m and retrieve it.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function takes an object x (will typically be the output of
## the previous function) and tries to retrieve the inverse m of the 
## matrix. If this has already been calculated and stored, it will display
## the message "getting cached data" and return the stored value.
## If the inverse of the matrix hasn't been calculated, it will do so, 
## store it, and finally return its value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

