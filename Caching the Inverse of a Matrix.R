makevector <- function(x = numeric()){
  m<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmean <- function(mean) m <<- mean
  getmean <- function()m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.

## The following functions are used to create a special object that stores a matrix and caches its inverse. 
#The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

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


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}

#使用一个矩阵调用函数，计算逆，从缓存列表中检索逆，将调用矩阵改为逆，计算逆并返回原始函数。
M <- matrix(1:4,2,2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
cacheSolve(M1)