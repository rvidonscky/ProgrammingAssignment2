## This program has a fuction that can cache the inverse of a matrix and 
## test if this inverse has alrady been calculated or not

## Description - makeCacheMatrix:
## 1. Defines an environment to a variable using myInverse <- makeCacheMatrix()
## 1.1 Set a matrix A using myInverse$set(A)
## 1.2 Get back matrix A using myInverse$get()
## 1.3 Set the inverse of matrix A using myInverse$setinverse()
## 1.4 Get the inverse of matrix A using myInverse$getinverse()

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() 
    x
  
  setinverse <- function() 
    i <<- solve(x)
  
  getinverse <- function() 
    i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Description - cacheSolve:
## 1. Pass the previously defined variable myInverse to cacheSolve 
## using cacheSolve(myInverse)
## 1.1 If the inverse has been previously calculated, x$getinverse gets 
## something different than null and the cached matrix is presented
## 1.2 If the inverse has not been previously calculated, x$getinverse gets 
## null and the inverse of the cached matrix is calculated and presented  

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse()
  i
}
