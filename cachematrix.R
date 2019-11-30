## I am going to code a function that caches the result of the inverse of a matrix
## Remember to always feed in a square matrix.


## The function "makeCacheMatrix" below contains the following:
## 1. Sets the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the set matrix
## 4. Get the value of that inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function below does the following:
## 1. Checks to see if we have already calculated the inverse of this particular marix
## 2. If it has, returns the cached value to us
## 3. If not, it goes ahead with a new computation

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Ah! You seemed to have done this before. Let's give it back to you!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
}


# Let's test it

a <- makeCacheMatrix(matrix(c(4,4,7,6), nrow = 2, ncol = 2))
a
cacheSolve(a) # Voila! Everything checks out! We got the right answer!
cacheSolve(a) # cache successfully retrieved

