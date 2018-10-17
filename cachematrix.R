## Put comments here that give an overall description of what your
## functions do

#1- set the value of the vector
#2- get the value of the vector
#3- set the value of the mean
#4- get the value of the mean

#This function generates an object which contains a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #definining the setters and getters for the function
  set <- function(y){
      #Assign the matrix and make null the inverse
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  #Without the <<- operator the inverse will not be saved from cacheSolved
  setinverse <- function(inver) inv <<- inver 
  getinverse <- function() inv 
  #list of methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This method calculates the inverse of a given matrix, in case the inverse has been already calculated
#it returs the saved value (simulating a cache)
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#For testing I used this invertible matrix I found
#myMatrix <- matrix(c(4,7,2,6), 2, 2)
#myCacheMatrix <- makeCacheMatrix(myMatrix)
#cacheSolve(myCacheMatrix)
#cacheSolve(myCacheMatrix)
#After repeating the call we get the message: "getting cached data"
