## Below are two functions that are used to create
## a special object that stores a numeric matrix and
## cache's its inverse

## makeCacheMatrix(): This function creates a special 
## "matrix" object tha can cache its inverse and has
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  set<-function(y){
    x<<-y
    invm<<-y
  }
  get<-function() x
  setinv<-function(inv) invm<<-inv
  getinv<- function() invm
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve(): This function calculates the inverse of 
## the special "matrix" created with makeCacheMatrix() 
## function. It skips computation and gets it from cache
## if the inverse has already been calculated. 
## It sets the value of the inverse in the cache using setinv() 
## function
## ASSUMPTION: The matrix is always invertible 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm<-x$getinv()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  
  invm <- solve(x$get())
  x$setinv(invm)
  invm
}

############### Sample Run:
## > x = rbind(c(4, 7), c(2, 6))
## > a<-makeCacheMatrix(x)
## > a$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6

## > a$getinv()
## NULL

###### First run: Not retrieving from cache, 
###### calculation  done
## > cacheSolve(a)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

###### Second run: retrieving from cache
###### calculation not done
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## >

###### Value of inverse set in cache
## > a$getinv()
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## >  
