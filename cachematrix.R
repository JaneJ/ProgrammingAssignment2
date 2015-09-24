## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
## It first checks to see if the inverse
## has already been calculated. If so, it gets the result from the cache
## and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
# Test run
# > x = rbind(c(2,4), c(4, 2))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    2    4
# [2,]    4    2
# > cacheSolve(m)
# [,1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667
# > cacheSolve(m)
# getting cached data
# [,1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667
# > 
