## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function returns TRUE if the argument being passed is an
#invertible matrix, otherwise returns FALSE and an error message
#concerning the reason why the argument is not an invertible matrix.

isInvertibleMatrix <- function(X){
  
  if(!is.matrix(X)) {
    message('arg is not a matrix')
    return(FALSE)
  } else if (nrow(X)!=ncol(X)) {
    message('matrix is not square')
    return(FALSE)
  } else if (is.na(det(X))) {
    message('matrix is empty')
    return(FALSE)
  } else if (det(X)==0) {
    message('matrix is not invertible')
    return(FALSE)
  } else return(TRUE) 
  
}

# This function returns a list of 4 functions: set, get, setInvMatrix, gewtInvMatrix. 
# set sets an invertible matrix Y in the cache, get returns this matrix, 
# setInvMatrix sets the matrix inverse of Y in the cache, 
# gewtInvMatrix returns this matrix.

makeCacheMatrix <- function(X =matrix()){
  inv <- NULL
  set <- function(Y=matrix()){
        X <<- Y
      inv <<-NULL
  }
  get <- function() {
     X
  } 
  setInvMatrix <- function(){
    if(isInvertibleMatrix(X)) { 
      inv <<- solve(X)
      print("I cache inverse of ")
      print(X)
    }
  } 
  getInvMatrix <- function() {
    if(!is.null(inv))  inv
   
  }  
  list(set=set, get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}

## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cacheSolve retrieve 
# the inverse from the cache, otherwise it calculates the inverse of the matrix, 
# puts it in the cache and displays it


cacheSolve <- function(X){
  inv <- X$getInvMatrix()
  if(!is.null(inv)){
    message("get cached matrix")
    return(inv)
  }
  X$setInvMatrix()
  print("Inverse of matrix is:")
  X$getInvMatrix()
  
}


# > lis <-makeCacheMatrix(matrix(c(1,2,1,2),nrow = 2))
# > lis$get()
# [,1] [,2]
# [1,]    1    1
# [2,]    2    2
# > lis$getInvMatrix()
# > lis$setInvMatrix()
# matrix is not invertible
# > lis$set(matrix(c(1,2,1,2,3,4),nrow = 3,ncol = 2))
# > lis$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    2    3
# [3,]    1    4
# > lis$getInvMatrix()
# > lis$setInvMatrix()
# matrix is not square
# > lis$set(matrix(c(1,2,3,4),nrow = 2))
# > lis$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > lis$getInvMatrix()
# > lis$setInvMatrix()
# [1] "I cache inverse of "
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(lis)
# get cached matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > lis$set(matrix(c(1,2,3,4),nrow = 2,ncol = 2))
# > lis$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > lis$getInvMatrix()
# > cacheSolve(lis)
# [1] "I cache inverse of "
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# [1] "Inverse of matrix is:"
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 


