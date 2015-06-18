## Following 2 functions makeCacheMatrix() , cahceSolve()
## use to compute the inverse of a given matrix.
##
## Due to the matrix inverse computation is expensive
## these functions were written to cache the result to 
## avoid recalculate. It will calculate once and return same 
## result by using cahced result.


## FUNCTION : makeCacheMatrix()

## accepting matrix vector and setup a list that could perform 4 operations  
## - set (setting input variable)
## - get (return the input variable )
## - setInvMatrix (setting inversed matrix)
## - getInvMatrix (return inversed matrix)

## NOTE : this function does not perform any compute for inversing the matrix, 
## inverse computation will done in second function cahceSolve() 

makeCacheMatrix <- function(x = matrix()) {
  
  # inversed matrix variable
  xMatrix <- NULL 
  
  # use to set the matrix passing in through makeCacheMatrix$set
  set <- function(y)
  {
      # stored value passing by $set to variable x
      x <<- y
      
      # initialize the xMatrix
      xMatrix <<- NULL
  }
  
  
  # return matrix set through above setter $set
  get <- function() x
  
  # set the (inversed) matrix passed by $setInvMatrix to xMatrix
  setInvMatrix <- function(z)
  {
    
     xMatrix <<- z 
    
  }
  
  # return the inversed matrix
  getInvMatrix <- function() xMatrix
  
  
  # make a list that allowed to perform those 4 operations
  list (set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
 
}


## FUNCTION : cacheSolve()
## Return a matrix that is the inverse of 'x'

## compute matrix of x to inversed matrix
## by checking whether it was computed and cached
## if cahced, return the cached result.
## otherwise compute, cache an return the result

cacheSolve <- function(x, ...) {
        
  # assign the current inversed matrix of x to variable xMatrix
  xMatrix <- x$getInvMatrix()
  
  # check whether the xMatrix was cached before
  # if has cached before, then return the cached result and skip the result of the code
  
  if(!is.null(xMatrix))
  {
    
    #print a message notify the user that it was computed and returning that result
    message("Message : Getting cached inversed matrix")
    return (xMatrix)
    
  }
  
  
  # if xMatrix is null, then it will execute following code
  # means inversed matrix has no calculated before
  aMatrix <- x$get()
  
  # NOTE : compute the inverse matrix by using built-in function, solve()
  xMatrix <- solve(aMatrix,...)
  
  # cache the computed matrix (inversed matrix)
  x$setInvMatrix(xMatrix)
  
  # return the inversed matrix result 
  xMatrix
}









## TEST CASE ##
# create a test matrix
sampleMatrix<- matrix(1:4,2,2)

# check the matrix
sampleMatrix

# create makeCachecMatrix object
testMatrix <-makeCacheMatrix(sampleMatrix)

# first time request compute inverse by above function cacheSolve()
CallTest01<-cacheSolve(testMatrix)

# check the result
CallTest01

# second time request compute inverse by above function cacheSolve()
# NOTE: take note of the print message
CallTest02<-cacheSolve(testMatrix)

# check the result
CallTest02
