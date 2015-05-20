## These functions accept an invertible matrix as input, then calculate the 
## inverse of the matrix and store it in cache. If function "cachesolve" is 
## called, inversr in the cache will be retured, if the cache is empty, then
## inverse of the matrix will be calculated. 

## Write a short comment describing this function
## This function calculate the inverse of a matrix and store it in cache

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # set a null valve to the vector inverse
    set <- function(m){
          x <<- m
          inverse <<- NULL
    } # set the value of the matrix
    get <- function()x # get the value of the matrix
    setinverse <- function(slove) inverse <<- slove # slove the matrix (get the inverse), 
                                                    # assign this value to the vector named "inverse" 
    getinverse <- function() inverse # get the value of the vector named "inverse"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # list the functions inside the makeCacheMatrix function

}

## This function check whether the inverse of a matrix has already been 
## calculated, if yes, return the inverse, if not, then calcuate the 
## inverse then return it. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() # get the value of inverse from the fisrt function 
                              # it could be null or a matrix
    if(!is.null(inverse)){    # check whether vector named inverse is null
          message("getting inverse from the cache...") # if it's not null, print this message
          return(inverse) # then get the inverse matrix  
    }
    matrix <- x$get() # if vector named inverse is null, assign the matrix value to the vector matrix
    inverse <- solve(matrix,...) # slove the matrix (get the inverse)
    x$setinverse(inverse)        # then, assign this value to the vector named "inverse" 
    inverse  # finally, return the inverse as out put of this function
}

