## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL    #initialize object inverse matrix object
    set <- function(y) {  #define the behavior for objects of type 
        x <<- y #set the value of x
        invX <<- NULL #null the value of inverse Matrix
    }
    get <- function() x #get the value of x retrieve from parent environment 
    setInverse <- function(inverseF) invX <<- inverseF #define m from parent environment to mean
    getInverse <- function() invX #retreive the value of m as getmean
    list(set = set, # gives the name 'set' to the set() function defined above
         get = get,# gives the name 'get' to the get() function defined above
         setInverse = setInverse, # gives the name 'setmean' to the setmean() function defined above
         getInverse = getInverse)# gives the name 'getmean' to the getmean() function defined above
    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invX <- x$getInverse() #attempts to retrieve an inverse Matrix from the object
    if(!is.null(invX)) { #if there is an inverse Matrix in the object, the function stop and return invX
        message("getting cached data")
        return(invX)
    }
    data <- x$get() # if invX=Null, calculate the inverse Matrix of x object
    invX <- solve(data)
    x$setInverse(invX) #set calculated inverse Matrix to object x
    invX
}
