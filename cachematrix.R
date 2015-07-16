## makeCacheMatrix - the function (object) that gets an input matrix 
## and cashes (stores) it in local variable (x). 
## The function returns list of 4 methods:
## get - returnes value of cashed input matrix;
## set - method by wich performed the cashing of input matrix 
##       and zeroing(*) the (also cashed) inerse matrix inver; 
## setinv - method that cashes inverse matrix to local variable inver
## getinv - method that returns the value of cashed inverse matrix
## 
## (*) Zeroing of inverse matrix is necessary at the step of changing the input matrix 

makeCacheMatrix <- function(x = matrix()) {
    inver<-NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inver <<- inv
    getinv <- function() inver
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The  function cacheSolve gets the function (object), created be function makeCacheMatrix
## By method getinv it gets the cashed value of inversed matrix , 
## and if it exists - returns it to the user. If is not exists - uses by other methods
## to get the data, calculates the inverse matrix and cashe it to the object

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
    
}

#x=matrix(1:4,2,2)
#M=makeCacheMatrix(x)
#debug(cacheSolve)
#cacheSolve(M)
