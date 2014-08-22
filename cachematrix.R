## The first function takes a matrix as its argument and returns an object
## that is a list of functions that are associated with that matrix and 
## can be used to store and return that matrix and store and return 
## (but not actually compute) its inverse.

## The second function takes a list output by the first function as its
## argument.  It uses the functions within that list to return the inverse
## of the matrix associated with the input argument.  If the matrix's
## inverse has already been stored within the functions that make up
## the argument to cacheSolve, then just return the already-stored inverse.
## Else, calculate it and modify the input argument using superassignment
## so that the inverse is stored.



## makeCacheMatrix takes an invertible matrix as its argument
## and returns an object that is a list, each element of which is
## a function
makeCacheMatrix <- function(x = matrix())  {
        xinv<-NULL 
        set<-function(y){
                x<<-y
                xinv<<-NULL
        }
        get<-function() x
        setinv<-function(xinvarg) xinv<<-xinvarg
        getinv<-function() xinv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve takes a list of function elements that was created by 
## makeCacheMatrix as its argument.  If the matrix has not yet been
## inverted, it inverts it and returns and stores the inverse.
## If the inverse has already been calculated, it returns the stored
## inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theinverse<-x$getinv() 
        if(!is.null(theinverse)) {
                message("getting cached matrix")
                return(theinverse) 
        }
        originalmatrix<-x$get()
        theinverse<-solve(originalmatrix) 
        x$setinv(theinverse)          
        theinverse
}



