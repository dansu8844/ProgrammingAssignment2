## The two functions below are used to cache the inverse of a matrix.
## The computed inverse of a matrix will be cached and the input
## new matrix will be computed to its inverse matrix.

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL                       ## Create a NULL as a place. 
                                         ## holder for inverse matrix.
        set<-function(y){                ## A function to set the matrix.
                x<<-y                    ## Introduce a new matrix.
                inv<<-NULL               ## Reset inverse matrix.
        }
        get<- function() x               ## Return a matrix.
        setInverseMatrix<- function (inverse) inv <<-inverse 
        ## Set the inverse matrix.
        getInverseMatrix<- function() inv## return the inverse matrix.
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
}


## Computes the inverse of the special"matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
z<- makeCacheMatrix()
cacheSolve <- function(z, ...) {
        inv<-z$getInverseMatrix()  
        ## Get the m value from the above function.
        if(!is.null(inv)){                
        ## Check whether the inverse matrix has been calculated.
                message("getting cached inverse matrix")
                return (inv)              ## Return cached inverse matrix.
        }
        data<-z$get()                     ## Get the matrix.
        inv <-solve(data, ...)            ## Compute the inverse matrix.
        z$setInverseMatrix(inv)           ## Cache the inverse matrix.
        inv        
        ## Return a matrix that is the inverse of 'x'
}