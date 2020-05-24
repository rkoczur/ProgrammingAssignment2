## makeCacheMatrix and cacheSolve functions in a pair allow to compute the inverse of a matrix 
## loaded into makeCacheMatrix. However if the inverse is alrady computed, it is stored in
## the enviroment of the makeCacheMatrix together with the original matrix. 

## the makeCacheMatrix function takes a matrix as an argument, and store it together with
## setter and getter functions. The inverse variable stores the inverse of the matrix in 
## the same object. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(sourceMatrix) {
                x <<- sourceMatrix
                inverse <<- NULL
        }
        get <- function() x
        makeInverse <- function() {
                inverse <<- solve(x)
        }
        getInverse <- function() inverse
        list(set = set, get = get,
             makeInverse = makeInverse,
             getInverse = getInverse)
}


## If you feed an object created with the makeCachematrix to cacheSolve, it returns with
## the inverse of the original matrix. But if it is already computed, it get the data with
## getInverse function, so it no need to compute again.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                print("getting inverse from cache")
                return(inverse)
        }
        inverse <- x$makeInverse()
        inverse
}
