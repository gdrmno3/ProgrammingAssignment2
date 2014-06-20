## Purpose of these functions are to be able to cache the inverse (by using the Solve() function in R), of a matrix.
## So it will not compute repeating the same inverse matrix, and save it in a list.
## Assumes that the matrix is invertible

## Example: create a 2:2 matrix with value 1:4 then inverse the matrix, and repeat:

## Test1 <- makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))
## Test2 <- cacheSolve(Test1)
## Test3 <- cacheSolve(Test1)

## Test3 will use the cache inverse value of matrix Test2 instead of recomputing the inverse value again



## makeCacheMatrix function creates a list containing a function to set and get values of special matrix
## and set and get values of the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of cached data
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## cacheSolve function calculates the inverse of the matrix object created by makeCacheMatrix function
## using the Solve() function. It checks if inverse has been computed. If true then it get the value
## from the the cache list, else it calculate the inverse matrix and saves the value in the cache list object.

cacheSolve <- function(x, ...) {
        ## Return a inverse matrix created in makeCacheMatrix.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## calculate the iverse of matrix using the solve() function.
        m <- solve(data)
        x$setinverse(m)
        m
}
