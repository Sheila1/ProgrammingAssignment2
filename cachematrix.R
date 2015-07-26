## Based on "Caching the Mean of a Vector" example

## This functions calculates the inverse of a matrix and store the matrix 
## and it's inverse into  cache

## makeCacheMatrix: This function returns a list of functions for set and get 
## a matrix and get and set it's inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL 
    
    set <- function(matrix1) 
    {
        x <<- matrix1  ## set matrix x to cache
        i <<- NULL ##Set i to cache with null value
    }

    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse ##set the inverse of the matrix x to cache
    
    getinverse <- function() i

    lista <- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    return(lista)
}


## cacheSolve: This function returns the inverse matrix, uses the getinverse function resulting from the above function
## First checks wheter if the inverse matrix exists in the cache, otherwise uses the "solve" function to calculate 
## the inverse matrix

cacheSolve <- function(x, ...) 
{
    matrix2 <- x$getinverse()

    if(is.null(matrix2))
    {
        data <- x$get()
        matrix2 <- solve(data,...) 
        x$setinverse(matrix2)
    }
    else
    {
        message("Reading data from cache")
    }
    
    return(matrix2)
}
