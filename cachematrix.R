## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(Values = Matrix()) 
{
    ## DR: Create the caching functions. Only functions to read the inverted Matrix already calculated
    Mat <- NULL
    
    Set <- function(x) 
    {
        Values <<- x
    }
    
    Get <- function() Values
    SetInv <- function(Inv) Mat <<- Inv
    GetInv <- function() Mat
    list(Set = Set, Get = Get, SetInv = SetInv, GetInv = GetInv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x) 
{
## Return a matrix that is the inverse of 'x'

    ## DR: If we know the inverse of the matrix, we read the cache, else, calculate it!
    InvMat <- x$GetInv()

    if (!is.null(InvMat)) 
    {
        return(InvMat)
    }
    InvMat <- solve(x$Get())
    x$SetInv(InvMat)
    
    return(InvMat)
    
}
