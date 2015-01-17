## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(Values = Matrix()) 
{
    ## DR: Create the caching functions. Only functions to read the inverted Matrix already calculated

    Mat <- NULL
    
    ##Function to manage the values
    Set <- function(x) 
    {
        Values <<- x
    }
    
    ##Prepare the Get and Set functions
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
    ## Let's try to get the cache
    InvMat <- x$GetInv()
    ## The cache exists? Then return it
    if (!is.null(InvMat)) 
    {
        return(InvMat)
    }
    ## Else, calculate it an set the according function to generate the cache
    InvMat <- solve(x$Get())
    x$SetInv(InvMat)
    
    ##Return the calculation
    return(InvMat)
    
}
