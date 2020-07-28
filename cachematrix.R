## makeCacheMatrix creates a special "Matrix" object that can cache its inverse
## For example
## mat <- c(1,-4,-6,0,0,-3,3,1,2,1,4,7,0,5,1,0)
## matr <- makeCacheMatrix(matrix(mat, 4,4))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){ ## Sets the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x ## Get the matrix value
        setinverse <- function(inverse) i <<- inverse ##Cache the matrix
        getinverse <- function() i ## Calls the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## List of the resulted attributes
}


## cacheSolve calculates de inverse from de matrix resulted at makeCacheMatrix.
## If it has been calculated, cacheSolve should retrieve the result from
## the cached environment. On the other hand, it will be calculated.
## For example
## cacheSolve(matr)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){ ## Testing cached results
                message("Getting cached data, please wait")
                return(i)
        }
        somematrix <- x$get()
        i <- solve(somematrix,...) ## Calculating inverse
        x$setinverse(i)
        i
}