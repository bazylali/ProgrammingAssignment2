## Student: Ali BAZYL. Email: bazyl.ali@gmail.com
## Course R Programming - Assignment week 3
## This assignment demonstrates lexical scoping in R
## on the primer of calculating and caching the inverse matrix

##This function stores the input matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set_matrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get_matrix <- function() x
        set_inv_matrix <- function(inv_matrix_temp) inv_matrix <<- inv_matrix_temp
        get_inv_matrix <- function() inv_matrix
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}

## This function either retrieves the stored inverse matrix or calculates
## the inverse matrix, if it was not calculated before
cacheSolve <- function(x, ...) {
    # read inverse matrix variable
    inv_matrix <- x$get_inv_matrix()
    # if it is not empty, return the cached inverrse matrix
    if(!is.null(inv_matrix)) {
        message("Returning cached inverse matrix")
        return(inv_matrix)
    }
    # if it is empty - 1.read the input matrix, 2.calculate the inverse 
    # with solve function, 3.store it in global environment, 4.return inverse matrix
    data <- x$get_matrix()
    inv_matrix <- solve(data)
    x$set_inv_matrix(inv_matrix)
    inv_matrix
}


## TESTING
aMatrix <- makeCacheMatrix(matrix(1:4,2,2))
aMatrix$get_matrix()
aMatrix$get_inv_matrix()
aMatrix$set_matrix(matrix(2:5,2,2))
cacheSolve(aMatrix)
aMatrix$get_inv_matrix()
