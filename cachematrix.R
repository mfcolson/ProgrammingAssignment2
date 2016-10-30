## The makeCacheMatrix evaluates a given square, invertible matrix and allocates memory to the result 
#solve() being called on it. It then calls solve(), and stores the resulting inverted matrix compactly
#as hexagesimal numbers.
## cacheSolve takes in the hexagesimal cached inversion of the matrix and returns it as a matrix 
#that is readible.

## This function is an adaptation of the MakeVector() function int he example prompt. It was honestly, 
#not that extensive of a modification, except in exchanging the function "solve()" for mean(), and
#matrix for numeric. Some of the variable names needed to be changed to replace "mean" with "Inv" for clarity
#reading the output is a little cryptic since it somes in numbers like <environment: 0x0000000015636138>


makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function is again an adaptation of the example in Assignment 2. I changed the variables as
#necessary for clarity namely, getmean to getinv and setmean to setinv. then m got solve()
# instead of mean. The input to this function must me the return value of makeCacheMatrix, so it could
#be called like cacheSolve(makeCacheMatrix(B)) or by saving makeCacheMatrix as a separate variable. 


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

cacheSolve(makeCacheMatrix(x))
}
