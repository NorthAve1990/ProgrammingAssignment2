## makeCashMatrix creates vector which is list containing function
## what functions do is setting the value of the vector into "set" variable
## getting the value of the vector into "get" variable
## setting the value of the mean into "setmean" variable
## getting the value of the mean into "getmean" variable

## Write a short comment describing this function

makeCacheMatrix <- function(listData = matrix()) {
        mat <- NULL
        set <- function(ld) {
                listData <<- ld
                mar <<- NULL
        }
        get <- function() listData
        setmean <- function(mean) mat <<- mean
        getmean <- function() mat
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## cacheSolve function computes the inverse of the matrix. If the inverse
## has alreayd been calculated then the cachesolve should retrive the inverse
## from the cache.

cacheSolve <- function(listData, ...) {
        mat <- listData$getmean()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- listData$get()
        mat <- mean(data, ...)
        listData$setmean(mat)
        mat
}
