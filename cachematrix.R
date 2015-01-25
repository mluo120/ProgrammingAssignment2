## Assignment 2
## A pair of functions to compute and cache the inverse of a matrix.


## This function creates a special "matrix" object that 
##    can cache a value.
## Assumption: x is a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL    		# clear cache when matrix x is set/changed
    }
    get <- function() x	
    setCache <- function(data) m <<- data
    getCache <- function() m
    
    list(	set = set,			# fct to set matrix x
          get = get,			# fct to get matrix x
          setCache = setCache,	# fct to set cache
          getCache = getCache)	# fct to get cache
}


## This function computes and caches the inverse of the special "matrix" xx
##  returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix 
##  has not changed) then the cached inverse is retuned.

cacheSolve <- function(xx, ...) {
    m <- xx$getCache()    	# get cached inverse
    if(is.null(m)) {
        x <- xx$get()		# x is assumed invertible
        m <- solve(x, ...)	# compute the inverse		
        xx$setCache(m)		# store the inverse in cache
    } 
    m
}


# test:
# a<-matrix(1:4,2,2)
# b<-makeCacheMatrix(a)
# c<-cacheSolve(b)