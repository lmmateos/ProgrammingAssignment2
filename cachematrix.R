## R Programming, Programming Assignment 2, louise.mateos@sbcglobal.net

## Put comments here that give an overall description of what your
## functions do

## Functions to cache the inverse of a matrix and check to see if inverse is already cached
## when performing matrix inversion

## Write a short comment describing this function

## Function to create a special matrix, which is returns a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
       
}


## Write a short comment describing this function

## Function to invert a special matrix created with "makeCacheMatrix" function that checks 
## to see if inversion is already cached.  If already cached, the function returns the cache 
## contents instead of performing the inversion again.  If not cached, the function performs
## the inversion and caches it as well as returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
