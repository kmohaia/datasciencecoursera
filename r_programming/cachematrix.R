#!/usr/bin/env Rscript


## special matrix object is created by this function
## it can cache the inverse of the matrix
## it provides methods for set matrix
makeCacheMatrix <- function(mtx = matrix()) {

	# intialise inverse variable as NULL
	inverse <- NULL
	
	#
	set_matrix <- function(x){
		mtx <<- x;
		inverse <<- NULL;
	}

	get_matrix <- function() return(mtx);

	set_inv <- function(inv) inverse <<- inv;
	get_inv <- function() return (inverse);

	return(list(set=set_matrix, get=get_matrix, set_inv=set_inv, get_inv=get_inv))
}	


## this function computes the inverse of the matrix
## if the inverse has been calculated then it should be cached
## in that case it will only return the cached value instead of 
## computing the inverse

cacheSolve <- function(mtx, ...) {
	
	# retrieve the cached inverse of the matrix
	inverse <- mtx$get_inv()

	# check whether the inverse is null 
	if(!is.null(inverse)){
		message("Retreving cached inverse..")
		# returns inverse if it is not null
		return(inverse)
	}

	# retrieve the actual matrix
	data <- mtx$get_matrix()
	# calculcate inverse
	inverse <- solve(data, ...)
	# cache the inverse
	mtx$set_inv(inverse)
	# return the inverse	
	return(inverse)
}

