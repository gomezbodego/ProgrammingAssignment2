#Function to cache the matrix and its inverse
makeCacheMatrix <- function(m = numeric()) {
  #m is the natrix and m_1 its inverse
  #Initially the inverse (m_1) has not been calculated
  m_1 <- NULL
  #store the matrix but not the inverse
  set <- function(y) {
    m <<- y
    m_1 <<- NULL
  }
  #Store the matrix
  get <- function() m
  #Clacule the inverse
  setinv <- function(solve) m_1 <<- solve
  #Store the inverse
  getinv <- function() m_1
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(m, ...) {
  #Put on m_1 the value stored for the inverse
  m_1 <- m$getinv()
  #if there is something stored in the inverse
  #shows a message and return the stored value
  if(!is.null(m_1)) {
    message("getting cached data")
    return(m_1)
  }
  #If there is nothing stored in the inverse, computes de inverse
  #Store this inverse with the matrix and shows the inverse
  data <- m$get()
  m_1 <- solve(data, ...)
  m$setinv(m_1)
  m_1
}