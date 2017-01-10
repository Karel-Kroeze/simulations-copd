#### when fed with a model and a response matrix, returns empirical theta for each respondent.
get_empirical_thetas <- function( test, prior, data ){
  
  data <- as.matrix( data )
  thetas <- matrix(NA, nrow(data), nrow(prior))
  
  for (i in 1:nrow(data)){
    person <- initPerson( test$items, prior = prior )
    
    person$administered <- which(!is.na(data[i,]))
    person$responses <- data[i,person$administered]
    thetas[i,] <- estimate(person, test)$estimate
  }
  
  return(thetas)
}