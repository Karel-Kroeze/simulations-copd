#### when fed with a model and a response matrix, returns empirical theta for each respondent.
impute_full_data <- function( test, prior, data, thetas ){
  
  data <- as.matrix( data )
  
  for (i in 1:nrow(data)){
    person <- initPerson( test$items, thetas[i,], prior = prior )
    
    na_items <- which(is.na(data[i,]))
    data[i,na_items] <- answer(person, test, na_items)$responses
  }
  
  return(data)
}