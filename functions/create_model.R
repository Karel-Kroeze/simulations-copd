### create mirt model function
# author: Karel Kroeze <k.a.kroeze@utwente.nl>
# sets up a mirt model definition from a data file with named items on the columns, DATA
# obtains correct factor/column numberings from FACTORS
#   FACTORS should be a list of named vectors for each dimension, containing item names that belong to that dimension
# returns model definition string
create_model <- function(data, factors, silent = FALSE){
  model <- ""
  for (factor in names(factors)){
    # name it
    model <- paste0(model, factor, "=")
    
    # fetch numbering
    indeces <- which(colnames(data) %in% factors[[factor]])
    model <- paste0(model, paste0(indeces, collapse = ','))
    
    # end line
    model <- paste0(model, "\n")

  }
  # COV clause
  model <- paste0(model, "COV=", paste0(names(factors), collapse = "*"))
  
  # done!
  return(model)
}