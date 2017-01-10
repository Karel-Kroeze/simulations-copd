# Create Mapping
# takes a matrix (or any 2d object) and collapses categories to where each category has at least threshold responses.
# rather naieve, will collapse to 1 category without warning if data requires.
# 
# returns a list of (named) lists, each with two elements - the old and new response category indeces.
# these can easily be used to recode, for example with recode() from the "car" package.
# 
# Author: Karel A. Kroeze <k.a.kroeze@utwente.nl>


# Start from the right, and collapse to the left whenever count < threshold
# Finally, collapse once more if first element is also too low.
create_mapping <- function(data, threshold) {
  counts <- list()
  for (i in 1:ncol(data)) {
    counts[[i]] <- table(factor(data[,i], levels = 1:5))
  }
  
  lapply(counts, function(count) {
    # number of response categories, k
    k <- length(count)
    # set up mapping lists
    old <- as.numeric(names(count))
    new <- 1:k-1
    # note that the count variable is only used as a temporary bookkeeping mechanism, old and new form the actual mapping.
    
    # loop over response cats, right to left up to second element.
    for (i in k:2){
      # if category is too low, add it to the left.
      if (count[i] < threshold){
        # update count for next step
        count[i-1] <- count[i-1] + count[i]
        count[i] <- 0
        
        # update new mapping
        new[i:k] <- new[i:k] - 1
      }
    }
    
    # first element
    if (count[1] < threshold){
      # sum first elements until threshold is reached
      for (i in 1:k) {
        if (sum(count[1:i]) >= threshold) break
      }
      
      # set counts
      count[i] <- sum(count[1:i])
      count[-(i:k)] <- 0
      
      # set new
      new[1:i] <- new[i+1] - 1
    }
    
    # start at zero
    new = new - min(new, na.rm = TRUE)
    
    # reverse direction (for Maarten, probably unnecessary)
    new <- rev(new)
    old <- rev(old)
    
    if(length(unique(new)) == 1) warning("Only one response category left after recoding.")
    
    list(old = old, new = new)
  })
}
