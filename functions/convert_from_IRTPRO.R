# Function to convert IRTPRO .txt results into workable set of .csv's
convert_from_IRTPRO <- function(path) {
  data <- readLines(path)
  
  # take relevant item information from data
  items <- lapply(data[-length(data)], function(x){
    item <- strsplit(x, "\t")[[1]]
    name <- item[1]
    item <- as.numeric(item)
    q <- item[2]
    m <- item[4]
    a <- item[5:(5+q-1)]
    b <- -item[(5+q):(5+q+m-2)]
    list(name = name, q = q, m = m, a = a, b = b)
  })
  
  # get max number of categories and dimensionality
  M <- max(sapply(items, function(x) x$m))
  Q <- max(sapply(items, function(x) x$q))
  
  # create parameter matrix
  pars <- matrix(NA, length(items), M+Q-1)
  names <- character(length(items))
  
  # fill the parameter matrix
  for (i in 1:nrow(pars)) {
    pars[i, 1:Q] <- items[[i]]$a
    pars[i, (Q+1):(Q+items[[i]]$m-1)] <- items[[i]]$b
    names[i] <- gsub('\"', "", items[[i]]$name)
  }
  rownames(pars) <- names
  colnames(pars) <- c(paste0("a", 1:Q), paste0("b", 1:(M-1)))
  
  # create the covariance matrix
  covdata <- as.numeric(strsplit(data[[length(data)]], "\t")[[1]][-(1:(Q+3))])
  cov <- matrix(NA, Q, Q)
  
  k <- 1
  for(i in 1:Q){
    for(j in 1:i){
      cov[i, j] <- cov[j, i] <- covdata[k]
      k <- k+1
    }
  }
  
  means <- rep(0, Q)
  colnames(cov) <- rownames(cov) <- names(means) <- paste0("F", 1:Q)
  
  return(list(items = pars, cov = cov, means = means))
}