### fix_values
# author: Karel Kroeze <k.a.kroeze@utwente.nl>
# sets starting estimators in VALUES to those in COEFS
# if fix = TRUE, fixes these values (default)
# mean and covariance fixing are enabled by default, but can be turned off with options
# if mean and covariance are enabled, they are ALWAYS fixed
fix_values <- function(values, coefs, fix = TRUE, mean = TRUE, cov = TRUE, verbose = FALSE){
  names <- rownames(coefs$items)
  pars <- colnames(coefs$items)
  
  for (i in 1:nrow(coefs$items)){
    for (j in 1:ncol(coefs$items)){
      if (! is.na(coefs$items[i,j])){
        index <- which(values$item == names[i] & values$name == pars[j])
        if (verbose) cat(names[i], pars[j], 'parnum:', index, ifelse(fix, 'fixed', 'start'), 'at', coefs$items[i,j], "\n")
        values$value[index] <- coefs$items[i,j]
        if (fix) values$est[index] <- !fix
      }
    }
  }
  
  if (mean){
    for (i in 1:length(coefs$means)){
      index <- which(values$item == "GROUP" & values$name == paste0("MEAN_", i))
      if (verbose) cat(paste0("MEAN_", i), 'parnum:', index, 'fixed at', coefs$means[i], "\n")
      values$value[index] <- coefs$means[i]
      values$est[index] <- FALSE
    }
  }
  
  if (cov){
    for (i in 1:nrow(coefs$cov)){
      for (j in 1:i){
        index <- which(values$item == "GROUP" & values$name == paste0("COV_", i, j))
        if (verbose) cat(paste0("COV_", i, j), 'parnum:', index, 'fixed at', coefs$cov[i,j], "\n")
        values$value[index] <- coefs$cov[i,j]
        values$est[index] <- FALSE
      }
    }
  }
  
  return(invisible(values))
}
