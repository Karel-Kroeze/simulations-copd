# extra functions required
source("functions/get_empirical_thetas.R")
source("functions/impute_full_data.R")

# simulation wrapper.
simulate <- function(n, estimator, objective, selection, sim, name, cluster = NULL, make_backups = FALSE, i = NULL, run_name = NULL) {
  
  # fixed upper limit to test length in variable length tests
  max_n = 100
  
  # if making backups, check if there is a backup available
  if (make_backups & !is.null(i) & !is.null(run_name)){
    temp_file <- paste0("temp_sims/", run_name, "/", i, ".rda")
    if ( file.exists( temp_file ) ){
      temp.env <- new.env()
      temp.name <- load(temp_file, temp.env)
      return(get(temp.name, temp.env))
    }
  }
  
  # sim contains model (pars and cov), start, stop, prior (type), and theta/answer simulation type.
  # set up simulation constants
  a <- as.matrix(sim$model$pars[,1:4])
  b <- as.matrix(sim$model$pars[,5:8])
  items <- initItembank('GRM', a, b, silent = TRUE)
  test <- initTest(items, sim$start, sim$stop, max_n, estimator, objective, selection)
  
  # get correct prior
  if (sim$prior == "model")
    prior <- sim$model$cov
  
  if (sim$prior == "normal")
    prior <- diag(4)
  
  if (sim$prior == "uninformative")
    prior <- diag(4)*10
  
  # for empirical simulations, we need to do some extra work
  if (sim$true == "empirical"){
    
    # remove data we don't have parameters for (removed)
    sim$data <- sim$data[,rownames(sim$model$pars)]
      
    # remove missing data points
    sim$data <- sim$data[which(apply(sim$data,1,function(row) !all(is.na(row)))),]
    
    # set length to minimum of desired length or data points
    n <- min(n, nrow(sim$data))
    
    # get theta scores for all respondents.
    empirical_thetas <- get_empirical_thetas( test, prior, sim$data )
    
    # impute full data set (single imputation)
    imputed_data <- impute_full_data( test, prior, sim$data, empirical_thetas )
  } 
  
  # start the simulation
  results <- foreach(i = icount(n), .inorder = FALSE, .packages = c('mvtnorm','MultiGHQuad','ShadowCAT'), .verbose = FALSE) %dopar% {
  # Use replicate instead of foreach for traceback in bugfixing
  # replicate( n, {
      # set theta
    if (grepl("fixed", sim$true)){
      theta <- rep(as.numeric(strsplit(sim$true, ":")[[1]][2]), 4)
    } else if (sim$true == "empirical") {
      theta <- empirical_thetas[i,]
    } else {
      theta <- rmvnorm(1, rep(0, 4), sim$model$cov)
    }
    
    # init person
    person <- initPerson(items, theta, prior)
    
    # run the cat
    if (sim$true == "empirical"){
      person <- ShadowCAT( person, test, responses = imputed_data[i,] )
    } else {
      person <- ShadowCAT( person, test )
    }
    
    # return person object
    person
  }
  # ) # uncomment when using replicate
  
  thetas <- sapply(results, function(x) x$theta)
  estimates <- sapply(results, function(x) x$estimate)
  deviations <- estimates - thetas
  administered <- lapply(results, function(x) x$administered)
  
  RESULT <- list(name = name,
                 th = thetas,
                 est = estimates,
                 err = deviations,
                 adm = administered,
                 full = list(results = results,
                             test = test))
  
  # store the intermediate result
  if (make_backups & !is.null(i) & !is.null(run_name)){
    temp_file <- paste0("temp_sims/", run_name, "/", i, ".rda")
    save(RESULT, file = temp_file)
  }
  
  return(RESULT)
  # return(list(name = name, results = results))
}