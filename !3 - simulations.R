###### simulations.R
# This is the main file for running simulations on data prepared by main.R.
# Author: Karel A. Kroeze <k.a.kroeze@utwente.nl>
# required packages
require(Utilities)
Require("doParallel")                 # speed up simulations by running them in parallel
require(devtools)
# install_github("Karel-Kroeze/MultiGHQuad")
Require("ShadowCAT", github = "Karel-Kroeze/ShadowCAT")                  # Allows MCAT simulations. (github.com/Karel-Kroeze/ShadowCAT)
Require("mvtnorm")                    # Multivariate normal distribution, required to sample persons

# working dir
setwd("D:/Profiles/kroezeKa/Dropbox/COPD/calibration+simulation/")
#setwd("C:/Users/Karel/Dropbox/COPD/calibration+simulation/")

# run name
run_name <- "17-6-2016"

# store intermediate results? (for long simulations where power outages/pc reboots might be a concern)
make_backups <- TRUE

# test setups
# max_n is set in simulate. TODO: migrate max_n to an option in this file.
n <- 1000                                     # persons to simulate in each test
mail <- FALSE                                 # send a "done" email
estimator <- "MAP"                            # use maximum a-posteriori estimates
objective <- "PD"                             # posterior determinant of Fisher information (Segall) as objective
selection <- "MI"                             # items are selected by max(objective)
theta_range <- c(-2,2)                        # min/max theta to simulate
theta_intervals <- .2                         # range between consecutive theta values

# ignore specific models
ignored_models <- c("fullmulti") # don't run the mirt model, IRTPRO is enough
ignored_combinations <- list(
                          # don't run a quasi-unidimensional test with multi pars
                          list( 
                            model = "irtpro", 
                            prior = "normal" ),
                          
                          # don't run a multi test with uni pars
                          list( model = "bydomain",
                                prior = "model" )
                          )


# custom functions
source("functions/simulate.R")      # simulation wrapper
source("functions/convert_from_IRTPRO.R") # convert IRTPRO output into a coefficient / covariance csv's


# 0. Set up options -------------------------------------------------------
# set up a parallel cluster. In theory one could go up to the number of cores (use detectCores()), but leaving 1-2 free is recommended to allow the computer to perform other tasks while simulations are running.
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

# change default R data loading
options(stringsAsFactors = FALSE)

# create temp dir
if (make_backups){
  backup_dir <- paste0("temp_sims/", run_name)
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE )
}


# variable test setups
# starting rules
starts <- list(rbd1 = list(type = "randomByDimension",     # 1 or 3 random items per dimension
                    n = 4,
                    nByDimension = 1),
               rbd3 = list(type = "randomByDimension",
                    n = 12,
                    nByDimension = 3))

# stopping rules
stops <- list(fixed10 = list(type = "length",        
                   n = 10),
              fixed20 = list(type = "length",        
                             n = 20),
              fixed30 = list(type = "length",       
                             n = 30),
              variance10 = list(type = "variance",
                                target = .1),
              variance15 = list(type = "variance",
                                target = .15))

# starting and stopping are supported in ShadowCAT, true theta and prior (below) require some additional setup in the simulate() function.
# data for answer simulations
thetas <- seq(theta_range[1], theta_range[2], theta_intervals )          # specific theta levels to simulate
responses <- read.csv("output/data/formirt/data_ordered_droppedquestions_internationalformat.csv", na.strings = 9)[,-1]
data <- list(random = NA, 
             fixed = NA, 
             empirical = responses)

# answer simulations
trues <- trues_names <- c(random = "random", 
                          fixed = paste0("fixed:", thetas) , 
                          empirical = "empirical")        # simulate answers from a random theta, fixed theta, or use empirical answers.

# prior distributions
priors <- priors_names <- c(model = "model", 
                            # uninformative = "uninformative", 
                            normal = "normal" )     # prior distribution to use

# 1. Load models ----------------------------------------------------------
pars <- list.files("output/coefs", "^pars")
covs <- list.files("output/coefs", "^cov")

if (length(pars) != length(covs)) stop("Number of parameter and covariance matrices do not match.")

# models
models <- list()
models_names <- character()
for (i in 1:length(pars)){
  nameparts <- strsplit(pars, "\\.")[[i]]
  models_names <- c(models_names, paste0(nameparts[3:length(nameparts)-1],collapse = ""))
  
  par <- read.csv2(paste0("output/coefs/", pars[[i]]), row.names = 1)
  cov <- read.csv2(paste0("output/coefs/", covs[[i]]), row.names = 1)
  cov <- as.matrix(cov)
  
  # check if cov is diagonal
  if(any(is.na(cov))) {
    # make full matrix
    cov[is.na(cov)] <- 0
    cov <- cov + t(cov)
    diag(cov) <- diag(cov)/2
  }
  
  models[[i]] <- list(pars = par, cov = cov)
}
names(models) <- models_names

# take out ignored models
for (ignored_model in ignored_models){
  models[[ignored_model]] <- NULL
}

# 2. Set up simulation combinations ---------------------------------------------------
# all unique combinations of options (combinatorial, will be a long list!)
sims <- expand.grid(model = names(models), start = names(starts), stop = names(stops), true = names(trues), prior = names(priors), stringsAsFactors = FALSE)

# remove ignored combinations
for (ignored_combination in ignored_combinations ){
  cols <- names(ignored_combination)
  ignore <- which( sims[,cols[1]] == ignored_combination[1] & sims[,cols[2]] == ignored_combination[2] )
  sims <- sims[-ignore,]
}

# spit out some info
cat(nrow(sims), "simulations will be performed, for a total of roughly", nrow(sims) * n, "simulated respondents.\n\n")

# make list
SIMS <- apply(sims, 1, as.list)
SIMS <- lapply(SIMS, function(x) {
  out <- list()
  out$model <- models[[x$model]]
  out$start <- starts[[x$start]]
  out$stop <- stops[[x$stop]]
  out$prior <- priors[[x$prior]]
  out$true <- trues[[x$true]]
  out$data <- data[[out$true]]
  return(out)
})

# 3. Run the simulations --------------------------------------------------
for (i in 1:length(SIMS)) {
  sim_name <- paste0(colnames(sims), ":", sims[i,], collapse = " ")
  SIMS[[i]]$results <- simulate(n, estimator, objective, selection, SIMS[[i]], sim_name, cl, make_backups, i, run_name)
  cat("\rDone with", i, "out of", length(SIMS), " [", sim_name,"]")
}

dir.create( "output/simulation results/", showWarnings = FALSE, recursive = TRUE )
save(SIMS, file = paste0("output/simulation results/", run_name, "_sim_results.rda"))
if (make_backups){
  # remove traces
  unlink(backup_dir, recursive = TRUE) 
}

inspect <- function(sim) {
  sim <- sim$results
  bias <- round(rowMeans(sim$err), 2)
  MSE <- round(rowMeans(sim$err^2), 2)
  RMSE <- round(MSE^.5, 2)
  length <- round(mean(sapply(sim$adm, length)), 1)
  exposure <- numeric(sim$full$test$items$K)
  for (resp in sim$adm) for (index in resp) exposure[index] <- exposure[index] + 1
  exposure <- exposure / n
  order <- order(-exposure)
  string <- paste0("\n++++++++++++++\n",sim$name," (N = ", n, ")\nBias\n",
                   paste0(1:4, ": ", bias, collapse = '\n'),
                   "\n\nMSE\n",
                   paste0(1:4, ": ", MSE, collapse = '\n'),
                   "\n\nRMSE\n",
                   paste0(1:4, ": ", RMSE, collapse = '\n'),
                   "\n\nAverage length\n",
                   length
                   #                    ,
                   #                    "\n\nItem exposure\n",
                   #                    paste0(sprintf("%8s",itemnames)[(1:length(exposure))[order]], ": ", round(exposure[order],2), collapse = '\n'),
                   #                    "\n\n\n+++++++++++++"
                   
                   
  )
  return(invisible(string))
}

cat(sapply(SIMS, inspect))

##### this is just playing around
# install.packages("sendmailR")
require(sendmailR)
sendmail_options(smtpServer = "smtp.utwente.nl", verbose = TRUE)
if (mail) sendmail(from = "<k.a.kroeze@utwente.nl>",
                   to = c("<karel.kroeze@gmail.com>","<k.a.kroeze@utwente.nl>"),
                   #cc = "<m.c.s.paap@utwente.nl>",
                   subject = "Simulation finished.",
                   msg = list("Simulations have finished, congratulations!",
                              paste0(sapply(results, inspect))))

