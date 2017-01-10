###### main.R
# This is the main runfile for cleaning, re-coding, re-ordering of data, calibration of models, and preparing parameter and T conversion tables for COPD MCAT.
# Author: Karel A. Kroeze <k.a.kroeze@utwente.nl>
# required packages
require(Utilities) # github.com/Karel-Kroeze/Utilities
Require('foreign')  # for read.spss()                   (step 1)
Require('openxlsx') # read/write xlsx                   (step 2, 12)
Require('car')      # for recode()                      (step 6)
Require('mirt')     # required for... I forgot... Seems important though? (steps 9-11)
Require('CTT')      # for CTT() [theta -> T]            (step 13a)
Require('reshape2') # for casting data in long/wide formats (step 13a/b)
Require('ggplot2')  # for plotting density plots        (step 13a)

# custom functions
source("functions/create_mapping.R")# for create_mapping()      (step 5)
source("functions/create_model.R")  # for create_model()        (step 8)
source("functions/fix_values.R")    # for fix_values()          (steps 10-11)
source("functions/convert_from_IRTPRO.R") # convert IRTPRO output into a coefficient / covariance csv's (step 12)


# 0. Specify options ------------------------------------------------------
# minimum number of responses per category (step 5)
count.threshold <- 10

# calibration method, see ?mirt (steps 9-11)
method <- "MHRM" # other options are "ME" (not recommended for full models) and "MHRM"

# IRTPRO output file (relative to current wd)
IRTPRO_output_file <- "../IRTPRO_output/mei16/data_ordered_droppedquestions_internationalformat.Test1-prm.txt"
# IRTPRO_output_file <- "../IRTPRO_output/december/full_data.Test1-prm.txt"

# dropped questions
dropped_questions <- c("ML_18", "ML_19", "ML_20", "ML_21", "ML_24", "ML_27", "ML_32", "ML_34", "ML_39", "ML_46", "ML_47", "SGRQ_1", "SGRQ_2", "SGRQ_4", "SGRQ_14", "SGRQ_15", "SGRQ_16")
# dropped_questions <- c()

# desired background variables
background_variables <- c("WERK", "LEEFTIJD")

# 1. Load data ------------------------------------------------------------
# override default R behaviour
SAF <- options(stringsAsFactors = FALSE)

# get item information from excel sheet (ITEMS)
ITEMS <- items <- read.xlsx("items.xlsx", sheet = "items")
rownames(items) <- items[[1]]


# load the data (DATA)
#DATA <- data <- data.matrix(read.spss("data.sav", to.data.frame = TRUE))
DATA <- data <- read.csv("data.csv", row.names = 1)

# create output dirs
dir.create("output/coefs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/data", recursive = TRUE, showWarnings = FALSE)
dir.create("output/data/formirt", recursive = TRUE, showWarnings = FALSE)
dir.create("output/data/withbackground", recursive = TRUE, showWarnings = FALSE)
dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
dir.create("output/T-conversions", recursive = TRUE, showWarnings = FALSE)
dir.create("output/worksheets", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)


# 2. Order and subset the data ----------------------------------------------------------
# Orders and subsets the data according to the appearance and order of items in the items.xlsx spreadsheet.
# creates list of items per factor
source("steps/2-order-and-subset.R")


# 3. Set 9's to NA --------------------------------------------------------
data[data == 9] <- NA

# 4. Reverse order --------------------------------------------------------
# reverse all but 'negatively' coded items. (Teleform -> CAT)
neg.index <- which(!is.na(items$omschalen))
data[,-neg.index] <- apply(data[,-neg.index], 2, function(x) max(x, na.rm = TRUE) - x + min(x, na.rm = TRUE)) # awkward min/max because of range from 1-5


# 5. Create recoding map ---------
# get the mappings
mappings <- create_mapping(data, count.threshold)

# create table of recodes.
recodes <- unique(mappings)

# create list of items -> recodes
for (i in 1:length(recodes)) {
  temp <- NULL
  for (j in 1:length(mappings)) {
    if (isTRUE(all.equal(recodes[[i]], mappings[[j]]))) items$omcodering[j] <- i     
  }
}

# 6. Recode data ----------------------------------------------------------
# create the recoded object (clone data for ease, will be filled in below loop).
data.recoded <- data
for (item in 1:nrow(items)){
  # paste together mapping for recode()
  mapping <- paste0(mappings[[item]]$old, " = ", mappings[[item]]$new, collapse = "; ")
  
  # do the actual recode
  data.recoded[,item] <- recode(data[,item], mapping, as.numeric.result = TRUE)
}

# 7. Compile and store data -----------------------------------------------
# stores full data, and data per booklet/factor
source("steps/7-compile-and-store-data.R")

# drop dropped questions
data.recoded <- data.recoded[,questions_in_order]
dataByDomain <- lapply( dataByDomain, function (data) data[,setdiff(colnames(data), dropped_questions)] )

# drop fully missing responses
data.recoded <- data.recoded[apply(data.recoded,1,function(x) any(!is.na(x))),]

# the same for by domain, but this is a bit trickier, as here we need to know what domains were non-NA
NotNA.ByDomain <- lapply( dataByDomain, function(data) which(apply(data, 1, function(row) any(!is.na(row)))))
dataByDomain <- lapply( dataByDomain, function(data) data[apply(data,1,function(row) any(!is.na(row))),])

# reattach background variables and store data.
source("steps/7b-compile-and-store-data-with-background-febbles.R")

# 8. Set up mirt model definitions ----------------------------------------
# set up models
model <- create_model(data.recoded, factors)
model.booklet1 <- create_model(booklet1, factors)
# no models needed for single factor models


# 9. Try the full model ---------------------------------------------------
mirt.full <- mirt(data.recoded, model, itemtype = "graded", method = method)

# store it
save(mirt.full, file = "output/models/full.multi.rda")

# obtain coefficients
coefs.full <- coef(mirt.full, simplify = TRUE)

# d -> b
coefs.full$items[,-(1:4)] <- -coefs.full$items[,-(1:4)]
colnames(coefs.full$items) <- c(paste0('a',1:4), paste0('b',1:(ncol(coefs.full$items)-4)))

# store them
write.csv2(coefs.full$items, file = "output/coefs/pars.full.multi.csv")
write.csv2(coefs.full$means, file = "output/coefs/means.full.multi.csv")
write.csv2(coefs.full$cov, file = "output/coefs/cov.full.multi.csv")


# # 10a. Alternate 1; fix booklet 1 ------------------------------------------
# # run mirt on booklet 1, then fix those values in full run.
# source("steps/10a-fix-booklet1.R")


# 10b. Alternate 2; combine unidimensional models. -------------------------
# run mirt per domain, then paste results together
# use correlation of scores as population covariance
source("steps/10b-by-domain.R")


# # 11. Retry full model with starting values -------------------------------
# # run full model with starting values set to results of alternate 1 (fix booklet1)
# source("steps/11-full-with-starting.R")


# 12. Create output from IRTpro calibration -------------------------------
# Create par/cov files from IRTPRO output
irtpro <- convert_from_IRTPRO(IRTPRO_output_file)

# store them
write.csv2(irtpro$items, file = "output/coefs/pars.irtpro.csv")
write.csv2(irtpro$means, file = "output/coefs/means.irtpro.csv")
write.csv2(irtpro$cov, file = "output/coefs/cov.irtpro.csv")

# get model values
values <- mirt(data.recoded, model, itemtype = "graded", method = method, pars = 'values')

# fix values to IRTPRO settings
# first, revert to d.
irtpro$items[,5:8] <- -irtpro$items[,5:8]
colnames(irtpro$items) <- c(paste0('a', 1:4), paste0('d', 1:4))
IRTPRO_fixed <- fix_values(values, irtpro, verbose = TRUE)

# run model (formality, there should be nothing to estimate)
IRTPRO <- mirt(data.recoded, model, itemtype = 'graded', pars = IRTPRO_fixed)

# store it
save(IRTPRO, file = "output/models/irtpro.rda")


# 13. Create theta-T conversion tables and items.xlsx -----------------------
# create conversion tables for each model in models folder, as well as a density plot.
source("steps/13a-theta-T-conversion.R")

# create items.xlsx for each model in models folder (these are base off of THE TRUTH, and therefore themselves A TRUTH)
source("steps/13b-items.xlsx.R")
