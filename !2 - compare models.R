# required packages
require(ggplot2)
require(reshape2)

# go away factors
SAF <- options(stringsAsFactors = FALSE)

# load all models
pars <- list()
files <- list.files("output/coefs/", "pars", full.names = TRUE)
for (file in files){
  name <- gsub(".*pars\\.(.*)\\.csv", "\\1", file)
  pars[[ name ]] <- cbind( read.csv2( file ), model = name )
}

# combine into single file
pars <- Reduce(rbind, pars)

# name item column
colnames(pars)[1] <- 'item'

# go to long format
molten.pars <- melt(pars, id.vars = c('model', 'item'), variable.name = 'parameter')

# do the plot
ggplot(molten.pars, aes( x = value, group = model, colour = model )) + geom_density() + facet_wrap( ~parameter, scales = 'free' )
ggplot(molten.pars, aes( x = parameter, y = value, colour = model, fill = model )) + geom_point() + facet_wrap( ~item )

### item info plots.
# load all models
models <- list()
files <- list.files("output/models/", full.names = TRUE)
for (file in files){
  name <- load(file)
  models[[ name ]] <- get(name)
}

# TODO: create unidimensional models.
# create a massive object of coordinates
itemplot.data <- data.frame( x = numeric(0), y = numeric(0), model = character(0), item = character(0), category = numeric(0))
for (model in models){
  for (item in colnames(model@Data$data)){
    plot <- itemplot(model, item)
  }
}
