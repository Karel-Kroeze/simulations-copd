# get list of models
model.files <- list.files("output/models")

# loop through the files
for (file in model.files){
  # load into environment
  name <- load(file = paste0("output/models/", file))
  model <- get(name)
  
  # there may be multiple models, either way collate the theta scores into a N*Q matrix
  if(is.list(model)) {
    theta.scores <- lapply(model, fscores, full.scores = TRUE, method = "MAP")
    
    # sublists can be of differing lengths
    for (q in 1:length(theta.scores)){
      fullscores <- rep(NA, N)
      fullscores[NotNA.ByDomain[[q]]] <- theta.scores[[q]]
      theta.scores[[q]] <- fullscores
    }
    
    theta.scores <- Reduce(cbind, theta.scores)
  } else {
    theta.scores <- fscores(model, full.scores = TRUE, method = "MAP")
  }
  
  # get T scores, maintain NA's that are present in theta scores.
  T.scores <- apply(theta.scores, 2, function(x){
    indeces <- which(!is.na(x))
    x[indeces] <- score.transform(na.omit(x), mu.new = 50, sd.new = 10, normalize = TRUE)$new.scores
    x
  })
  
  ### create lookup tables.
  theta_T <- list()
  raw_theta_T <- list()
  for (i in 1:ncol(theta.scores)){
    raw <- cbind(theta.scores[,i], round(T.scores[,i]))
    raw <- raw[order(theta.scores[,i]),]
    
    # initiate table
    table <- NULL
    
    ### simplify lookup tables (we only need min/max for T-scores with 2+ scores.)
    for (score in unique(raw[,2])){
      # individual T scores
      minitable <- raw[which(raw[,2] == score),,drop=FALSE]
      
      # 2+ translations
      if (nrow(minitable) > 2){
        # save only min max
        minitable <- minitable[c(which.min(minitable[1,]), which.max(minitable[1,])),]
      }
      
      # collect minitables
      table <- rbind(table, minitable)
    }
    
    # bind in list
    theta_T[[i]] <- table
    raw_theta_T[[i]] <- raw
  }
  
  # set minima to -Inf
  theta_T <- lapply(theta_T, function(x) {
    rbind(x[1,] - c(.01, Inf), x)
  })
  
  # set maxima to +Inf, if not already set
  theta_T <- lapply(theta_T, function(x) {
    if(sum(is.infinite(x[,2])) > 1) {
      return(x)
    } else {
      return(rbind(x, c(max(x[,1]) + .1, Inf)))
    }
  })
  
  # store
  save(theta_T, file = paste0("output/T-conversions/", file))
  
  
  # create a distribution plot (for the shine!)
  plotdata <- data.frame(theta.scores)
  colnames(plotdata) <- c("Vermoeidheid", "Impact van COPD op KvL", "Lichamelijk functioneren", "Sociale rollen en activiteiten")
  plotdata <- melt(plotdata)
  
  temp <- ggplot(plotdata, aes(x = value)) + 
    geom_density(fill = "grey", colour = "steelblue", lwd = 1) + 
    coord_cartesian(xlim = c(-6,6)) +
    facet_wrap( ~ variable) + 
    theme_bw() + labs(x = "Score (z-value)")
  
  png(filename = paste0("output/plots/density.", substr(file, 0, nchar(file) - 4), ".png"), width = 14, height = 7, units = "in", res = 150)
  print(temp)
  dev.off()
  
  svg(filename = paste0("output/plots/density.", substr(file, 0, nchar(file) - 4), ".svg"), width = 14, height = 7)
  print(temp)
  dev.off()
}



# # some analysis
# # distributions
# # theta, BEFORE culling the lookup table
# par(mfrow = c(2,2))
# lapply(raw_theta_T, function(x) plot(density(x[,1])))
# # T, BEFORE culling
# lapply(raw_theta_T, function(x) plot(density(x[,2])))
# # theta, after (note that there were a lot of duplicates around the mean - these were culled, so the distribution has a gap near the mean)
# lapply(theta_T, function(x) plot(density(x[,1])))
# # T, after (culled in middle, so a weird flat normal)
# lapply(theta_T, function(x) plot(density(x[,2])))
# 
# # maximum 'gaps'
# lapply(theta_T, function(x) {
#   cat("++++++++++++++++++++\n")
#   last <- round(x[1,], 2)
#   gaps <- matrix(0, nrow = nrow(x)-1, ncol = 2)
#   for (i in 2:nrow(x)){
#     current <- round(x[i,], 2)
#     # more detailed output, turn on on demand
#     # cat(last[2], "to", current[2], ":", current[2] - last[2], "(theta", last[1], "to", current[1], ":", current[1] - last[1], ")\n")
#     gaps[i-1,] <- current - last
#     last <- current
#   }
#   gaps[!is.finite(gaps)] <- NA
#   print(plot(gaps[,1]))
#   cat("maximum gaps: theta", max(gaps[,1], na.rm = TRUE), ", T", max(gaps[,2], na.rm = TRUE),"\n")
# })

