# calibrate per domain (mirt will deal with NA, hopefully ;))
modelByDomain <- lapply(dataByDomain, function(x) {
  mirt(x, 1, itemtype = "graded", method = method)
})

# store em
save(modelByDomain, file = "output/models/bydomain.rda")

# collate and clean parameters
# works under the assumption that the max number of response cats is equal in each domain.
# if this is not true, rbind will fail.
parsByDomain <- lapply(modelByDomain, function(x) coef(x, simplify = TRUE)$items)

pars <- NULL
for(i in 1:length(parsByDomain)){
  par <- parsByDomain[[i]]
  temp <- matrix(0, ncol = ncol(par)+3, nrow = nrow(par))
  rownames(temp) <- rownames(par)
  temp[,i] <- par[,1] # alpha
  temp[,-(1:4)] <- -par[,-1] # d -> beta
  pars <- rbind(pars,temp)
}
colnames(pars) <- c(paste0('a',1:4), paste0('b',1:(ncol(pars)-4)))

# store it
# re-sort it (domains are pasted in order, item )
new_order <- intersect(c(F1, F2, F3, F4), rownames(pars))
pars <- pars[new_order,]
write.csv2(pars, file = "output/coefs/pars.bydomain.csv")

# get scores per domain, position them correctly (length of estimate vectors differs by domain)
N <- 0
lapply(NotNA.ByDomain, function(x) N <<- max(N, max(x)))

scoresByDomain <- list()
for (i in 1:length(NotNA.ByDomain)){
  temp <- rep(NA_real_, N)
  temp[NotNA.ByDomain[[i]]] <- fscores(modelByDomain[[i]], full.scores = TRUE, method = "EAP")
  scoresByDomain[[i]] <- temp
}

# to data.frame
scoresByDomain <- as.data.frame(scoresByDomain)
colnames(scoresByDomain) <- paste0("F", 1:4)

# create covariance matrix (which really is the correlation matrix, since variances would be <1)
# pairwise - each cell in the matrix is computed separately to use the maximum amount of data.
covarianceByDomainPairwise.cor <- matrix(0, ncol(scoresByDomain), ncol(scoresByDomain))
for (i in 1:ncol(scoresByDomain)){
  for (j in 1:i){
    temp <- cor(na.omit(cbind(scoresByDomain[[i]], scoresByDomain[[j]])))[1,2]
    covarianceByDomainPairwise.cor[i,j] <- covarianceByDomainPairwise.cor[j,i] <- temp
  }
}

# store cov and mean
write.csv2(covarianceByDomainPairwise.cor, file = "output/coefs/cov.bydomain.csv")
write.csv2(vapply(modelByDomain, function(x) coef(x, simplify = TRUE)$means, numeric(1)), file = "output/coefs/means.bydomain.csv")

