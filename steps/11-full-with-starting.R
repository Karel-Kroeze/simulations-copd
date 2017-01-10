# based on full run after fixing booklet 1.
values.full.starting <- fix_values(values.full, coef(mirt.full.b1fixed, simplify = TRUE), fix = FALSE, mean = FALSE, cov = FALSE)

# run the model with booklet1 parameters fixed
mirt.full.starting <- mirt(data.recoded, model, itemtype = 'graded', method="MHRM", accelerate = 'none', pars = values.full.starting)

# store it
save(mirt.full.starting, file = "output/models/full.multi.starting.rda")

# obtain coefficients
coefs.full.starting <- coef(mirt.full.starting, simplify = TRUE)

# d -> b
coefs.full.starting$items[,-(1:4)] <- -coefs.full.starting$items[,-(1:4)]
colnames(coefs.full.starting$items) <- c(paste0('a',1:4), paste0('b',1:(ncol(coefs.full.starting$items)-4)))

# store them
write.csv2(coefs.full.starting$items, file = "output/coefs/pars.full.multi.starting.csv")
write.csv2(coefs.full.starting$means, file = "output/coefs/means.full.multi.starting.csv")
write.csv2(coefs.full.starting$cov, file = "output/coefs/cov.full.multi.starting.csv")
