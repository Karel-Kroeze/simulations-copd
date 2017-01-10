# run model for booklet 1 only
mirt.booklet1 <- mirt(booklet1, model.booklet1, itemtype = 'graded', method = method)

# obtain coefficients
coefs.booklet1 <- coef(mirt.booklet1, simplify = TRUE)

# fetch estimation values of full model
values.full <- mirt(data.recoded, model, pars = 'values', itemtype = 'graded')

# fix booklet 1 parameters in full model
values.full.fixed <- fix_values(values.full, coefs.booklet1)

# run the model with booklet1 parameters fixed
mirt.full.b1fixed <- mirt(data.recoded, model, itemtype = 'graded', method = method, pars = values.full.fixed)

# store it
save(mirt.full.b1fixed, file = "output/models/full.multi.fixed.rda")

# obtain coefficients
coefs.full.b1fixed <- coef(mirt.full.b1fixed, simplify = TRUE)

# d -> b
coefs.full.b1fixed$items[,-(1:4)] <- -coefs.full.b1fixed$items[,-(1:4)]
colnames(coefs.full.b1fixed$items) <- c(paste0('a',1:4), paste0('b',1:(ncol(coefs.full.b1fixed$items)-4)))

# store them
write.csv2(coefs.full.b1fixed$items, file = "output/coefs/pars.full.multi.fixed.csv")
write.csv2(coefs.full.b1fixed$means, file = "output/coefs/means.full.multi.fixed.csv")
write.csv2(coefs.full.b1fixed$cov, file = "output/coefs/cov.full.multi.fixed.csv")
