# booklet numbers
booklets <- DATA[,'Boekje']
nr <- DATA[,'NR']

# write the recoded, reordered data
write.csv2(data.recoded, "output/data/full_data.csv", row.names = FALSE)
write.csv2(cbind(data.recoded, NR = nr), "output/data/full_data_with_NR.csv", row.names = FALSE)

# output data per booklet, only containing items/persons with data.
# check we havent mucked about with rows.
nrow(data) == nrow(data.recoded)

# booklet 1
booklet1 <- data.recoded[which(booklets == 1),]
booklet1 <- booklet1[,which(apply(booklet1, 2, function(x) !all(is.na(x))))]
write.csv2(booklet1, "output/data/booklet1_data.csv", row.names = FALSE)

# booklet 2
booklet2 <- data.recoded[which(booklets == 2),]
booklet2 <- booklet2[,which(apply(booklet2, 2, function(x) !all(is.na(x))))]
write.csv2(booklet2, "output/data/booklet2_data.csv", row.names = FALSE)

# booklet 3
booklet3 <- data.recoded[which(booklets == 3),]
booklet3 <- booklet3[,which(apply(booklet3, 2, function(x) !all(is.na(x))))]
write.csv2(booklet3, "output/data/booklet3_data.csv", row.names = FALSE)

# Factor 1
factor1 <- data.recoded[,F1]
write.csv2(factor1, "output/data/factor1_data.csv", row.names = FALSE)

# Factor 2
factor2 <- data.recoded[,F2]
write.csv2(factor2, "output/data/factor2_data.csv", row.names = FALSE)

# Factor 3
factor3 <- data.recoded[,F3]
write.csv2(factor3, "output/data/factor3_data.csv", row.names = FALSE)

# Factor 4
factor4 <- data.recoded[,F4]
write.csv2(factor4, "output/data/factor4_data.csv", row.names = FALSE)

# create dataset per domain
dataByDomain <- list(F1 = factor1, F2 = factor2, F3 = factor3, F4 = factor4)

# write dataset for MIRT
questions_in_order <- setdiff( c( F1, F2, F3, F4 ), dropped_questions )
data_for_mirt <- data.recoded[,questions_in_order]
data_for_mirt[is.na(data_for_mirt)] <- 9

write.csv2( data_for_mirt, "output/data/formirt/data_ordered_droppedquestions_dutchformat.csv")
write.csv( data_for_mirt, "output/data/formirt/data_ordered_droppedquestions_internationalformat.csv")
