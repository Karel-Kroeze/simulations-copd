# check mismatches
# there should be only demographic stuff and intentionally deleted items here
colnames(data)[!colnames(data) %in% rownames(items)]
# there should be nothing here
rownames(items)[!rownames(items) %in% colnames(data)] 

# problems in coding remain?!
### merge FATIMP6 into FATEXP6
# check if there is overlap - this would be bad
any((data[,"FATIMP6"] != 9) & (data[,"FATEXP6"] != 9))
# no issues, proceed with merging.
# considering missings are 9, we can just take the minimum of both vars (pmin is vectorized min())
data[,"FATEXP6"] <- pmin(data[,"FATIMP6"], data[,"FATEXP6"], na.rm = TRUE)
# FATIMP6 will be deleted when ordering variables, we'll just leave it for now.

### split FATEXP56 into FATEXP46 (Booklet 3) and FATEXP56 (Booklet 2)
# copy 56 into 46
data <- cbind(data, FATEXP46 = data[,"FATEXP56"])
# delete values from 'wrong' booklets
data[which(data[,"Boekje"] == 2), "FATEXP46"] <- 9
data[which(data[,"Boekje"] == 3), "FATEXP56"] <- 9

### split SGRQ42 into a (booklet 1) and b (booklet 3)
data <- cbind(data, SGRQ_42b = data[,"SGRQ_42"])
# delete values from 'wrong' booklets
data[which(data[,"Boekje"] == 1), "SGRQ_42b"] <- 9
data[which(data[,"Boekje"] == 3), "SGRQ_42"] <- 9
# rename 42 into 42a
colnames(data)[colnames(data) == "SGRQ_42"] <- "SGRQ_42a"

## SGQR_42b and SGQR_43 were interchanged.
colnames(data)[colnames(data) == "SGRQ_42b"] <- "temp"
colnames(data)[colnames(data) == "SGRQ_43"] <- "SGRQ_42b"
colnames(data)[colnames(data) == "temp"] <- "SGRQ_43"

# there should be nothing here
if (any(!rownames(items) %in% colnames(data))) stop("Items in excel not in data:", rownames(items)[!rownames(items) %in% colnames(data)])

# orders by the order of appearance in the ITEMS file - deletes stuff not in ITEMS
# intersect of items in DATA and in ITEMS
order <- intersect(rownames(items), colnames(data))

# further drop items that were shown to be not useful
order <- setdiff(order, dropped_questions)

# and order the data - dropping everything not in the intersect between items and data.
data <- data[,order]
# make sure items matches data
items <- items[order,]

# define factors, ^X = starts with X
F1 <- rownames(items)[which(grepl("(^AN)|(^FAT)|(^HI)", rownames(items)))]
F2 <- rownames(items)[which(grepl("(^ML)|(^SGRQ)", rownames(items)))]
F3 <- rownames(items)[which(grepl("(^PF)", rownames(items)))]
F4 <- rownames(items)[which(grepl("(^RP)|(^SRP)", rownames(items)))]

# factors
factors <- list(F1 = F1, F2 = F2, F3 = F3, F4 = F4)

# did we miss any items when setting up factors?
if(any(!items$itemname %in% c(F1, F2, F3, F4))) stop("Items not in factors.")

