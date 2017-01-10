# for each model in output/models, create an excel worksheet which is an updated copy of THE TRUTH.
# prepare recode strings, - 1 because Maarten assumes CAT coding, and these are based on shuffled teleform
recodes <- unique(mappings)
recodes <- sapply(recodes, function(recode){
  paste0(recode$old - 1, " = ", recode$new, "\n", collapse = "")
})

for (model in list.files("output/models")) {
  # model name
  name <- strsplit(model, "\\.")[[1]]
  name <- paste0(name[-length(name)], collapse = ".")
  
  # set up workbook
  wb <- createWorkbook()
  addWorksheet(wb, "items")
  addWorksheet(wb, "antwoordopties")
  addWorksheet(wb, "omcodering")
  addWorksheet(wb, "parameters")
  
  # wrap style
  wrap <- createStyle(wrapText = TRUE)
  
  # write items (is equal to original with the possible exception of 'omcodering')
  temp <- items
  colnames(temp) <- gsub("\\.", " ", colnames(items))
  writeData(wb, "items", temp)
  
  # copy THE TRUTH for answering options
  temp <- read.xlsx("items.xlsx", "antwoordopties")
  colnames(temp)[1] <- "Antwoord ID"
  writeData(wb, "antwoordopties", temp)
  addStyle(wb, "antwoordopties", wrap, cols = 2:4, rows = 1:(nrow(temp) + 1), gridExpand = TRUE)
  
  # write new recoding mappings
  temp <- data.frame(id = 1:length(recodes), mapping = recodes)
  colnames(temp) <- c("mapping id", "mapping")
  writeData(wb, "omcodering", temp)
  addStyle(wb, "omcodering", wrap, cols = 2, rows = 1:(length(recodes) + 1))
  
  # write parameters
  pars <- read.csv2(paste0("output/coefs/pars.", name, ".csv"))
  colnames(pars) <- c('Variable Name', paste0('alpha ', 1:4), paste0('beta ', 1:(length(pars)-5)))
  writeData(wb, "parameters", pars)
  
  # store workbook
  saveWorkbook(wb, paste0("output/worksheets/", name, ".xlsx"), overwrite = TRUE)
}