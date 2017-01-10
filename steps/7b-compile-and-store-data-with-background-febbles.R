
data.background <- merge(data,DATA[background_variables],by="row.names",all.x=TRUE)[,-1]

write.csv2(data.background, "output/data/withbackground/international.csv", row.names = FALSE)
write.csv(data.background, "output/data/withbackground/dutch.csv", row.names = FALSE)