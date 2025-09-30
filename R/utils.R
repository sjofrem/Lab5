# normalize vectors to comma-separated strings
to_csv <- function(x) paste(unique(as.character(x)), collapse = ",")
