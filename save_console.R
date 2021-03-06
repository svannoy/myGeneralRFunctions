
# Redirect to a file, overwriting, capturing output and error messages
logName <- "WA_DMHP_log.txt"
con <- file(logName)
sink(con, append=FALSE)
sink(con, append=FALSE, type="message")


# Restore output to console
sink() 
sink(type="message")
unlink(con)


# This will reset warnings to NULL
assign("last.warning", NULL, envir = baseenv())