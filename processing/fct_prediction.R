SUBMISSION_DIR <- "submissions"

write_submission <- function(test, pred, f) {
  submission <- data.frame(
    listing_id = test$listing_id
    , high = pred$high
    , medium = pred$medium
    , low = pred$low)
  
  f_ts <- file.path(SUBMISSION_DIR, paste(f, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep=""))
  
  write.csv(submission, file = f_ts, row.names = FALSE, quote = FALSE)
}