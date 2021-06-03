# create new method for is.nan on data.frames
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))