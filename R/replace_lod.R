# Function to remove < and down arrow from samples below lower limit of detection,
# and > and up arrow from samples above upper limit of detection.
#'
#' @param data Data to replace lods, either a character vector or a data.frame.
#' @param replace.fun Will replace lower limits of detection only, using a 
#' user-specified function. Default is to return the llod value itself.
#' 
replace_lod <- function(data, llod.replace.fun = function(x) x) {
  if (!is.function(llod.replace.fun)) 
    stop("llod.replace.fun must be a function")
  UseMethod("replace_lod")
}
replace_lod.character <- function(chr, llod.replace.fun = function(x) x) {
  llod <- grep('^<.*↓$', chr)
  chr <- sub('(↓|↑)$', '', sub('^(<|>)', '', chr))
  tryCatch(
    {
      num <- as.numeric(chr)
      num[llod] <- llod.replace.fun(num[llod])
      return(num)
    }, error = function(err) {
      cat("llod.replace.fun failed, returning identity\n")
      return(num)
    }, warning = function(wrn) {
      cat("Non-numeric column detected, leaving as-is\n")
      return(chr)
    }
  )
}
replace_lod.data.frame <- function(df, llod.replace.fun = function(x) x) {
  charcol <- map_lgl(df, is.character)
  df[charcol] <- map_df(df[charcol], replace_lod, llod.replace.fun)
  df
}