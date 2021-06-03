# get llod
get_lod <- function(data) {
  UseMethod("get_lod")
}
get_lod.character <- function(v) {
  v <- unlist(v)
  if (!is.atomic(v))
    stop('Input to get_llod must be an atomic vector')
  llod <- v[grep('<', v)]
  ulod <- v[grep('>', v)]
  if (length(unique(llod)) > 1L)
    stop('More than one lower limit of detection per column')
  if (length(unique(ulod)) > 1L)
    stop('More than one upper limit of detection per column')
  data.frame(
    llod = replace_lod(llod[1]), ulod = replace_lod(ulod[1]),
    n_llod = length(llod), n_ulod = length(ulod)
    )
}
get_lod.data.frame <- function(df) {
  charcol <- map_lgl(df, is.character)
  map_dfr(df[charcol], get_lod, .id = "analyte")
}